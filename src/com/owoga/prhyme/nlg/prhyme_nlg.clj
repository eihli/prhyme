(ns com.owoga.prhyme.nlg.prhyme-nlg
  (:require [clojure.zip :as zip]
            [clojure.string :as string]
            [taoensso.timbre :as timbre]
            [examples.core :as examples]
            [taoensso.nippy :as nippy]
            [com.owoga.prhyme.nlp.core :as nlp]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [clojure.set :as set]))

(defn update-values [m f & args]
  (reduce
   (fn [acc [k v]]
     (assoc acc k (apply f v args)))
   {}
   m))

(defn generate
  [pos-path->word-freqs
   pos->word-freqs
   target-parse-tree]
  (loop [parse-zipper (zip/seq-zip target-parse-tree)]
    (cond
      (zip/end? parse-zipper) (zip/root parse-zipper)

      (zip/branch? parse-zipper)
      (recur (zip/next parse-zipper))

      (string? (zip/node parse-zipper))
      (recur (zip/next parse-zipper))

      (and (symbol? (zip/node parse-zipper))
           (or (pos->word-freqs (zip/node parse-zipper))
               (pos-path->word-freqs (seq (map first (zip/path parse-zipper))))))
      (let [target-path (seq (map first (zip/path parse-zipper)))
            target-pos (zip/node parse-zipper)
            pos-path-word (pos-path->word-freqs target-path)
            pos-word (pos->word-freqs target-pos)]
        (timbre/info "Choosing POS for" target-path)
        (let [selection (weighted-rand/weighted-selection-from-map
                         (merge-with +
                          (update-values pos-path-word * 2)
                          pos-word))]
          (timbre/info "from" (take 5
                                    (merge-with +
                                     (update-values pos-path-word * 2)
                                     pos-word)))
          (timbre/info "Chose " selection)
          (recur
           (-> parse-zipper
               zip/up
               (#(zip/replace % (list (zip/node (zip/down %)) selection)))
               zip/next
               zip/next))))

      :else
      (recur (zip/next parse-zipper)))))

(defn next-word [zipper]
  (->> zipper
      nlp/iter-zip
      (filter #(string? (zip/node %)))
      first
      (#(if (nil? %) :end (zip/node %)))))

(defn next-two-words [nodes]
  (timbre/info
   (apply list (->> nodes
                    (map zip/node))))
  (->> nodes
       (filter #(string? (zip/node %)))
       (take 2)
       (map #(if (nil? %) :end (zip/node %)))))

(comment
  (let [zipper (zip/seq-zip '(TOP (S (NN "Eric") (VBZ "is") (JJ "testing"))))]
    (->> zipper
         nlp/iter-zip
         reverse
         next-two-words))

  )

(defn set-last [zipper f]
  (let [last-node (->> zipper
                       (iterate zip/next)
                       (take-while (complement zip/end?))
                       last
                       zip/prev)]
    (-> last-node
        (zip/replace (f last-node)))))

(comment
  (let [zipper (zip/seq-zip '(TOP (S (NN) (VBZ "is") (JJ))))]
    (-> zipper
        (set-last (fn [node] (list (zip/node (zip/next node)) "bad")))
        zip/root))

  )

(defn generate-with-markov
  [pos-path->word-freqs
   pos->word-freqs
   target-parse-tree
   markov]
  (loop [parse-zipper (zip/seq-zip target-parse-tree)]
    (cond
      (zip/end? parse-zipper) (zip/root parse-zipper)

      (zip/branch? parse-zipper)
      (recur (zip/next parse-zipper))

      (string? (zip/node parse-zipper))
      (recur (zip/next parse-zipper))

      (and (symbol? (zip/node parse-zipper))
           (or (pos->word-freqs (zip/node parse-zipper))
               (pos-path->word-freqs (seq (map first (zip/path parse-zipper))))))
      (let [target-path (seq (map first (zip/path parse-zipper)))
            target-pos (zip/node parse-zipper)
            pos-path-word (pos-path->word-freqs target-path)
            pos-word (pos->word-freqs target-pos)
            markov-options (markov (reverse (next-two-words (nlp/iter-zip
                                                             parse-zipper
                                                             zip/prev
                                                             nil?))))]
        (timbre/info "Markov options are"
                     (apply list (next-two-words (nlp/iter-zip
                                                  parse-zipper
                                                  zip/prev
                                                  nil?)))
                     (apply list (take 3 markov-options)))
        (timbre/info "Choosing POS for" target-path)
        (let [selection (weighted-rand/weighted-selection-from-map
                         (merge-with
                          *
                          (update-values markov-options * 10)
                          (update-values pos-path-word * 2)
                          pos-word))]
          (timbre/info "from" (apply
                               list
                               (take
                                5
                                (merge-with
                                 *
                                 (update-values markov-options * 10)
                                 (update-values pos-path-word * 2)
                                 pos-word))))
          (timbre/info "Chose " selection)
          (recur
           (-> parse-zipper
               zip/up
               (#(zip/replace % (list (zip/node (zip/down %)) selection)))
               zip/next
               zip/next))))

      :else
      (recur (zip/next parse-zipper)))))

(defn generate-with-markov-with-custom-progression
  "Sams as above, but with next/prev and stop fns"
  [next
   prev
   next-stop?
   prev-stop?
   pos-path->word-freqs
   pos->word-freqs
   parse-zipper
   markov]
  (loop [parse-zipper parse-zipper]
    (cond
      (nil? (next parse-zipper)) (zip/root parse-zipper)
     
      (next-stop? parse-zipper) (zip/root parse-zipper)

      (zip/branch? parse-zipper)
      (recur (next parse-zipper))

      (string? (zip/node parse-zipper))
      (recur (next parse-zipper))

      (and (symbol? (zip/node parse-zipper))
           (or (pos->word-freqs (zip/node parse-zipper))
               (pos-path->word-freqs (seq (map first (zip/path parse-zipper))))))
      (let [target-path (seq (map first (zip/path parse-zipper)))
            target-pos (zip/node parse-zipper)
            pos-path-word (pos-path->word-freqs target-path)
            pos-word (pos->word-freqs target-pos)
            pos-map (merge-with
                     (fn [a b] (* 1.5 (+ a b)))
                     pos-path-word
                     pos-word)
            markov-options (markov (reverse
                                    (next-two-words (nlp/iter-zip
                                                     parse-zipper
                                                     prev
                                                     prev-stop?))))
            selection-possibilities (merge-with
                                     (fn [a b]
                                       (let [max-pos (apply max (vals pos-map))]
                                         (+ a b max-pos)))
                                     pos-map
                                     markov-options)]
        (timbre/info "Markov options are"
                     (apply list (next-two-words (nlp/iter-zip
                                                  parse-zipper
                                                  prev
                                                  prev-stop?)))
                     (apply list (take 10 markov-options)))
        (timbre/info "Choosing POS for" target-path)
        (let [selection (weighted-rand/weighted-selection-from-map
                         selection-possibilities)]
          (timbre/info
           "Most likely selection possibilities"
           (apply list (take 5 (reverse (sort-by second selection-possibilities)))))
          (timbre/info "Chose " selection)
          (recur
           (-> parse-zipper
               zip/up
               (#(zip/replace % (list (zip/node (zip/down %)) selection)))
               zip/down
               next
               next))))

      :else
      (recur (next parse-zipper)))))

(comment
  (let [structure '(TOP (S (NP (DT) (JJ) (NN))
                           (VP (VBZ))
                           (NP (DT) (JJ) (NN))))
        structure (-> structure
                      zip/seq-zip
                      nlp/iter-zip
                      last)
        pos-freqs (examples/pos-paths->pos-freqs
                   examples/t1)]
    (repeatedly
     10
     (fn []
       (->> (generate-with-markov-with-custom-progression
             zip/prev
             zip/next
             nil?
             zip/end?
             examples/t1
             pos-freqs
             structure
             examples/darkov-2)
            nlp/leaf-nodes
            (string/join " ")))))

  (timbre/set-level! :info)
  (timbre/set-level! :error)

  (let [pos-path->word-freqs
        {'(S N) {"Eric" 1 "Edgar" 2}
         '(S V) {"tests" 2 "runs" 1}}
        pos->word-freqs
        {'N {"Edward" 1}
         'V {"breaks" 1}}
        target-parse-tree
        '(S (N) (V))]
    (-> (generate
         pos-path->word-freqs
         pos->word-freqs
         target-parse-tree)))
  (time (def example-pos-freqs examples/example-pos-freqs))
  (nippy/thaw)
  (nippy/freeze-to-file "resources/1000-pos-path-freqs.nip" example-pos-freqs)

  (time (def example-structures examples/example-structures))
  (weighted-rand/weighted-selection-from-map
   example-structures)



  (take 5 examples/t2)
  (let [structure '(TOP (S (NP (DT) (JJ) (NN))
                           (VP (VBZ))
                           (NP (DT) (JJ) (NN))))
        structure (-> structure
                      zip/seq-zip
                      nlp/iter-zip
                      last)
        pos-freqs (examples/pos-paths->pos-freqs
                   examples/t1)]
    (repeatedly
     10
     (fn []
       (->> (generate-with-markov-with-custom-progression
             zip/prev
             zip/next
             nil?
             zip/end?
             examples/t1
             pos-freqs
             structure
             examples/darkov-2)
            nlp/leaf-nodes
            (string/join " ")))))

  (repeatedly
   10
   (fn []
     (let [structure (weighted-rand/weighted-selection-from-map
                      (->> examples/t2
                           (sort-by second)
                           (reverse)
                           (take 20)))
           structure (-> structure
                         zip/seq-zip
                         nlp/iter-zip
                         last)
           pos-freqs (examples/pos-paths->pos-freqs
                      examples/t1)]
       (repeatedly
        10
        (fn []
          (->> (generate-with-markov-with-custom-progression
                zip/prev
                zip/next
                nil?
                zip/end?
                examples/t1
                pos-freqs
                structure
                examples/darkov-2)
               nlp/leaf-nodes
               (string/join " ")))))))

  )


;;; Most common grammars
(comment
  '([(TOP (NN)) 857]
    [(TOP (NP (NN) (NN))) 569]
    [(TOP (NP (JJ) (NN))) 563]
    [(TOP (NP (NP (NN)) (PP (IN) (NP (NN))))) 424]
    [(TOP (PP (IN) (NP (DT) (NN)))) 390]
    [(TOP (NP (NP (NN)) (PP (IN) (NP (DT) (NN))))) 314]
    [(TOP (NP (DT) (NN))) 300]
    [(TOP (NP (DT) (JJ) (NN))) 265]
    [(TOP (NP (NP (DT) (NN)) (PP (IN) (NP (NN))))) 250]
    [(TOP (VP (VB) (NP (DT) (NN)))) 221]
    [(TOP (NP (NP (NN)) (PP (IN) (NP (PRP$) (NN))))) 218]
    [(TOP (NP (JJ) (NNS))) 211]
    [(TOP (VB)) 204]))
