(ns com.owoga.prhyme.generation.markov-example
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combinatorics]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.syllabify :as syllabify]
            [cljol.dig9 :as d]
            [clojure.zip :as zip]
            [com.owoga.tightly-packed-trie.bit-manip :as bm]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.tightly-packed-trie :as tpt]
            [taoensso.nippy :as nippy]
            [com.owoga.prhyme.nlp.tag-sets.treebank-ii :as tb2]
            [com.owoga.prhyme.nlp.core :as nlp]))

(def corpus (slurp (io/resource "cask_of_amontillado.txt")))

;; For better generation of text, you'll probably want to pad the starts
;; of sentences with n-1 "start-of-sentence" tokens.
(defn prep-punctuation-for-tokenization
  "Puts spaces around punctuation so that they aren't
  tokenized with the words they are attached to.

  Might add extraneous whitespace, but presumedly that will be ignored/removed
  during tokenization."
  [text]
  (string/replace text #"([\.,!?])" " $1 "))

(defn remove-quotes
  "...and hyphens"
  [text]
  (string/replace text #"[\"-]" ""))

(defn remove-formatting-characters
  "Input has underscores, presumably because the text
  might be rendered by something that can italicize or bold text.
  We'll just ignore them for now."
  [text]
  (string/replace text #"[_*]" ""))

(defn tokenize [text]
  (-> text
      remove-formatting-characters
      prep-punctuation-for-tokenization
      remove-quotes
      string/lower-case
      (string/split #"[\n ]+")))

(defn interleave-all
  "Like interleave, but instead of ending the interleave when the shortest collection
  has been consumed, continues to interleave the remaining collections."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2)
                               (interleave-all (rest s1) (rest s2))))
        (lazy-seq (or s1 s2))))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (->> (map seq (conj colls c2 c1))
                  (remove nil?))]
      (when ss
        (concat (map first ss) (apply interleave-all (map rest ss))))))))

(comment
  (let [tokens [1 2 3 4 5]
        p1 (partition 1 1 tokens)
        p2 (partition 2 1 tokens)
        p3 (partition 3 1 tokens)]
    (interleave-all p1 p2 p3)))

(defn ngramify-tokens [n m tokens]
  (let [partition-colls (map #(partition % 1 tokens) (range n m))
        ngrams (apply interleave-all partition-colls)]
    ngrams))

(comment
  (->> (tokenize corpus)
       (take 5)
       (ngramify-tokens 1 4))
  ;; => (("the")
  ;;     ("the" "thousand")
  ;;     ("the" "thousand" "injuries")
  ;;     ("thousand")
  ;;     ("thousand" "injuries")
  ;;     ("thousand" "injuries" "of")
  ;;     ("injuries")
  ;;     ("injuries" "of")
  ;;     ("injuries" "of" "fortunato")
  ;;     ("of")
  ;;     ("of" "fortunato")
  ;;     ("fortunato"))
  )



(defn add-terminal-value-to-ngram
  "The Trie expects entries to be of the form '(k1 k2 k3 value).
  The ngrams generated above are just '(k1 k2 k3).
  This adds a value that is simply the ngram itself:
  '(k1 k2 k3 '(k1 k2 k3))."
  [ngram]
  (concat ngram (list ngram)))


(defn trie->frequency-of-frequencies-map
  "The second argument to this function specifies which rank you
  want to get the map for."
  [trie n]
  (->> trie
       (trie/children-at-depth n)
       (map (comp :count second first seq))
       frequencies
       (into (sorted-map))))

(comment
  (trie->frequency-of-frequencies-map trie 1)
  ;; => {1 558,
  ;;     2 110,
  ;;     ,,,
  ;;     167 1,
  ;;     177 1}

  )

;; The frequency of a thus-far unseen species is the number of species seen once over the
;; total number of species.
;; That's commonly referred to as P0
;; There will be a different P0 for each rank of N-gram.

(defn P0 [trie n]
  (let [freq-map (trie->frequency-of-frequencies-map trie n)]
    (/ (freq-map 1) (apply + (vals freq-map)))))

(comment
  (P0 trie 1)
  ;; => 31/45
  )

;; From here on out, we follow a similar procedure.
;; What we just did, P0, is the probability of seeing something
;; that has been previously unseen.
;; We found that by using what we know about P1 (how many times
;; things have been seen once).
;;
;; Now, we need to adjust our P1 number since we just gave some probability
;; to P0, which previously had no probability since it wasn't in our
;; frequency table.
;;
;; What's the new probability that the next thing we see is from the group of
;; n-grams that we've seen once?
;;
;; The same way P0 was based off P1, P1 will be based off P2.
;;
;; It's basically 2 * the number of times we've seen things twice divided
;; by the total number of things we've seen.
;;
;; P0 was 1 * number of 1-time things / total number of n-time things.
;; P1 is 2 * number of 2-time things / total number of n-time things.
;; P2 is 3 * number of 3-time things / total number of n-time things.
;;
;; With a slight adjustment. The frequency of frequencies needs to be smoothed
;; so there are no 0-values. When you get up to P14, P15, etc... there might be gaps
;; where you'll see P14 1-time, then won't see anything 15 or 16 times, so P15 and P16 will
;; be 0, then you'll see something 17 times twice.
;;
;; This is just noise from having limited data. The noise needs to be smoothed out.

(defn simple-good-turing-map [trie n]
  (let [freq-map (trie->frequency-of-frequencies-map trie n)
        xs (->> freq-map keys (map #(Math/log %)))
        ys (->> freq-map vals (map #(Math/log %)))
        sgt (math/sgt (keys freq-map) (vals freq-map))
        sgt-map (into (sorted-map) (apply map vector sgt))]
    sgt-map))


(comment
  (let [freq-map (trie->frequency-of-frequencies-map trie 2)
        xs (->> freq-map keys (map #(Math/log %)))
        ys (->> freq-map vals (map #(Math/log %)))
        sgt (math/sgt (keys freq-map) (vals freq-map))
        sgt-map (into (sorted-map) (apply map vector sgt))
        sgt-with-counts (math/sgt-with-counts (keys freq-map)
                                                        (vals freq-map))
        c1 (freq-map 1)
        c1* (sgt-map 1)]
    [c1 c1* sgt-with-counts])

  )


;; Maximum Likelihood Estimate
;;
;; It was about dusk, one evening during the supreme madness of the
;; carnival season, that I encountered my friend.  He accosted me with
;; excessive warmth, for he had been drinking much.  The man wore motley.
;; He had on a tight-fitting parti-striped dress, and his head was
;; surmounted by the conical cap and bells.  I was so pleased to see him,
;; that I thought I should never have done wringing his hand.
;;
;; Consider 3-grams...
;;
;; it was about
;; it was there
;;
;; Let `N` be a sample text size and `nr` be the number of
;; m-grams which occurred in the text exactly `r` times.
;;
;; So that `N` = (apply + (map #(* r nr) frequency-of-frequencies)
;; `N` = sum for all seen-counts ("number of things seen 'count' times" * 'count')
;; 10 things seen 5 times
;; 4 things seen 4 times
;; 2 things seen 1 time
;;
;; 10 things seen 5 times each makes up 50 "things"
;; 4 things seen 4 times each makes up 16 "things
;; 2 things seen once each makes up 2 "things"
;;
;; Makes for `N` = 50 + 16 + 2 things... 68 things (m-grams).
;;
;; Consider the m-gram "it was about" occurred 4 times.
;; And in total we saw 60 3-grams. Then the MLE
;; is 4 / 60.
;;
;;
;;;; Base MLE
;;
;; Disount of the n-gram
;; *
;; Count of n-gram
;; /
;; Count of n-1-gram


(defn maximum-likelihood-estimate [trie trie-database n-gram]
  (/ (get-in trie-database [n-gram :count])
     (get-in trie-database [(butlast n-gram) :count])))


(comment
  (maximum-likelihood-estimate trie trie-database '("," "the"))

  (maximum-likelihood-estimate trie trie-database '(","))

  (let [[rs nrs ests lgts]
        (apply
         math/sgt-with-counts
         (apply map vector (seq (trie->frequency-of-frequencies-map trie 2))))]
    [rs nrs ests lgts])

  )

;;;; KATZ ;;;;

;; (defn N [trie n-gram-rank]
;;   (let [r->Nr (trie->frequency-of-frequencies-map trie n-gram-rank)]
;;     (apply + (map (fn [[r nr]] (* r nr)) r->Nr))))

;; (defn r* [trie n-gram-rank]
;;   (let [r->Nr (trie->frequency-of-frequencies-map trie n-gram-rank)
;;         _ _ _ r*s]))


;;;;

(defn zipper-leaf-path-seq
  [zipper]
  (->> zipper
       (iterate zip/next)
       (take-while (complement zip/end?))
       (filter (complement zip/branch?))
       (map zip/path)
       (map (partial map first))
       (filter (comp tb2/words last))))

(comment
  (def target-grammar-structure
    '(TOP (S (NP (WDT)) (VP (VBD) (NP (DT) (NN))))))

  (reverse (zipper-leaf-path-seq (zip/seq-zip target-grammar-structure)))

  (defn decode-fn
    "Decodes a variable-length encoded number from a byte-buffer.
  Zero gets decoded to nil."
    [byte-buffer]
    (let [value (encoding/decode byte-buffer)]
      (if (zero? value)
        nil
        value)))

  (def tpt (tpt/load-tightly-packed-trie-from-file
            (io/resource "dark-corpus-4-gram-backwards-tpt.bin")
            decode-fn))

  (def database (nippy/thaw-from-file (io/resource "dark-corpus-4-gram-backwards-db.bin")))

  (def example-story
    (loop [generated-text (vec (repeat 3 (get database "</s>")))
           i              0]
      (if (> i 20)
        generated-text
        (let [children (loop [i 4]
                         (let [node
                               (trie/lookup
                                tpt
                                (vec (take-last i generated-text)))
                               children
                               (and node (trie/children node))]
                           (cond
                             (nil? node)    (recur (dec i))
                             (< i 0)        (throw (Exception. "Error"))
                             (seq children) children
                             :else          (recur (dec i)))))]
          (recur
           (conj
            generated-text
            (->> children
                 (map #(vector (.key %) (get % [])))
                 (remove (comp nil? second))
                 (#(math/weighted-selection
                    (fn [[_ c]] c)
                    %))
                 first))
           (inc i))))))

  (map database example-story)

  )

(defn syllabify-phrase
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map phonetics/get-phones)
       (map first)
       (map syllabify/syllabify)
       (reduce into [])))

(defn markov-choice
  [trie generated-text k xf-filter]
  (let [node     (trie/lookup trie k)
        children (and node
                      (->> node
                           trie/children
                           (map #(vector (.key %) (get % [])))
                           (remove (comp nil? second))))
        choices (transduce
                 (comp
                  (map (fn [child]
                         (vector generated-text child)))
                  xf-filter)
                 conj
                 children)]
    (cond
      (nil? node) (recur trie generated-text (butlast k) xf-filter)

      (seq children)
      (if (< (rand) (/ (apply max (map second children))
                       (apply + (map second children))))
        (recur trie generated-text (butlast k) xf-filter)
        (first
         (math/weighted-selection
          (fn [[_ c]] c)
          choices)))

      (> (count k) 0) (recur trie generated-text (butlast k) xf-filter)

      :else (throw (Exception. "Error")))))

(defn syllable-count-pred
  [syllable-count database]
  (fn [node]
    (let [syllables (syllabify-phrase (database (get node [])))]
      (= syllable-count
         (count syllables)))))

(defn markov-select
  [n]
  (fn [{:keys [trie database xf-filter tokens] :as context}]
    (loop [n n]
      (if (= n 0)
        ;; Unable to find a selection
        nil
        (let [key (take-last n tokens)
              node (trie/lookup trie key)
              children (and node (->> (trie/children node)
                                      (remove nil?)))
              choices (transduce
                       (comp
                        (map #(vector (.key %) (get % [])))
                        (map (fn [child] [context child]))
                        xf-filter)
                       conj
                       children)]
          (let [freqs (map #(get % []) children)]
            (cond
              (or (empty? choices) (empty? freqs))
              (recur (dec n))

              (and
               (> n 1)
               (< (rand)
                  (/ (apply max freqs)
                     (apply + freqs))))
              (recur
               (dec n))

              :else
              (let [result (second (math/weighted-selection
                                    (comp second second)
                                    choices))]
                (first result)))))))))

(defn generate-sentence
  [{:keys [trie database stop? xf-filter tokens] :as context}]
  (let [markov-fn (markov-select 4)]
    (loop [context (assoc context :i 0)]
      (let [tokens (:tokens context)]
        (cond
          (stop? context)
          tokens

          :else
          (let [selection (markov-fn context)]
            (if (nil? selection)
              (do
                (println tokens)
                (throw (Exception. "No possible selection")))
              (recur (update
                      context
                      :tokens
                      conj
                      selection)))))))))

(comment
  (trie/lookup tpt '(1 1 1))
  (let [context {:tokens (vec (repeat 3 (database "</s>")))
                 :trie tpt
                 :database database
                 :stop? (fn [{:keys [tokens] :as context}]
                          (let [sentence (->> tokens
                                              (map database)
                                              (remove #{"</s>" ","})
                                              (string/join " "))]
                            (<= 10 (count (syllabify-phrase sentence)))))
                 :xf-filter (comp
                             (remove
                              (fn [[context [k v]]]
                                (= k 7)))
                             (filter
                              (fn [[context [k v]]]
                                (let [current-sentence
                                      (->> (:tokens context)
                                           (map database)
                                           (remove #{"</s>"})
                                           (string/join " "))
                                      current-syllable-count
                                      (count (syllabify-phrase current-sentence))
                                      current-word (database k)
                                      current-word-syllable-count (count (syllabify-phrase current-word))]
                                  (>= (- 10 current-syllable-count)
                                      current-word-syllable-count)))))}]
    (->> (generate-sentence context)
         (map database)))

  (let [context {:tokens (mapv database ["</s>" "is" "it" "how"])
                 :trie tpt
                 :database database
                 :stop? (fn [{:keys [tokens] :as context}]
                          (let [sentence (->> tokens
                                              (map database)
                                              (remove #{"</s>" ","})
                                              (string/join " "))]
                            (<= 10 (count (syllabify-phrase sentence)))))
                 :xf-filter (comp
                             (remove
                              (fn [[context [k v]]]
                                (= k 7)))
                             (filter
                              (fn [[context [k v]]]
                                (let [current-sentence
                                      (->> (:tokens context)
                                           (map database)
                                           (remove #{"</s>"})
                                           (string/join " "))
                                      current-syllable-count
                                      (count (syllabify-phrase current-sentence))
                                      current-word (database k)
                                      current-word-syllable-count (count (syllabify-phrase current-word))]
                                  (>= (- 10 current-syllable-count)
                                      current-word-syllable-count))))
                             (filter
                              (fn [[context [k v]]]
                                (let [current-rhyme
                                      (->> (:token context)
                                           (take-last 3)
                                           (map (comp firstphonetics/get-phones)))]))))}]
    (->> (generate-sentence context)
         (map database)))

  (->> (map (comp first phonetics/get-phones) ["is" "it" "how"])
       (reduce into [])
       )

  (database "<s>")
  )
#_(defn generate-sentence
  [trie database stop? filters]
  (loop [generated-text (vec (repeat 3 (get database "</s>")))
         i              0]
    (cond
      (> i 400)
      nil

      (stop? generated-text)
      generated-text

      ;; reset
      (or (zero? (mod i 40))
          (> syllable-count target-syllable-count))
      (recur (vec (repeat 3 (get database "</s>"))) 0)

      :else
      (let [choice (markov-choice
                    trie
                    (take-last 4 generated-text)
                    filters)]
        (recur
         (conj
          generated-text
          choice)
         (inc i))))))

(comment
  (let [disallow-sentence-start-xf
        (remove (= (database (first %)) "<s>") children)
        ])

  (map database (generate-sentence tpt database 10))

  (def grammar-freqs (nippy/thaw-from-file "/tmp/grammar-freqs-top-100.bin"))
  (def part-of-speech-freqs (nippy/thaw-from-file "/tmp/part-of-speech-freqs.bin"))
  (take 100 part-of-speech-freqs)

  (loop [generated-text (vec (repeat 3 (get database "</s>")))
         i              0]
    (let [current-sentence
          (take-while
           (complement (into #{} (map database ["<s>" "</s>"])))
           (reverse generated-text))]
      (if (> i 20)
        generated-text
        (let [children (loop [i 4]
                         (let [node
                               (trie/lookup
                                tpt
                                (vec (take-last i generated-text)))
                               children
                               (and node (trie/children node))]
                           (cond
                             (nil? node)    (recur (dec i))
                             (< i 0)        (throw (Exception. "Error"))
                             (seq children) children
                             :else          (recur (dec i)))))]
          (recur
           (conj
            generated-text
            (->> children
                 (map #(vector (.key %) (get % [])))
                 (remove (comp nil? second))
                 (#(math/weighted-selection
                    (fn [[_ c]] c)
                    %))
                 first))
           (inc i))))))

  )


(comment
  (def rhyme-trie (atom (trie/make-trie)))

  ;; Turning a word frequency into a phoneme trie
  (transduce
   (comp
    (drop 100000)
    (take 20)
    (map first)
    (map (partial remove #{1 7})) ;; </s> and <s>
    (remove empty?)
    (map (juxt identity (partial map database)))
    (map (juxt first (comp (partial mapv phonetics/get-phones) second)))
    (map reverse)
    (map
     (fn [[words keys]]
       (run!
        (fn [[phrase keys]]
          (swap! rhyme-trie assoc (reduce into [] phrase) keys))
        (map vector
             (apply combinatorics/cartesian-product words)
             (repeat keys)))
       (map
        (fn [[pronunciations key]]
          (run!
           (fn [phonemes]
             (swap! rhyme-trie assoc phonemes key))
           pronunciations)
          [pronunciations key])
        (map vector words keys))
       [words keys])))
   conj
   (trie/children-at-depth tpt 0 2))

  (take 20 (drop 100 @rhyme-trie))

  (take 20 (trie/children-at-depth tpt 0 2))

  (let [words [[[["DH" "IH1" "S"] ["DH" "IH0" "S"]] [["IH1" "Z"] ["IH1" "S"]]] '(11 77)]]
    (map
     (fn [[phrase keys]]
       (reduce into [] phrase))
     (map vector
          (apply combinatorics/cartesian-product (first words))
          (repeat (second words)))))

  (map vector [1 2 3] [4 5 6])
  )
