(ns com.owoga.prhyme.gen
  (:require [clojure.string :as string]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [com.owoga.prhyme.util.nlp :as nlp]
            [com.owoga.corpus.darklyrics :as dr]
            [com.owoga.prhyme.frp :as frp]
            [com.owoga.prhyme.core :as prhyme]))

(def words-map
  (into {} (map #(vector (string/lower-case (:word %)) %) frp/words)))

(defn merge-phrase-words
  "Given multiple `Word`, like the words for 'well off', create a single `Word`
  that is syllabified as ('well' 'off') rather than as the combined ('weh'
  'loff'). Useful for finding single-word rhymes of multiple-word targets.

  An example: 'war on crime' -> 'turpentine'.
  As opposed to: 'war on crime' -> 'caw fawn lime'."
  [phrase phrase-words]
  (loop [merged (first phrase-words)
         phrase-words (rest phrase-words)]
    (cond
      (and (empty? phrase-words) (empty? merged)) nil
      (empty? phrase-words) (assoc merged :word phrase)
      :else (recur (-> merged
                       (assoc :syllables (concat (:syllables merged)
                                                 (:syllables (first phrase-words))))
                       (assoc :syllable-count (+ (:syllable-count merged)
                                                 (:syllable-count (first phrase-words))))
                       (assoc :rimes (concat (:rimes merged)
                                             (:rimes (first phrase-words))))
                       (assoc :onsets (concat (:onsets merged)
                                              (:onsets (first phrase-words))))
                       (assoc :nuclei (concat (:nuclei merged)
                                              (:nuclei (first phrase-words)))))
                   (rest phrase-words)))))

(defn phrase->word
  "Given a word like 'well-off' or a phrase like 'war on poverty', return a Word
  that has the correct syllables, rimes, onsets, and nucleus. This way we can
  rhyme against phrases that aren't in the dictionary, as long as the words that
  make up the phrase are in the dictionary. Returns nil if the word is not in
  the dictionary."
  [words phrase]
  (->> (string/split phrase #"[ -]")
       (map (fn [phrase-word]
              (let [word (first (filter (fn [word]
                                          (= phrase-word (:norm-word word)))
                                        words))]
                (if (nil? word)
                  (frp/make-word (cons phrase-word (util/get-phones phrase-word)))
                  word))))
       (merge-phrase-words phrase)))

(defn adjust-for-markov
  [markov percent]
  (let [target-markov-n (count (first (first markov)))]
    (fn [[words target result]]
      (if (>= (count result) target-markov-n)
        (let [markov-options (markov (->> result
                                          (take target-markov-n)
                                          (map :norm-word)))
              markov-option-avg (/ (apply + (vals markov-options))
                                   (max 1 (count markov-options)))]
          (if (nil? markov-options)
            [words target result]
            (let [[markovs non-markovs]
                  ((juxt filter remove)
                   #(markov-options (:norm-word %))
                   words)
                  weight-non-markovs (apply + (map :weight non-markovs))
                  target-weight-markovs (* 100 percent weight-non-markovs)
                  count-markovs (count markovs)
                  adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
              [(concat
                (map
                 (fn [m]
                   (let [option (markov-options (:norm-word m))]
                     (as-> m m
                       (assoc m :weight (* (/ option markov-option-avg) adjustment-markovs (:weight m)))
                       (assoc m :adjustment-for-markov (* (/ option markov-option-avg) adjustment-markovs)))))
                 markovs)
                non-markovs)
               target
               result])))
        [words target result]))))

(defn adjust-for-markov-with-boundaries
  [markov percent]
  (let [markov-n (count (first (first markov)))]
    (fn [[words target result]]
      (let [key (let [k (map :norm-word (take markov-n result))]
                  (reverse
                   (if (> markov-n (count k))
                     (concat k (repeat (- markov-n (count k)) nil))
                     k)))
            markov-options (markov key)
            markov-option-avg (/ (apply + (vals markov-options))
                                 (max 1 (count markov-options)))]
        (if (nil? markov-options)
          [words target result]
          (let [[markovs non-markovs]
                ((juxt filter remove)
                 #(markov-options (:norm-word %))
                 words)
                weight-non-markovs (apply + (map :weight non-markovs))
                target-weight-markovs (- (/ weight-non-markovs (- 1 percent))
                                         weight-non-markovs)
                count-markovs (count markovs)
                adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
            [(concat
              (map
               (fn [m]
                 (let [option (markov-options (:norm-word m))]
                   (as-> m m
                     (assoc m :weight (* (/ option markov-option-avg) adjustment-markovs (:weight m)))
                     (assoc m :adjustment-for-markov (* (/ option markov-option-avg) adjustment-markovs)))))
               markovs)
              non-markovs)
             target
             result]))))))

(comment
  (let [markov-1-example
        {'("dream") {"a" 1}
         '("a") {"me" 1}}
        markov-2-example
        {'(nil nil) {"dream" 1}
         '(nil "dream") {"a" 1}
         '("dream" "a") {"me" 1}
         '("a" "me") {"give" 1}
         '("give" nil) {nil 1}}
        result-a '()
        result-b '({:norm-word "dream",
                    :weight 9.000000000000002,
                    :adjustment-for-markov 9.000000000000002})
        words [{:norm-word "dream" :weight 1}
               {:norm-word "foo" :weight 1}
               {:norm-word "a" :weight 1}
               {:norm-word "me" :weight 1}
               {:norm-word "give" :weight 1}]
        adj (adjust-for-markov-with-boundaries markov-2-example 0.9)]
    (adj [words 'target result-b]))

  ((adjust-for-markov-with-boundaries {'("foo" "bar") {}} 0.5)
   ['() '() '("hi" "bye" "there")]))

(defn adjust-for-rimes
  [dictionary percent]
  (fn [[words target result]]
    (let [words-with-rime-count
          (map
           (fn [word]
             (assoc word :num-matching (count (frp/consecutive-matching target word :rimes))))
           words)

          [rhyming non-rhyming]
          ((juxt filter remove)
           #(< 0 (:num-matching %))
           words-with-rime-count)

          weight-non-rhyming (apply + (map :weight non-rhyming))
          target-weight-rhyming (* 100 percent weight-non-rhyming)
          count-rhyming (count rhyming)
          adjustment-rhyming (if (= 0 count-rhyming) 1 (/ target-weight-rhyming count-rhyming))]
      [(concat
        (map
         (fn [word]
           (as-> word word
             (assoc word :weight (* adjustment-rhyming (:weight word)))
             (assoc word :adjustment-for-rimes adjustment-rhyming)))
         rhyming)
        non-rhyming)
       target
       result])))

(defn adjust-for-tail-rimes
  [dictionary percent]
  (fn [[words target result]]
    (if (empty? result)
      (let [words-with-rime-count
            (map
             (fn [word]
               (assoc word :num-matching (if (prhyme/rimes? target word) 1 0)))
             words)

            [rhyming non-rhyming]
            ((juxt filter remove)
             #(< 0 (:num-matching %))
             words-with-rime-count)

            weight-non-rhyming (apply + (map :weight non-rhyming))
            target-weight-rhyming (* 100 percent weight-non-rhyming)
            count-rhyming (count rhyming)
            adjustment-rhyming (if (= 0 count-rhyming) 1 (/ target-weight-rhyming count-rhyming))]
        [(concat
          (map
           (fn [word]
             (as-> word word
               (assoc word :weight (* adjustment-rhyming (:weight word)))
               (assoc word :adjustment-for-rimes adjustment-rhyming)))
           rhyming)
          non-rhyming)
         target
         result])
      [words target result])))

(defn attempt-gen-target-by-syllable-count [adj syllable-count words]
  (loop [result '()]
    (cond
      (<= syllable-count (apply + (cons 0 (map :syllable-count result))))
      result
      :else
      (let [[weighted-words target result] (adj [words nil result])]
        (recur (cons (weighted-rand/weighted-selection :weight weighted-words) result))))))

(defn gen-sentence-with-syllable-count [adj syllable-count words]
  (->> (repeatedly
        (fn []
          (attempt-gen-target-by-syllable-count adj syllable-count words)))
       (filter #(= syllable-count (apply + (map :syllable-count %))))
       (map #(map :norm-word %))
       (map #(string/join " " %))
       (filter nlp/valid-sentence?)
       first))

(defn remove-selection-from-target [target selection]
  (->> target
       (#(assoc % :syllables (drop-last
                              (:syllable-count
                               selection)
                              (:syllables
                               target))))
       (#(assoc % :rimes (prhyme/rimes (:syllables %))))
       (#(assoc % :onsets (prhyme/onset+nucleus (:syllables %))))
       (#(assoc % :nuclei (prhyme/nucleus (:syllables %))))))

(defn selection-seq
  ([words adjust target]
   (selection-seq words adjust target '()))
  ([words adjust target result]
   (let [[weighted-words _ _] (adjust [words target result])
         selection (math/weighted-selection :weight weighted-words)
         new-target (remove-selection-from-target target selection)
         new-result (cons selection result)]
     (cons selection
           (lazy-seq (selection-seq words adjust new-target new-result))))))

(defn generate-prhyme [words adjust target stop?]
  (loop [result '()]))

(defn prhyme
  "2020-10-21 iteration"
  [words weights-adjuster target stop?]
  (let [target (assoc target :original-syllables (:syllables target))
        words (map #(assoc % :weight 1) words)]
    (loop [target target
           result '()
           sentinel 0]
      (if (or (stop? target result)
              (> sentinel 5))
        result
        (let [[weighted-words _ _] (weights-adjuster [words target result])
              rng (weighted-rand/from-weights (map :weight weighted-words))
              index (weighted-rand/nextr rng nil)
              selection (nth weighted-words index)
              new-target (->> target
                              (#(assoc % :syllables (drop-last
                                                     (:syllable-count
                                                      selection)
                                                     (:syllables
                                                      target))))
                              (#(assoc % :rimes (prhyme/rimes (:syllables %))))
                              (#(assoc % :onsets (prhyme/onset+nucleus (:syllables %))))
                              (#(assoc % :nuclei (prhyme/nucleus (:syllables %)))))
              result (cons selection result)]
          (recur new-target result (inc sentinel)))))))

(defn attempt-gen-rhyme-with-syllable-count [adj syllable-count words target]
  (prhyme
   words
   adj
   target
   (fn [target result]
     (<= syllable-count (apply + (map :syllable-count result))))))

(defn gen-rhyme-with-syllable-count [adj syllable-count words target]
  (->> (repeatedly
        (fn []
          (attempt-gen-rhyme-with-syllable-count adj syllable-count words target)))
       (filter #(= syllable-count (apply + (map :syllable-count %))))
       (map #(map :norm-word %))
       (map #(string/join " " %))
       (filter nlp/valid-sentence?)
       first))

(defn prhymer [words weights-adjuster target stop]
  (cons (prhyme
         words
         weights-adjuster
         target
         stop)
        (lazy-seq (prhymer words weights-adjuster target stop))))

(defn sentence-stop [target]
  (fn [inner-target result]
    (let [result-sentence (string/join " " (map :norm-word result))]
      (when-not (empty? result)
        (or (nlp/valid-sentence? result-sentence)
            (< (:syllable-count target)
               (apply + (map :syllable-count result)))
            (< 5 (count result)))))))

(defn gen-prhymes [words adjust poem-lines]
  (let [words (map #(assoc % :weight 1) words)
        words-map (into {} (map #(vector (:norm-word %) %) words))]
    (map (fn [line]
           (let [target (phrase->word words line)
                 stop (sentence-stop target)
                 r (prhymer words adjust target stop)]
             (string/join " " (map #(:norm-word %) (first r)))))
         poem-lines)))

(defn phrase-syllable-count [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (partial phrase->word frp/words))
       (map :syllable-count)
       (apply +)))

(defn filter-for-syllable-count [syllable-count coll]
  (filter #(= syllable-count (phrase-syllable-count %)) coll))

(defn syllable-stop
  [target]
  (fn [inner-target result]
    (<= (:syllable-count target)
        (apply + (map :syllable-count result)))))

(defn generate-rhyme-for-phrase
  [words adjust phrase]
  (let [words (map #(assoc % :weight 1) words)
        words-map (into {} (map #(vector (:norm-word %) %) words))
        target (phrase->word words phrase)]
    (prhymer words adjust target (syllable-stop target))))

#_(defn generate-prhymes [poem]
  (let [r (partial generate-rhyme-for-phrase frp/popular adj)]
    (fn []
      (->> poem
           (map (fn [phrase]
                  (let [target (phrase->word frp/popular phrase)]
                    (first
                     (filter
                      #(and
                        (or (< 0.9 (rand))
                            (nlp/valid-sentence? (string/join " " (map :norm-word %))))
                        (= (:syllable-count target)
                           (apply + (map :syllable-count %))))
                      (r phrase))))))
           (map (fn [line] (map #(:norm-word %) line)))
           (map #(string/join " " %))))))

(defn generate-prhymes-darkov [words adj phrase]
  (let [target (phrase->word words phrase)
        r (generate-rhyme-for-phrase words adj target)]
    (first
     (filter
      #(and
        (or (< 0.9 (rand))
            (nlp/valid-sentence? (string/join " " (map :norm-word %))))
        (= (:syllable-count target)
           (apply + (map :syllable-count %))))
      r))
    (map (fn [line] (map #(:norm-word %) line)))
    (map #(string/join " " %))))

(comment
  (let [adj (comp (adjust-for-markov-with-boundaries dr/darkov-2 0.9)
                  (adjust-for-tail-rimes words-map 0.9))]
    (->> (generate-rhyme-for-phrase frp/popular adj "make him the cutest that i've ever seen")
         (take 20)
         (map #(map :norm-word %))
         (map #(string/join " " %))))

  (let [adj (comp (adjust-for-markov-with-boundaries dr/darkov-2 0.9)
                  (adjust-for-tail-rimes words-map 0.9))]
    (->> (generate-rhyme-for-phrase frp/popular adj "mister sandman give me a dream")
         (take 20)
         (map #(map :norm-word %))
         (map #(string/join " " %))))

  (let [adj (adjust-for-markov-with-boundaries dr/darkov-2 0.9)]
    (apply map vector
           (->> ["mister sandman give me a dream"
                 "make him the cutest that i've ever seen"
                 "give him two lips like roses in clover"
                 "then tell him that his lonesome nights are over"]
                (map #(generate-prhymes-darkov util/popular adj %)))))

  (apply map vector (->> ["taylor is my star"
                          "she brightens my day"]
                         (generate-prhymes)
                         (repeatedly)
                         (take 10)))

  (frp/phrase->word frp/popular "homer")
  (frp/phrase->word frp/popular "")
  (apply map vector (->> ["mister sandman"
                          "give me a dream"
                          "make him the cutest"
                          "that i've ever seen"]
                         (generate-prhymes)
                         (repeatedly)
                         (take 10)))

  (def adj (comp (adjust-for-markov dr/darkov 0.9)
                 (adjust-for-tail-rimes words-map 0.9)))

  (let [r (generate-rhyme-for-phrase
           frp/popular
           adj
           "mister sandman")]
    (take 3 r))

  (def r (partial generate-rhyme-for-phrase frp/popular adj))
  (take
   10
   (repeatedly
    (fn []
      (->> ["mister sandman"
            "give me a dream"
            "make him the cutest"
            "that i've ever seen"]
           (map (fn [phrase]
                  (let [target (phrase->word frp/popular phrase)]
                    (first
                     (filter
                      #(= (:syllable-count target)
                          (apply + (map :syllable-count %)))
                      (r phrase))))))
           (map (fn [line] (map #(:norm-word %) line)))
           (map #(string/join " " %))))))

  (map #(take 1 %) (map r ["mister sandman"
                           "give me a dream"
                           "make him the cutest"
                           "that i've ever seen"]))
  (take 3 frp/words)
  (phrase->word frp/popular "well-off")
  (map (fn [line] (phrase->word frp/popular line))
       ["mister sandman"
        "give me dream"
        "make him the cutest"
        "that i've ever seen"])

  (defonce lovecraft-markov (read-string (slurp "lovecraft.edn")))

  (->> (gen-prhymes frp/popular
                    adj
                    ["mister sandman"
                     "give me dream"
                     "make him the cutest"
                     "that i've ever seen"]))

  (take 5 (filter #(= 7 (phrase-syllable-count (first %)))
                  (repeatedly #(gen-prhymes frp/popular adj ["taylor is my beautiful"]))))

  (let [target (frp/phrase->word frp/words "i solemnly swear i am up to no good")
        words (map #(assoc % :weight 1) frp/popular)
        weights-adjuster (comp (adjust-for-markov lovecraft-markov 0.9)
                               (adjust-for-rimes target words-map 0.9))
        stop (sentence-stop target)
        r (prhymer words weights-adjuster target stop)]
    (map (fn [p] (string/join " " (map #(:norm-word %) p))) (take 5 r))))
