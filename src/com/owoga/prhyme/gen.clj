(ns com.owoga.prhyme.gen
  (:require [clojure.string :as string]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [com.owoga.prhyme.nlp.core :as nlp]
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

(defn adjust-for-markov
  [markov percent]
  (let [target-markov-n (count (first (first markov)))]
    (fn [[words target result]]
      (if (>= (count result) target-markov-n)
        (let [markov-options (markov (->> result
                                          (take target-markov-n)
                                          (map :normalized-word)))
              markov-option-avg (/ (apply + (vals markov-options))
                                   (max 1 (count markov-options)))]
          (if (nil? markov-options)
            [words target result]
            (let [[markovs non-markovs]
                  ((juxt filter remove)
                   #(markov-options (:normalized-word %))
                   words)
                  weight-non-markovs (apply + (map :weight non-markovs))
                  target-weight-markovs (* 100 percent weight-non-markovs)
                  count-markovs (count markovs)
                  adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
              [(concat
                (map
                 (fn [m]
                   (let [option (markov-options (:normalized-word m))]
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
      (let [key (let [k (map :normalized-word (take markov-n result))]
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
                 #(markov-options (:normalized-word %))
                 words)
                weight-non-markovs (apply + (map :weight non-markovs))
                target-weight-markovs (- (/ weight-non-markovs (- 1 percent))
                                         weight-non-markovs)
                count-markovs (count markovs)
                adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
            [(concat
              (map
               (fn [m]
                 (let [option (markov-options (:normalized-word m))]
                   (as-> m m
                     (assoc m :weight (* (/ option markov-option-avg) adjustment-markovs (:weight m)))
                     (assoc m :adjustment-for-markov (* (/ option markov-option-avg) adjustment-markovs)))))
               markovs)
              non-markovs)
             target
             result]))))))

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
       (map #(map :normalized-word %))
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

(defn selection-stream
  "Continuously make the first selection."
  ([words adjust target]
   (selection-stream words adjust target '()))
  ([words adjust target result]
   (let [[weighted-words _ _] (adjust [words target result])]
     (repeatedly #(math/weighted-selection :weight weighted-words)))))

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
       (map #(map :normalized-word %))
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
    (let [result-sentence (string/join " " (map :normalized-word result))]
      (when-not (empty? result)
        (or (nlp/valid-sentence? result-sentence)
            (< (:syllable-count target)
               (apply + (map :syllable-count result)))
            (< 5 (count result)))))))

(defn gen-prhymes [words adjust poem-lines]
  (let [words (map #(assoc % :weight 1) words)
        words-map (into {} (map #(vector (:normalized-word %) %) words))]
    (map (fn [line]
           (let [target (prhyme/phrase->word words line)
                 stop (sentence-stop target)
                 r (prhymer words adjust target stop)]
             (string/join " " (map #(:normalized-word %) (first r)))))
         poem-lines)))

(defn phrase-syllable-count [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (partial prhyme/phrase->word frp/words))
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
        words-map (into {} (map #(vector (:normalized-word %) %) words))
        target (prhyme/phrase->word words phrase)]
    (prhymer words adjust target (syllable-stop target))))

#_(defn generate-prhymes [poem]
  (let [r (partial generate-rhyme-for-phrase frp/popular adj)]
    (fn []
      (->> poem
           (map (fn [phrase]
                  (let [target (prhyme/phrase->word frp/popular phrase)]
                    (first
                     (filter
                      #(and
                        (or (< 0.9 (rand))
                            (nlp/valid-sentence? (string/join " " (map :normalized-word %))))
                        (= (:syllable-count target)
                           (apply + (map :syllable-count %))))
                      (r phrase))))))
           (map (fn [line] (map #(:normalized-word %) line)))
           (map #(string/join " " %))))))

(defn generate-prhymes-darkov [words adj phrase]
  (let [target (prhyme/phrase->word words phrase)
        r (generate-rhyme-for-phrase words adj target)]
    (first
     (filter
      #(and
        (or (< 0.9 (rand))
            (nlp/valid-sentence? (string/join " " (map :normalized-word %))))
        (= (:syllable-count target)
           (apply + (map :syllable-count %))))
      r))
    (map (fn [line] (map #(:normalized-word %) line)))
    (map #(string/join " " %))))
