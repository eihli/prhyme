(ns com.owoga.prhyme.gen
  (:require [clojure.string :as string]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [com.owoga.prhyme.util.nlp :as nlp]
            [com.owoga.prhyme.frp :as frp]
            [com.owoga.prhyme.core :as prhyme]))

(def words-map
  (into {} (map #(vector (string/lower-case (:word %)) %) frp/words)))

(defn merge-phrase-words
  "Given multiple `Word`, like the words for 'well off', create a single `Word`
  that is syllabified as ('well' 'off') rather than as the combined ('weh'
  'loff'). Useful for finding single-word rhymes of multiple-word targets.

  An example: 'war on crime' -> 'turpentine'."
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
                (when (nil? word)
                  (throw (ex-info "Word not found in dictionary." {:word phrase-word})))
                word)))
       (merge-phrase-words phrase)))

(defn adjust-for-markov
  [markov percent]
  (fn [[words target result]]
    (let [markov-options (markov (list (:norm-word (first result))))
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
           result])))))

(defn adjust-for-rimes
  [target-rime dictionary percent]
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

(defn gen-prhymes [words markov poem-lines]
  (let [words (map #(assoc % :weight 1) words)
        words-map (into {} (map #(vector (:norm-word %) %) words))]
    (map (fn [line]
           (let [target (frp/phrase->word words line)
                 stop (sentence-stop target)
                 weights-adjuster (comp (adjust-for-markov markov 0.9)
                                        (adjust-for-rimes target words-map 0.9))
                 r (prhymer words weights-adjuster target stop)]
             (string/join " " (map #(:norm-word %) (first r)))))
         poem-lines)))

(comment
  (take 3 frp/words)
  (phrase->word frp/popular "well-off")
  (map (fn [line] (phrase->word frp/popular line))
       ["mister sandman"
        "give me dream"
        "make him the cutest"
        "that have ever seen"])
  (defonce lovecraft-markov (read-string (slurp "lovecraft.edn")))
  (def adj (comp (adjust-for-markov lovecraft-markov 0.99)))
  (gen-prhymes frp/popular
               lovecraft-markov
               ["mister sandman"
                "give me the dream"
                "make him the cutest"
                "that eye have ever seen"])
  (repeatedly 20 #(gen-prhymes frp/popular lovecraft-markov ["mister sandman"]))

  (let [target (frp/phrase->word frp/words "i solemnly swear i am up to no good")
        words (map #(assoc % :weight 1) frp/popular)
        weights-adjuster (comp (adjust-for-markov lovecraft-markov 0.9)
                               (adjust-for-rimes target words-map 0.9))
        stop (sentence-stop target)
        r (prhymer words weights-adjuster target stop)]
    (map (fn [p] (string/join " " (map #(:norm-word %) p))) (take 5 r))))
