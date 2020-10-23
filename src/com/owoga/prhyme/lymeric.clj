(ns com.owoga.prhyme.lymeric
  (:require [com.owoga.prhyme.gen :as gen]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [clojure.string :as string]
            [com.owoga.prhyme.frp :as frp]
            [com.owoga.prhyme.util.nlp :as nlp]
            [com.owoga.corpus.darklyrics :as darklyrics]))


(defn rhyme-from-scheme
  "scheme of format [[A 9] [A 9] [B 5] [B 5] [A 9]]"
  [rhymer scheme]
  (let [base-words (map #(assoc % :weight 1) frp/popular)]
    (loop [scheme scheme
           rhymes {}
           result []]
      (cond
        (empty? scheme) result
        :else
        (let [[pattern syllable-count] (first scheme)
              banned-words (into #{} (->> result
                                          (map #(string/split % #" "))
                                          (map #(last %))))
              adj (apply
                   comp
                   (remove
                    nil?
                    [(gen/adjust-for-markov-with-boundaries darklyrics/darkov-2 0.9)
                     (when (rhymes pattern)
                       (gen/adjust-for-tail-rimes util/words-map 0.9))]))
              rhyme (if (nil? (get rhymes pattern))
                      (gen/gen-sentence-with-syllable-count
                       adj
                       syllable-count
                       base-words)
                      (gen/gen-rhyme-with-syllable-count
                       adj
                       syllable-count
                       (remove #(banned-words (:norm-word %))
                               base-words)
                       (frp/phrase->word frp/words (get rhymes pattern))))]
          (println banned-words)
          (recur (rest scheme)
                 (assoc rhymes pattern rhyme)
                 (conj result rhyme)))))))


(comment
  (rhyme-from-scheme nil '((A 8) (A 8) (B 5) (B 5) (A 8)))

  (first (filter #(= "abba" (:norm-word %)) frp/words))

  (gen/gen-sentence-with-syllable-count darklyrics/darkov-2 8 (map #(assoc % :weight 1) frp/popular))
  (def adj
    (comp (gen/adjust-for-markov-with-boundaries darklyrics/darkov-2 0.9)
          (gen/adjust-for-tail-rimes util/words-map 0.99)))
  (repeatedly
   10
   (fn []
     (gen/gen-rhyme-with-syllable-count
      adj
      8
      frp/popular
      (frp/phrase->word frp/words "famed watched waterloo"))))

  )

(comment
  ["romancing realized too late"
   "my crown revive my withered state"
   "reign is obsolete"
   "i sit in the street"
   "but nobody cares of my fate"]
  ["flesh is hacked to get me sedate"
   "demonstration obsessed with hate"
   "justice will be written in stone"
   "and you will be shown"
   "bedrooms of icons suffocate"]
  ["you will bow to their hungry gods"
   "come will come whatever the odds"
   "now we see the light"
   "you can't put it right"
   "recklessly chopping firing squads"]
  ["untimely they fool their poor life"
   "it wither away with this knife"
   "hate is my virtue"
   "my feelings are well overdue"
   "war we await the afterlife"]
  )
(->> (repeatedly
      (fn []
        (gen/gen-target-by-syllable-count darklyrics/darkov-2 8 (map #(assoc % :weight 1) frp/popular))))
     (filter #(= 8 (apply + (map :syllable-count %))))
     (map #(map :norm-word %))
     (map #(string/join " " %))
     (filter nlp/valid-sentence?)
     (take 5))

(take 3 frp/popular)
(defn genlymeric []
  (let [adj (comp (gen/adjust-for-markov darklyrics/darkov-2)
                  (gen/adjust-for-tail-rimes util/words-map))]))

(apply (fnil + 0) '())

(map :syllable-count '())
