(ns com.owoga.prhyme.limerick
  (:require [com.owoga.prhyme.gen :as gen]
            [com.owoga.prhyme.generation.weighted-selection :as weighted-selection]
            [clojure.string :as string]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]))

(defn rhyme-from-scheme
  "scheme of format [[A 9] [A 9] [B 5] [B 5] [A 9]]"
  [words markov scheme]
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
            adj (util/comp-rnil
                 (weighted-selection/adjust-for-markov
                  markov
                  0.99)
                 (when (rhymes pattern)
                   (weighted-selection/adjust-for-tail-rhyme 0.99)))
            rhyme (if (nil? (get rhymes pattern))
                    (gen/gen-sentence-with-syllable-count
                     adj
                     syllable-count
                     words)
                    (gen/gen-rhyme-with-syllable-count
                     adj
                     syllable-count
                     (remove #(banned-words (:normalized-word %)) words)
                     (prhyme/phrase->word words (get rhymes pattern))))]
        (recur (rest scheme)
               (assoc rhymes pattern rhyme)
               (conj result rhyme))))))

(comment
  (require '[com.owoga.prhyme.data.dictionary :as dict]
           '[com.owoga.prhyme.data.darklyrics :refer [darklyrics-markov-2]]
           '[clojure.java.io :as io])
  (rhyme-from-scheme dict/prhyme-dict darklyrics-markov-2 '((A 8) (A 8) (B 5) (B 5) (A 8)))

  )

(comment
  ["bishop larch smitten us dwell"
   "solely first week in hell"
   "and take that for three"
   "come wrapped in glory"
   "you ever leave it so well"]
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
   "war we await the afterlife"])


