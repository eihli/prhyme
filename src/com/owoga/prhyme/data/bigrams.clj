(ns com.owoga.prhyme.data.bigrams
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.owoga.prhyme.syllabify :refer [syllabify]]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.core :as prhyme]))

(def popular-bigrams
  (->> (io/resource "bigrams-with-counts.txt")
       io/reader
       line-seq
       (map #(string/split % #"\s+"))
       (map #(map string/lower-case %))
       (filter (fn [[w1 w2 num]]
                 (and (dict/popular w1)
                      (dict/popular w2))))
       (map (partial take 2))))

(def phrase->Word
  (prhyme/make-phrase->Word
   (into
    {}
    (map
     (fn [[word & phonemes]]
       [(string/lower-case word)
        phonemes])
     dict/cmu-dict))))

(def popular-bigrams-with-syllables
  (->> popular-bigrams
       (map (partial string/join " "))
       (map phrase->Word)))

(def popular-bigram-words
  (->> popular-bigrams
       (map (partial string/join " "))
       (map phrase->Word)))

(def popular-mono-and-bi-grams
  (concat
   dict/prhyme-dict
   popular-bigram-words))

(def popular-mono-and-bi-grams-by-rime
  (prhyme/prhyme-words-by-rime*
   popular-mono-and-bi-grams))

(comment
  (->> (reverse (:rimes (phrase->Word "beautiful")))
       (get-in popular-mono-and-bi-grams-by-rime)
       (prhyme/flatten-node)
       (remove #(re-matches #".*beautiful" (:word %)))
       (map :word)
       (map string/lower-case))
  (get-in
   popular-mono-and-bi-grams-by-rime
   ['("AA") '("ER") '("IY")])
  (take 5 popular-bigrams)
  (take 5 popular-bigram-words)
  (take-last 5 popular-bigram-words)
  (prhyme/phrase->word dict/prhyme-dict "hi there"))
(def words-and-bigrams())
