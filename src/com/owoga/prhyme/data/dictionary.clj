(ns com.owoga.prhyme.data.dictionary
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.core :as prhyme]))

(def cmu-dict
  (->> (io/reader (io/resource "cmudict_SPHINX_40"))
       (line-seq)
       (map #(string/split % #"[\t ]"))))

(def spelling->phonemes
  (loop [words cmu-dict
         accum {}]
    (let [word (first words)
          key ((fnil util/clean-text "") (first word))]
      (cond
        (nil? word) accum
        :else (recur (rest words)
                     (update accum key (fnil conj []) (rest word)))))))

(def phrase->Word
  (into
   {}
   (map
    (fn [[word & phonemes]]
      [(string/lower-case word)
       phonemes])
    cmu-dict)))

(def prhyme-dict
  (into [] (map prhyme/cmu->prhyme cmu-dict)))

(def popular
  (set (line-seq (io/reader (io/resource "popular.txt")))))

(def adverbs
  (clojure.set/intersection
   popular
   (set (line-seq (io/reader (io/resource "adverbs.txt"))))))

(def adjectives
  (clojure.set/intersection
   popular
   (set (line-seq (io/reader (io/resource "adjectives.txt"))))))

(def verbs
  (clojure.set/intersection
   popular
   (set (line-seq (io/reader (io/resource "verbs.txt"))))))

(def nouns
  (clojure.set/intersection
   popular
   (set (line-seq (io/reader (io/resource "nouns.txt"))))))

(defn english? [text]
  (let [word-set (into #{} (map :normalized-word prhyme-dict))
        words (string/split text #"\s+")
        english-words
        (->> words
             (filter #(word-set (string/lower-case %))))]
    (< 0.7 (/ (count english-words) (max 1 (count words))))))
