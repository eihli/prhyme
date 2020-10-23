(ns com.owoga.prhyme.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.zip :as z])
  (:import (com.sun.speech.freetts.lexicon LetterToSoundImpl)
           (com.sun.speech.freetts.en.us CMULexicon)
           (java.io File)))

(defn prepare-word
  "Splits whitespace-separated fields into a sequence."
  [line]
  (string/split line #"[\t ]"))

(def dictionary
  (line-seq (io/reader (io/resource "cmudict_SPHINX_40"))))

(def words (map prepare-word dictionary))

(def words-map
  (into {} (map #(vector (string/lower-case (first %)) {:phonemes (rest %)}) words)))

(def popular
  (set (line-seq (io/reader (io/resource "popular.txt")))))

(def adverbs
  (set/intersection popular (set (line-seq (io/reader (io/resource "adverbs.txt"))))))

(def adjectives
  (set/intersection popular (set (line-seq (io/reader (io/resource "adjectives.txt"))))))

(def verbs
  (set/intersection popular (set (line-seq (io/reader (io/resource "verbs.txt"))))))

(def nouns
  (set/intersection popular (set (line-seq (io/reader (io/resource "nouns.txt"))))))


(CMULexicon. "cmulex" true)

(def cmu-lexicon (CMULexicon/getInstance true))

(defn remove-stress [phoneme]
  (string/replace phoneme #"\d" ""))

(defn convert-to-sphinx [phoneme]
  (if (= phoneme "ax")
    "ah"
    phoneme))

(defn get-phones [word]
  (->> (map str (.getPhones cmu-lexicon word nil))
       (map remove-stress)
       (map convert-to-sphinx)
       (map string/upper-case)))

(def phonemap
  (->> (io/reader (io/resource "cmudict-0.7b.phones"))
       (line-seq)
       (map #(string/split % #"\t"))
       (into {})))

(def long-vowel #{"EY" "IY" "AY" "OW" "UW"})

(def short-vowel #{"AA" "AE" "AH" "AO" "AW" "EH" "ER" "IH" "OY" "UH"})

(def vowel (set/union long-vowel short-vowel))

(def consonant (set/difference (into #{} (keys phonemap)) vowel))

(def syllable-end (set/union consonant long-vowel))

(def single-sound-bigram #{"TH" "SH" "PH" "WH" "CH"})

(defn window [n]
  (fn [coll]
    (cond
      (empty? coll) []
      (< (count coll) n) []
      :else (cons (take n coll)
                  (lazy-seq ((window n) (drop n coll)))))))

(defn clean-text [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]" "")))

(defn make-markov [text]
  (let [tokens (reverse (string/split (clean-text text) #"\s+"))]
    (reduce
     (fn [a [t1 t2]]
       (update-in a [t1 t2] (fnil inc 0)))
     {}
     ((window 2) tokens))))

(defn write-markov [filename markov]
  (spit filename (pr-str markov)))

(defn read-markov [filename]
  (read-string (slurp filename)))

(defn take-through
  "(take-through even? [1 2 3 4 7 7 5 2 8 10])
   returns '((1 2 3 4) (7 7 5 2) (8) (10))"
  [pred coll]
  (loop [coll coll
         acc '()]
    (cond
      (empty? coll)
      (if (empty? acc) acc (list (reverse acc)))

      (pred (first coll))
      (let [acc (cons (first coll) acc)]
        (lazy-seq (cons (reverse acc) (take-through pred (rest coll)))))

      :else
      (recur (rest coll)
             (cons (first coll) acc)))))

(defn take-between
  "Seq [) of pred."
  [pred coll]
  (->> coll
       (drop-while #(not (pred %)))
       ((fn [coll]
          (if (empty? coll)
            nil
            (cons (cons (first coll) (take-while #(not (pred %)) (rest coll)))
                  (lazy-seq (take-between pred (rest coll)))))))))

(defn max-consecutive [pred coll]
  (loop [coll coll
         cur-count 0
         max-count 0]
    (cond
      (empty? coll) max-count
      (pred (first coll)) (recur (rest coll) (inc cur-count) max-count)
      :else (recur (rest coll) 0 (max cur-count max-count)))))

(defn count-pred [pred coll]
  (count (filter pred coll)))

(def count-vowels (partial count-pred vowel))

(defn single? [coll] (= 1 (count coll)))

(defn partitions
  "There is a partitions in clojure.combinatorics that might be more
  efficient. This was fun to write. Want to understand more ways to
  write this algorithm. How to make it lazy? How to jump immediately
  to a specific rank?"
  ([coll]
   (partitions coll '()))
  ([coll acc]
   (cond
     (empty? coll) acc
     (single? coll) `((~coll))
     :else
     (let [x (first coll)]
       (reduce (fn [val el]
                 (cons
                  (cons (cons x (first el)) (rest el))
                  (cons (cons (list x) el) val)))
               '()
               (partitions (rest coll) acc))))))
