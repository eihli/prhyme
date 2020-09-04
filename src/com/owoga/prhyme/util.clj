(ns com.owoga.prhyme.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.zip :as z]))

;; {"AY" "vowel
;;  "B"  "
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

(def dictionary
  (line-seq (io/reader (io/resource "cmudict_SPHINX_40"))))

(defn prepare-word
  "Splits whitespace-separated fields into a sequence."
  [line]
  (string/split line #"[\t ]"))

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
