(ns com.owoga.prhyme.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.zip :as z]))

;; Pulled from cmudict-0.7b.phones.
(def phonemap
  {"AA" "vowel"
   "AE" "vowel"
   "AH" "vowel"
   "AO" "vowel"
   "AW" "vowel"
   "AY" "vowel"
   "B"  "stop"
   "CH" "affricate"
   "D"  "stop"
   "DH" "fricative"
   "EH" "vowel"
   "ER" "vowel"
   "EY" "vowel"
   "F"  "fricative"
   "G"  "stop"
   "HH" "aspirate"
   "IH" "vowel"
   "IY" "vowel"
   "JH" "affricate"
   "K"  "stop"
   "L"  "liquid"
   "M"  "nasal"
   "N"  "nasal"
   "NG" "nasal"
   "OW" "vowel"
   "OY" "vowel"
   "P"  "stop"
   "R"  "liquid"
   "S"  "fricative"
   "SH" "fricative"
   "T"  "stop"
   "TH" "fricative"
   "UH" "vowel"
   "UW" "vowel"
   "V"  "fricative"
   "W"  "semivowel"
   "Y"  "semivowel"
   "Z"  "fricative"
   "ZH" "fricative"})

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

(defn take-through [pred coll]
  "(take-through even? [1 2 3 4 7 7 5 2 8 10])
   returns '((1 2 3 4) (7 7 5 2) (8) (10))"
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

(defn pp-word [word]
  (let [spelling (first word)
        phones (rest word)
        phonetypes (map phonemap phones)
        formatted-phones (map #(format "%-10s" %) phones)
        formatted-phonetypes (map #(format "%-10s" %) phonetypes)]
    (format "%s\n%s\n%s"
            spelling
            (string/join " " formatted-phones)
            (string/join " " formatted-phonetypes))))

(defn count-pred [pred coll]
  (count (filter pred coll)))

(def count-vowels (partial count-pred vowel))

(defn node->zipper [node]
  (z/zipper (fn branch? [node]
              (cond
                (map? node)
                (->> (keys (into {} node))
                     (remove #{:word})
                     ((complement empty?)))
                :else
                (do
                  (let [b (->> (keys (into {} (second node)))
                               (remove #{:word})
                               ((complement empty?)))]
                    b))))
            (fn children [node]
              (let [node (if (map? node) node (second node))
                    ch (seq (select-keys node (remove #{:word} (keys node))))]
                ch))
            (fn make-node [node ch]
              (into {} ch))
            node))

(defn leafs [leaf? zipper]
  (->> zipper
       (iterate z/next)
       (take-while (complement z/end?))
       (map z/node)
       (filter leaf?)))

(def word-leafs (partial leafs (fn [node] (:word (second node)))))
