(ns com.owoga.prhyme.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import (com.sun.speech.freetts.en.us CMULexicon)))

(defn prepare-word
  "Splits whitespace-separated fields into a sequence."
  [line]
  (string/split line #"[\t ]"))

(CMULexicon. "cmulex" true)

(def cmu-lexicon (CMULexicon/getInstance true))

(defn remove-stress [phoneme]
  (string/replace phoneme #"\d" ""))

(defn convert-to-sphinx [phoneme]
  (if (= phoneme "ax")
    "ah"
    phoneme))

(comment
  (map str (.getPhones cmu-lexicon "two" nil)))

(defn get-phones [word]
  (->> (map str (.getPhones cmu-lexicon word nil))
       (map remove-stress)
       (map convert-to-sphinx)
       (map string/upper-case)))

(defn window [n]
  (fn [coll]
    (cond
      (empty? coll) []
      (< (count coll) n) []
      :else (cons (take n coll)
                  (lazy-seq ((window n) (rest coll)))))))

(defn comp-rnil
  "Compose functions, ignoring nil values."
  [& functions]
  (apply comp (remove nil? functions)))

(defn reduce-while
  [pred f val coll]
  (loop [val val
         coll coll]
    (cond
      (empty? coll) val
      (pred val)
      (let [new-val (f val (first coll))]
        (recur new-val (rest coll)))
      :else val)))

(defn extend-coll [coll val n]
  (concat (repeat n val)
          coll
          (repeat n val)))

(defn window-with-nil [n]
  (fn [coll]
    (cond
      (empty? coll) []
      (< (count coll) n) []
      :else (cons (take n coll)
                  (lazy-seq ((window n) (drop n coll)))))))

(defn clean-text [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]" "")))

(defn padr [val n coll]
  (concat coll (repeat n val)))

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
