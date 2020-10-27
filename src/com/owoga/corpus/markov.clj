(ns com.owoga.corpus.markov
  (:require [com.owoga.prhyme.util :as util]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn clean-text [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]" "")))

(defn make-markov [tokens n]
  (reduce
   (fn [a w]
     (let [k (butlast w)
           v (last w)]
       (update-in a [k v] (fnil inc 0))))
   {}
   ((util/window (inc n)) tokens)))

(defn merge-markov [& maps]
  (apply
   merge-with
   (fn [a-possibilities b-possibilities]
     (merge-with
      (fn [a b]
        ((fnil + 0) a b))
      a-possibilities
      b-possibilities))
   maps))

(comment
  (merge-markov
   {'("away") {"her" 1
               "foo" 7}}
   {'("away") {"her" 2
               "them" 1
               "bar" 8}}
    {'("away") {"her" 10
               "them" 50
               "baz" 99}}))

(defn gen-markov [directory]
  (->> (file-seq (io/file directory))
       (remove #(.isDirectory %))
       (map #(slurp %))
       (map clean-text)
       (filter util/english?)
       (map #(string/split % #"\n+"))
       (flatten)
       (map #(string/split % #"\s+"))
       (map reverse)
       (map #(util/extend-coll % nil 2))
       (map #(make-markov % 2))
       (apply merge-markov)
       (util/write-markov "resources/dark-corpus-2.edn")))
