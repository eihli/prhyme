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

(defn -main [directory]
  (let [directory (io/file "dark-corpus")
        files (file-seq directory)
        lines (->> files
                   (remove #(.isDirectory %))
                   (map #(slurp %))
                   (map #(string/split % #"\n+"))
                   #_(map #(util/extend-coll % nil 1)))
        markovs (->> lines
                     (map #(make-markov % 1)))]
    (take 1 markovs)))

(merge-with
 (fn [a b]
   (+ (if (nil? a) 0 a)
      b))
 {:foo 1}
 {:bar 2 :foo 3})

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

(defn gen-markov []
  (->> (file-seq (io/file "dark-corpus"))
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
       (util/write-markov "dark-corpus-2.edn")))

(comment
  (gen-markov)
  (->> (file-seq (io/file "dark-corpus"))
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
       (util/write-markov "dark-corpus-2.edn"))

  (def darkov-2 (util/read-markov "dark-corpus-2.edn"))
  (def darkov-1 (util/read-markov "dark-corpus-1.edn"))
  (get darkov-2 '(nil nil))
  (darkov-1 '("london"))

  (-main "dark-lyrics"))
