(ns com.owoga.prhyme.data.thesaurus
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set]))

(def thesaurus
  (->> (line-seq (io/reader (io/resource "mthesaur.txt")))
       (map #(string/split % #","))
       (map #(vector (first %) (rest %)))
       (into {})))

(defn synonyms
  ([& words]
   (->> words
        (map string/lower-case)
        (map #(get thesaurus %))
        (map (fn [ws] (into #{} ws)))
        (apply clojure.set/intersection))))
