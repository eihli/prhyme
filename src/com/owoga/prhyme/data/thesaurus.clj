(ns com.owoga.prhyme.data.thesaurus
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def thesaurus
  (->> (line-seq (io/reader (io/resource "mthesaur.txt")))
       (map #(string/split % #","))
       (map #(vector (first %) (rest %)))
       (into {})))
