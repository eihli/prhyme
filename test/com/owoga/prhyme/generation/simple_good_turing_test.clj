(ns com.owoga.prhyme.generation.simple-good-turing-test
  (:require [com.owoga.prhyme.generation.simple-good-turing :as sgt]
            [com.owoga.prhyme.data.dictionary :as dict]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]))

(def train-corpus
  (with-open [reader (io/reader (io/resource "dark-corpus-train.txt"))]
    (->> (line-seq reader) doall)))

(def test-corpus
  (with-open [reader (io/reader (io/resource "dark-corpus-test.txt"))]
    (->> (line-seq reader) doall)))

(def train-trie
  (sgt/lines->trie train-corpus 3))

(def sgt-model
  (sgt/simple-good-turing train-trie))

(def vocab
  (into #{} (remove #{:count} (keys train-trie))))

(def maps-for-sgt (sgt/maps-for-simple-good-turing train-trie))

(def n->r->nr (first maps-for-sgt))
(def n->r->sgt-prob (second maps-for-sgt))
(def Ns (nth maps-for-sgt 2))
(def P0s (nth maps-for-sgt 3))

(deftest simple-good-turing
  (testing "accuracy"))

