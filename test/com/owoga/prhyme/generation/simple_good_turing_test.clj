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

(def test-sentence (first test-corpus))

(def test-tokens
  (sgt/pad-tokens (sgt/tokenize-line test-sentence) 1))

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

(partition 2 1 test-tokens)
;; => (("<s>" "three")
;;     ("three" "years")
;;     ("years" "passed")
;;     ("passed" "since")
;;     ("since" "it")
;;     ("it" "began")
;;     ("began" "</s>"))

(defn ngram-perplexity [model vocab n]
  (fn [tokens]
    (->> tokens
         (partition n 1)
         (map #(model vocab %))
         (map #(/ (Math/log %)
                  (Math/log 2))))))

(def unigram-perplexity (ngram-perplexity sgt-model vocab 1))
(def bigram-perplexity (ngram-perplexity sgt-model vocab 2))
(unigram-perplexity ["<s>" "you're" "a" "dweller" "</s>"])
;; => (-2.3988984034800693
;;     -9.075314877722885
;;     -4.843244892573044
;;     -11.69197410313705
;;     -2.3988984034800693)
;; => -30.408330680393117
(bigram-perplexity ["<s>" "you're" "a" "dweller" "</s>"])
;; => (-8.228033550648288 -9.95473739539021 -10.916015756609784 -13.731947650675696)
;; => -42.830734353323976
(sgt-model vocab '("it" "began"))
(->> test-corpus
     (map sgt/tokenize-line)
     (map #(sgt/pad-tokens % 1))
     (map #(partition 2 1 %))
     (map
      (fn [tokens]
        (map #(sgt-model vocab %) tokens)))
     (take 10)
     (map
      (fn [tokens]
        (map #(/ (Math/log %)
                 (Math/log 2))
             tokens))))

(/ (Math/log 10)
   (Math/log 2))
