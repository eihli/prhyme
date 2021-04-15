(ns com.owoga.prhyme.data.phonetics
  (:require [clojure.string :as string]
            [clojure.set]
            [clojure.java.io :as io]))

(def phonemap
  (->> (io/reader (io/resource "cmudict-0.7b.phones"))
       (line-seq)
       (map #(string/split % #"\t"))
       (into {})))

(comment
  (take 5 (seq phonemap))
  ;; => (["T" "stop"] ["CH" "affricate"] ["K" "stop"] ["HH" "aspirate"] ["UH" "vowel"])
  )

(def long-vowel #{"EY" "IY" "AY" "OW" "UW"})

(def short-vowel #{"AA" "AE" "AH" "AO" "AW" "EH" "ER" "IH" "OY" "UH"})

(def vowel (clojure.set/union long-vowel short-vowel))

(def consonant (clojure.set/difference (into #{} (keys phonemap)) vowel))

(def syllable-end (clojure.set/union consonant long-vowel))

(def single-sound-bigram #{"TH" "SH" "PH" "WH" "CH"})
