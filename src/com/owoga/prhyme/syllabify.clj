(ns com.owoga.prhyme.syllabify
  (:require [com.owoga.prhyme.data.phonetics :as phonetics]
            [com.owoga.prhyme.util :as util]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

;; TODO:
;; ER is not yet handled properly.
;; PARENTHESES is syllabified as ("P" "ER" "IH" "N") ("TH" "UH") ("S" "IY" "S")
;; Glides are also broken. "R OY AH L" gets syllabified as a single syllable.

;; This sonority hierarchy is far from perfect.
;; It stems from: http://www.glottopedia.org/index.php/Sonority_hierarchy
;; I tried to match the phones provided by the CMU dict to the hierarchies
;; listed on that page:
;;   vowels > liquids > nasals > voiced fricatives
;;   > voiceless fricatives = voiced plosives
;;   > voiceless plosives (Anderson & Ewen 1987)
;;
;; *** Comment below this line is left as a future reference
;; *** but it does not reflect the true code.
;; One other modification I made is to put fricatives after stops.
;; I think that fricatives technically have priority over stops with the
;; exception of "s" at the end of codas. To quote a comment from a Reddit thread,
;; https://www.reddit.com/r/phonetics/comments/i7hp5f/what_is_the_alaska_rule_in_reference_to/
;;   Also, for "ellipsis", /ps/ is not a legal internal coda in English.
;;   The /s/ can only occur as an appendix, e.g. the plural -s at the end
;;   of a word. So it should be e.lip.sis
;; As an alternative to handling the isolated "s"-at-the-end-of-internal-coda case,
;; it works well-enough for me to treat all fricatives as lowest priority.

(def ^clojure.lang.PersistentVector sonority-hierarchy
  ["vowel" "liquid" "affricate" "fricative" "nasal" "stop" "semivowel" "aspirate"])

;; Ok. Sonority hierarchy doesn't work.
;; Think abount nasals and stops.
;; N D IH NG doesn't work.
;; D N IH NG doesn't work.
;; Nasal/Stop doesn't work in any order.

(def lax-vowels #{"EH" "IH" "AE" "AH" "UH"})

(defn sonority [phone]
  (.indexOf sonority-hierarchy (phonetics/phonemap phone)))

(defn vowel? [phone]
  (phonetics/vowel phone))

(defn >sonorous [a b]
  (> (sonority a) (sonority b)))

(defn slurp-rime [phones]
  (let [splits (util/take-through vowel? phones)]
    [(first splits) (flatten (rest splits))]))

(defn slurp-onset [phones]
  (if (empty? (take-while #(not (vowel? %)) phones))
    [[] phones]
    (loop [phones phones
          onset []]
     (cond
       (empty? phones) [onset []]
       (empty? (filter #(vowel? %) phones)) [(into onset phones) '()]
       (empty? onset) (recur (rest phones) [(first phones)])
       (not (>sonorous (first phones) (last onset))) [onset phones]
       :else (recur (rest phones) (conj onset (first phones)))))))

(defn fix-lax
  "https://www.reddit.com/r/phonetics/comments/i7hp5f/what_is_the_alaska_rule_in_reference_to/

  He wants to ensure that vowels that cannot form legal codas (lax vowels like
  /æ/) always have some sort of consonantal coda after them.
  "
  [syllables]
  (loop [old-syllables syllables
         new-syllables '()]
    (cond
      (empty? old-syllables) (reverse new-syllables)

      (and (lax-vowels (last (first old-syllables)))
           (< 1 (count old-syllables)))
      (recur (cons (rest (first (rest old-syllables)))
                   (rest (rest old-syllables)))
             (cons (concat (first old-syllables)
                           (list (first (first (rest old-syllables)))))
                   new-syllables))

      :else (recur (rest old-syllables)
                   (cons (first old-syllables) new-syllables)))))

(defn syllabify [phones]
  (let [phones (reverse phones)]
    (loop [phones phones
           segments []]
      (if (empty? phones)
        (reverse (map reverse segments))
        (let [[rime phones] (slurp-rime phones)
              [onset phones] (slurp-onset phones)]
          (cond
            ;; ROYAL -> R OY - AH L
            ;; DEBUTANT -> D EH B - Y AH - T AH N T
            (and (< 1 (count (first onset)))
                 (= \Y (last (first onset))))
            (recur phones (into segments [rime onset]))

            :else
            (recur phones (conj segments (concat rime onset)))))))))

(comment
  (syllabify ["AH" "L" "AE" "S" "K" "AH"])
  ;; => (("AH") ("L" "AE" "S") ("K" "AH"))
  (syllabify ["H" "ER" "AH" "L" "D"])
  ;; => (("H" "ER") ("AH" "L" "D"))
  (syllabify ["H" "EH" "R" "AH" "L" "D"])
  ;; => (("H" "EH") ("R" "AH" "L" "D"))
  (syllabify ["B" "OY" "N" "K"])
  ;; => (("B" "OY" "N" "K"))
  (syllabify ["G" "L" "IH" "M" "P" "S" "T"])
  ;; => (("G" "L" "IH" "M" "P" "S" "T"))
  (syllabify ["B" "IY" "G" "L" "IH" "M" "P" "S" "T"])
  ;; => (("B" "IY") ("G" "L" "IH" "M" "P" "S" "T"))
  (syllabify ["G" "L" "IH" "M" "P" "S" "T" "R" "EH" "D"])
  ;; => (("G" "L" "IH" "M" "P" "S") ("T" "R" "EH" "D"))
  (syllabify ["UH" "P" "R" "AY" "S" "IY" "NG"])
  ;; => (("UH") ("P" "R" "AY") ("S" "IY" "NG"))
  (syllabify ["UH" "L" "AE" "S" "K" "UH"])
  ;; => (("UH") ("L" "AE" "S") ("K" "UH"))
  (syllabify ["R" "OY" "AH" "L"])
  ;; => (("R" "OY") ("AH" "L"))
  (syllabify ["R" "AY" "AH" "L"])
  ;; => (("R" "AY") ("AH" "L"))
  ;; TODO: Fix below wi-thcheeze
  (syllabify ["R" "OY" "AH" "L" "W" "IH" "TH" "CH" "IY" "Z"])
  ;; => (("R" "OY") ("AH" "L") ("W" "IH") ("TH" "CH" "IY" "Z"))
  ;;
  ;; ["GLIMPSED" "G" "L" "IH" "M" "P" "S" "T"]
  ;; ["BEGLIMPSED" "B" "IY" "G" "L" "IH" "M" "P" "S" "T"]
  ;; ["BEGLIMPSED" "B" "EH" "G" "L" "IH" "M" "P" "S" "T"]
  ;; ["GLIMSTEST" "G" "L" "IH" "M" "S" "T" "EH" "S" "T"]
  ;; ["GLIMPSTRED" "G" "L" "IH" "M" "P" "S" "T" "R" "EH" "D"]
  ;; ["GLIMSTRED" "G" "L" "IH" "M" "S" "T" "R" "EH" "D"]
  )
