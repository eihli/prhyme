(ns com.owoga.prhyme.generation.weighted-selection
  "Utilities for generation of rhymes by assigning weights to a collection of
  words and randomly choosing words based on their weights.

  For example, we might want the first word of the rhyme of a target phrase to
  be selected from words that are highly weighted by their rhymeness. It's not
  as important for subsequent words to rhyme, so we might want subsequent words
  to be selected weighted by whether or not they are synonyms to some other
  word, by how many syllables they have, by whether they are associated with a
  markov value, etc..."
  (:require [com.owoga.prhyme.core :as prhyme]))

;;;; Signature of "weight adjustment" functions
;;;
;;; A weight adjustment function gets called every time a decision needs to be
;;; made for which word to choose, so I think it's important to not be too slow.
;;;
;;; The function will receive:
;;; - a collection of the words from which to weight.
;;; - the target phrase we're rhyming for
;;; - the current result that we have so far
;;;
;;; By default, the weights of the passed in words will probably be 1. If you
;;; want future weights to be adjusted from past weights, that's up to you.
;;;
;;; The target phrase will change as words are chosen for the result.
;;; A good and strategy will be to chop off syllables from the target phrase
;;; for each syllable of a matching result.

(defn adjust-for-markov
  "Works with a markov data structure that was generated taking into account
  sentence boundaries (represented as nils).

  A key in the markov structure of '(nil) would have a value that represents all
  words that have occurred in position 1 of the raw data.

  A key of '(nil \"foo\") would have a value that represents all words
  that occurred in position 2 following \"foo\"

  Automatically detects the order (window size) of the markov model. Does this
  by counting the length of the first key.
  "
  [markov percent]
  (let [markov-n (count (first (first markov)))]
    (fn [[words target result]]
      (let [key (let [k (map :norm-word (take markov-n result))]
                  (reverse
                   (if (> markov-n (count k))
                     (concat k (repeat (- markov-n (count k)) nil))
                     k)))
            markov-options (markov key)
            markov-option-avg (/ (apply + (vals markov-options))
                                 (max 1 (count markov-options)))]
        (if (nil? markov-options)
          [words target result]
          (let [[markovs non-markovs]
                ((juxt filter remove)
                 #(markov-options (:norm-word %))
                 words)
                weight-non-markovs (apply + (map :weight non-markovs))
                target-weight-markovs (- (/ weight-non-markovs (- 1 percent))
                                         weight-non-markovs)
                count-markovs (count markovs)
                adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
            [(concat
              (map
               (fn [m]
                 (let [option (markov-options (:norm-word m))]
                   (as-> m m
                     (assoc m :weight (* (/ option markov-option-avg) adjustment-markovs (:weight m)))
                     (assoc m :adjustment-for-markov (* (/ option markov-option-avg) adjustment-markovs)))))
               markovs)
              non-markovs)
             target
             result]))))))

(defn adjust-for-rhymes
  "Weights words by whether or not they rhyme.
  Once result contains something, becomes inactive. If you want to try to rhyme
  every selection, you'll need a different function. This one will only rhyme
  the tail of a target."
  [percent]
  (fn [[words target result]]
    (if (empty? result)
      (let [words-with-rime-count
            (map
             (fn [word]
               (assoc word :num-matching (if (prhyme/rhymes? target word) 1 0)))
             words)

            [rhyming non-rhyming]
            ((juxt filter remove)
             #(< 0 (:num-matching %))
             words-with-rime-count)

            weight-non-rhyming (apply + (map :weight non-rhyming))
            target-weight-rhyming (* 100 percent weight-non-rhyming)
            count-rhyming (count rhyming)
            adjustment-rhyming (if (= 0 count-rhyming) 1 (/ target-weight-rhyming count-rhyming))]
        [(concat
          (map
           (fn [word]
             (as-> word word
               (assoc word :weight (* adjustment-rhyming (:weight word)))
               (assoc word :adjustment-for-rimes adjustment-rhyming)))
           rhyming)
          non-rhyming)
         target
         result])
      [words target result])))
