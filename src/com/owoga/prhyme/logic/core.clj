(ns com.owoga.prhyme.logic.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.fd :as fd]
            [clojure.string :as string]
            [clojure.core.logic.pldb :as db]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.syllabify :as syllabify])
  (:use [clojure.core.logic]))

(defn productsumo [vars dens sum]
  (fresh [vhead vtail dhead dtail product run-sum]
    (conde
     [(emptyo vars) (== sum 0)]
     [(conso vhead vtail vars)
      (conso dhead dtail dens)
      (fd/* vhead dhead product)
      (fd/+ product run-sum sum)
      (productsumo vtail dtail run-sum)])))

(defn change [amount denoms]
  (let [dens (sort > denoms)
        vars (repeatedly (count dens) lvar)]
    (run* [q]
      (== q (zipmap dens vars))
      (everyg #(fd/in % (fd/interval 0 amount)) vars)
      (productsumo vars dens amount))))

(change 14 #{1 2 5 10})

(run* [s p o]
  (membero s [:mother :child])
  (membero o [:mother :child])
  (membero p [:loves :has])
  (!= s o))

(run* [s p o]
  (everyg #(membero % [:mother :child])
          [s o])
  (membero p [:loves :has])
  (distincto [s o]))

(def words-for-db dict/cmu-with-stress)

(defn phonemes-for-rhyme [word-phonemes]
  (->> word-phonemes
       reverse
       (split-with #(re-matches #".*[^1]" %))
       (#(concat (first %) (take 1 (second %))))))

(defn perfect-rhyme? [a b]
  (= (phonemes-for-rhyme a)
     (phonemes-for-rhyme b)))

(defn remove-consonants [w]
  (remove #(re-matches #".*[^\d]" %) w))

(defn perfect-vowel-rhyme? [a b]
  (= (remove-consonants (phonemes-for-rhyme a))
     (remove-consonants (phonemes-for-rhyme b))))

(db/db-rel
 word
 ^:index w
 ^:index syllable-count
 ^:index perfect-rhyme-phonemes)

(defn make-word [w]
  [word
   w
   (->> w
        rest
        (map #(string/replace % #"\d" ""))
        syllabify/syllabify
        count)
   (remove-consonants (phonemes-for-rhyme w))])

(remove-consonants (phonemes-for-rhyme ["po" "P" "AA1" "V" "ER0" "T" "IY0"]))
(def words
  (apply
   db/db
   (map make-word words-for-db)))

(db/with-db words
  (run 5 [sentence]
    (fresh [w s p
            w1 s1 p1]
      (== p '("IY0" "OW1"))
      (== sentence `(~w ~w1))
      (fd/+ s s1 7)
      (word w s p)
      (word w1 s1 p1)
      (== p p1)
      (!= w w1))))

(db/db-rel test-word w)
(def test-db (db/db [test-word "hello"]
                    [test-word "world"]))
(db/with-db test-db
  (run 5 [q]
    (test-word q)))

(db/db-rel test-word w ^:index pos)
(def test-db (db/db [test-word "hello" :greeting]
                    [test-word "world" :noun]))
(db/with-db test-db
  (run 5 [e a v]
    (test-word "hello" a)))

(comment
  (run* [q r]
    (membero q ["linux" "windows" "mac" "android" ""])
    (conda
     [fail]
     [(membero q ["linux" "windows"]) (== r 1)]
     [(== q "mac") (== r 2)]
     [succeed (== q "") (== r 3)]))
  ;; => (["linux" 1] ["windows" 1])
  (run* [q r]
    (conde
     [fail]
     [(membero q ["linux" "windows"]) (== r 1)]
     [(== q "mac") (== r 2)]
     [succeed (== q "") (== r 3)]))
  ;; => (["mac" 2] ["" 3] ["linux" 1] ["windows" 1]) 
  (run* [q r]
    (membero q ["linux" "windows" "mac" "android" ""])
    (condu
     [(membero q ["linux" "windows"]) (== r 1)]
     [(== q "mac") (== r 2)]
     [succeed (== q "") (== r 3)]))
  ;; => (["linux" 1] ["windows" 1] ["mac" 2] ["" 3])


  (run* [q r]
    (conde
     [(membero q ["linux" "windows"]) (membero r ["iOS" "android"])]
     [(== q "mac") (== r 2)]
     [succeed (== q "") (== r 3)]))
  ;; => (["linux" "iOS"] ["linux" "android"] ["windows" "iOS"] ["windows" "android"])
  (run* [q r]
    (condu
     [(== q "linux") (== r 1)]
     [(membero q ["linux" "windows"]) (membero r ["iOS" "android"])]
     [(== q "windows") (== r 2)]
     [(== q "linux") (== r 3)]))

  ;; => (["linux" "iOS"] ["linux" "android"])
  )

(let [people       (repeatedly 5 lvar)
      magazines    (repeatedly 5 lvar)
      cheeses      (repeatedly 5 lvar)
      reservations (repeatedly 5 lvar)
      answers (map list people magazines cheeses reservations)]
  (run* [q]
       (== q answers)))
(run* [q]
      (fd/in q (apply fd/domain (take 10 (iterate #(* % 2) 1)))))
