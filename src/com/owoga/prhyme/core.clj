(ns com.owoga.prhyme.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [com.owoga.prhyme.util :as u]
            [com.owoga.prhyme.syllabify :as s]))

(def dictionary
  (line-seq (io/reader (io/resource "cmudict_SPHINX_40"))))

(def words (map u/prepare-word dictionary))

(def popular
  (set (line-seq (io/reader (io/resource "popular.txt")))))

(def adverbs
  (set/intersection popular (set (line-seq (io/reader (io/resource "adverbs.txt"))))))

(def adjectives
  (set/intersection popular (set (line-seq (io/reader (io/resource "adjectives.txt"))))))

(def verbs
  (set/intersection popular (set (line-seq (io/reader (io/resource "verbs.txt"))))))

(def nouns
  (set/intersection popular (set (line-seq (io/reader (io/resource "nouns.txt"))))))

(defn words-by-rime* [words]
  (let [words-with-rime (->> words
                             (map rest)
                             (map s/syllabify)
                             (map #(map reverse %))
                             (map #(map
                                    (fn [syllable]
                                      (first (u/take-through u/vowel syllable))) %))
                             (map #(map reverse %))
                             (map reverse)
                             (map #(cons %1 %2) (map first words)))]
    (loop [by-rime {}
           words words-with-rime]
      (let [key (rest (first words))
            val (first (first words))
            existing (get-in by-rime key {:words '()})]
        (cond
          (empty? words) by-rime
          (empty? key) (recur by-rime (rest words))
          :else (recur (assoc-in by-rime
                                 (concat key [:words])
                                 (cons val (:words existing)))
                       (rest words)))))))

(def words-by-rime (words-by-rime* words))

(defn words-by-onset-nucleus* [words]
  (let [words-with-onset-nucleus (->> words
                                      (map rest)
                                      (map s/syllabify)
                                      (map #(map
                                             (fn [syllable]
                                               (first (u/take-through u/vowel syllable)))
                                             %))
                                      (map #(cons %1 %2) (map first words)))]
    (loop [by-onset {}
           words words-with-onset-nucleus]
      (let [key (rest (first words))
            val (ffirst words)
            existing (get-in by-onset key {:words '()})]
        (cond
          (empty? words) by-onset
          (empty? key) (recur by-onset (rest words))
          :else (recur (assoc-in by-onset
                                 (concat key [:words])
                                 (cons val (:words existing)))
                       (rest words)))))))

(def words-by-onset-nucleus (words-by-onset-nucleus* words))

(defn words-by-nucleus* [words]
  (let [words-with-nucleus (->> words
                                (map rest)
                                (map s/syllabify)
                                (map #(map
                                       (fn [syllable]
                                         (list
                                          (last
                                           (first (u/take-through u/vowel syllable)))))
                                       %))
                                (map #(cons %1 %2) (map first words)))]
    (loop [by-nucleus {}
           words words-with-nucleus]
      (let [key (rest (first words))
            val (ffirst words)
            existing (get-in by-nucleus key {:words '()})]
        (cond
          (empty? words) by-nucleus
          (empty? key) (recur by-nucleus (rest words))
          :else (recur (assoc-in by-nucleus
                                 (concat key [:words])
                                 (cons val (:words existing)))
                       (rest words)))))))

(def words-by-nucleus (words-by-nucleus* words))

(defn words-by-syllables* [words]
  (loop [by-syllables {}
         words words]
    (let [word (first words)
          syllable-count (count (s/syllabify word))
          entry (get by-syllables syllable-count '())]
      (cond
        (empty? words) by-syllables
        :else (recur (assoc by-syllables syllable-count (cons word entry))
                     (rest words))))))

(defn add-word-to-tree [tree word]
  (let [phonemes (reverse (rest word))]
    (assoc-in tree (conj (vec phonemes) :word) word)))

(defn build-tree [words]
  (reduce add-word-to-tree {} words))

(def phone-tree (build-tree words))

(defn rhyme-node [rhyme-tree phonemes]
  (let [phonemes (reverse phonemes)
        node (get-in rhyme-tree phonemes)]
    node))

(defn filter-to-syllable-count [n words]
  (filter (fn [word] (= n (count (s/syllabify (rest word))))) words))

(defn rimes [syllables]
  (->> syllables
       (map reverse)
       (map #(first (u/take-through u/vowel %)))
       (map reverse)))

(defn onset+nucleus [syllables]
  (->> syllables
       (map #(first (u/take-through u/vowel %)))))

(defn nucleus [syllables]
  (map #(list (last (first (u/take-through u/vowel %)))) syllables))

(defn rhyming-word
  "Simple lookup in data.
  Data is a tree of syllables to words.
  {(IH TH) {:words [WITH SMITH ...]
            (IY Z) {:words [SMITHIES PITHIES ...]
                    (OW) {:words [DITHIESOH ...]}]}}}"
  [data syllables]
  (get-in data (into '(:words) syllables)))

(defn rhyming-words
  "A rime is made of lists of syllables.
  Each of the following is a rime.
  ([(AH L)] [(IH TH) (IY Z)])
  ([(AH L)] [(IH TH)] [(IY Z)])
  The first represents rhymes of a single-syllable word
  followed by a two-syllable word. The second represents
  a rhyme of three single-syllable words.
  This returns the list of possible words that fulfill each
  collection of syllables. If no rhyme matches, nil is in that
  spot in the list."
  [data rime]
  (map (partial rhyming-word data) rime))

(defn all-rhymes [syllables]
  )
(defn prhyme [phones]
  (let [syllables (s/syllabify phones)
        rhymes (remove #(some nil? %)
                       (map (partial rhyming-words words-by-rime)
                            (u/partitions (rimes syllables))))
        onsets (remove #(some nil? %)
                       (map (partial rhyming-words words-by-onset-nucleus)
                            (u/partitions (onset+nucleus syllables))))
        nuclei (remove #(some nil? %)
                       (map (partial rhyming-words words-by-nucleus)
                            (u/partitions (nucleus (reverse syllables)))))
        popular-rhymes
        (let [popular (into #{} (map string/upper-case popular))]
          (remove #(some empty? %)
                  (map (fn [rhyme]
                         (map (fn [words-list]
                                (set/intersection popular (into #{} words-list)))
                              rhyme))
                       rhymes)))]
    {:rhymes popular-rhymes
     :onsets onsets
     :nuclei nuclei}))

(comment
  (take 10 popular)
  (prhyme ["R" "OY" "AH" "L"])
  (let [phones ["D" "R" "IY" "M" "S" "AE" "N" "D" "HH" "OW" "P" "S"]]
    (prhyme phones))
  (let [phones ["D" "R" "IY" "M" "S" "AE" "N" "D" "HH" "OW" "P" "S"]]
    (s/syllabify phones))
  (let [phones ["AE" "N" "D" "HH" "OW" "P" "S"]]
    (prhyme phones)
    (get-in words-by-nucleus (nucleus (s/syllabify phones)))
    (prhyme phones)
    (u/partitions (nucleus (s/syllabify phones)))
    (prhyme phones))
  (let [phones ["T" "AY" "M" "T" "UW" "TH" "IH" "NG" "K"]]
    (rimes (s/syllabify phones))
    (prhyme phones))
  (let [phones ["R" "UH" "N" "AW" "T" "AH" "F" "S" "L" "IY" "P"]]
    (prhyme phones)
    (s/syllabify phones))
  (let [phones ["S" "L" "IY" "P"]]
    (prhyme phones))
  (let [phones ["AH" "F"]]
    (prhyme phones))
  (let [phones ["D" "OW" "N" "T" "F" "UH" "K" "W" "IH" "TH" "M" "IY"]]
    (prhyme phones))
  (prhyme ["B" "Y" "UW" "T" "IH" "F" "AH" "L" "G" "ER" "L"])
  (let [r (rimes (s/syllabify ["R" "OY" "AH" "L" "W" "IH" "TH" "CH" "IY" "Z"]))]
    (remove #(some nil? %) (map rhyming-words (u/partitions r))))

  (let [r (rimes (s/syllabify ["B" "Y" "UW" "T" "IH" "F" "AH" "L" "G" "ER" "L"]))]
    (remove #(some nil? %) (map (partial rhyming-words words-by-rime) (u/partitions r))))

  (get
   (->> words
        (filter-to-syllable-count 1)
        (words-by-rime*))
   '("AA" "L"))
  )
(comment
  (-> (s/syllabify ["HH" "AA" "R" "D" "B" "AA" "L"])
      (rimes))
  ;; => (("AA" "R" "D") ("AA" "L")) 
  )

