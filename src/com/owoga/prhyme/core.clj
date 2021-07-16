(ns com.owoga.prhyme.core
  (:require [clojure.zip :as zip]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]
            [com.owoga.trie :as trie]
            [com.owoga.prhyme.util :as util]
            [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.stress-manip :as stress-manip]
            [com.owoga.phonetics.syllabify :as syllabify]
            [com.owoga.prhyme.util :as u]
            [com.owoga.prhyme.syllabify :as s]))

(def BOS "<s>")
(def EOS "</s>")

;;; Typical rhyme model (explanation of following 3 functions)
;;
;; In the typical theory of syllable structure, the general structure of a
;; syllable (σ) consists of three segments. These segments are grouped into two
;; components:
;;
;; Onset (ω)
;;     a consonant or consonant cluster, obligatory in some languages,
;;     optional or even restricted in others
;;    
;; Rime (ρ)
;;     right branch, contrasts with onset, splits into nucleus and coda
;;
;;     Nucleus (ν)
;;         a vowel or syllabic consonant, obligatory in most languages
;;     Coda (κ)
;;         consonant, optional in some languages, highly restricted or prohibited in others

(defn rimes [syllables]
  (->> syllables
       (map reverse)
       (map #(first (u/take-through phonetics/vowel %)))
       (map reverse)))

(defn onset+nucleus [syllables]
  (->> syllables
       (map #(first (u/take-through phonetics/vowel %)))))

(defn nucleus [syllables]
  (map #(list (last (first (u/take-through phonetics/vowel %)))) syllables))

(defn merge-phrase-words
  "Given multiple `Word`, like the words for 'well off', create a single `Word`
  that is syllabified as ('well' 'off') rather than as the combined ('weh'
  'loff'). Useful for finding single-word rhymes of multiple-word targets.

  An example: 'war on crime' -> 'turpentine'.
  As opposed to: 'war on crime' -> 'caw fawn lime'."
  [phrase phrase-words]
  (loop [merged (first phrase-words)
         phrase-words (rest phrase-words)]
    (cond
      (and (empty? phrase-words) (empty? merged)) nil
      (empty? phrase-words) (assoc merged :word phrase)
      :else (recur (-> merged
                       (assoc :syllables (concat (:syllables merged)
                                                 (:syllables (first phrase-words))))
                       (assoc :syllable-count (+ (:syllable-count merged)
                                                 (:syllable-count (first phrase-words))))
                       (assoc :rimes (concat (:rimes merged)
                                             (:rimes (first phrase-words))))
                       (assoc :onsets (concat (:onsets merged)
                                              (:onsets (first phrase-words))))
                       (assoc :nuclei (concat (:nuclei merged)
                                              (:nuclei (first phrase-words))))
                       (assoc :normalized-word (string/join
                                                " "
                                                [(:normalized-word merged)
                                                 (:normalized-word (first phrase-words))])))
                   (rest phrase-words)))))

(defrecord Word [word syllables syllable-count rimes onsets nuclei weight normalized-word])

(defn cmu->prhyme-word [word phonemes]
  (let [syllables (s/syllabify phonemes)
        rimes (rimes syllables)
        onsets (onset+nucleus syllables)
        nuclei (nucleus syllables)]
    (->Word
     word
     syllables
     (count syllables)
     rimes
     onsets
     nuclei
     1
     (-> word
         string/lower-case
         (string/replace #"\(\d+\)" "")))))

(defn cmu->prhyme [[word & phonemes]]
  (let [syllables (s/syllabify phonemes)
        rimes (rimes syllables)
        onsets (onset+nucleus syllables)
        nuclei (nucleus syllables)]
    {:word word
     :syllables syllables
     :syllable-count (count syllables)
     :rimes rimes
     :onsets onsets
     :nuclei nuclei
     :weight 1
     :normalized-word (-> word
                          string/lower-case
                          (string/replace #"\(\d+\)" ""))}))

(defn make-phrase->Word
  [phonemes-lookup]
  (fn [phrase]
    (->> (string/split phrase #"[ -]")
         (map
          (fn [phrase-word]
            (let [phonemes (or (phonemes-lookup phrase-word)
                               (u/get-phones phrase-word))]
              (cmu->prhyme-word phrase-word phonemes))))
         (merge-phrase-words phrase))))

(defn phrase->word
  "Given a word like 'well-off' or a phrase like 'war on poverty', return a Word
  that has the correct syllables, rimes, onsets, and nucleus. This way we can
  rhyme against phrases that aren't in the dictionary, as long as the words that
  make up the phrase are in the dictionary. Returns nil if the word is not in
  the dictionary."
  [words phrase]
  (let [word-set (into #{} (map :normalized-word words))]
    (->> (string/split phrase #"[ -]")
         (map (fn [phrase-word]
                (let [word (first (filter word-set phrase-word))]
                  (if (nil? word)
                    (cmu->prhyme (cons phrase-word (u/get-phones phrase-word)))
                    word))))
         (merge-phrase-words phrase))))

(defn phrase->perfect-rhyme-trie
  [words]
  (transduce
   (comp
    (map #(vector (map reverse (phonetics/get-phones %)) %)))
   (completing
    (fn [trie [lookups v]]
      (reduce
       (fn [trie lookup]
         (update trie lookup (fnil #(update % 1 inc) [v 0])))
       trie
       lookups)))
   (trie/make-trie)
   words))

(comment
  (let [trie (words->perfect-rhyme-trie ["dog" "hog" "bog" "hop"])]
    trie)
  ;; => {("G" "AA1" "B") ["bog" 1],
  ;;     ("G" "AA1" "HH") ["hog" 1],
  ;;     ,,,
  ;;     ("P" "AA1" "HH") ["hop" 1],
  ;;     ("P" "AA1") nil,
  ;;     ("P") nil}
  )

;;;; Flex rhymes maintain primary stress and ignore all other stress.
;;;;

(defn phrase->all-flex-rhyme-phones
  "Takes a space-seperated string of words
  and returns the concatenation of the words
  vowel phones.

  Returns them in reversed order so they
  are ready to be used in a lookup of a rhyme trie.

  Returns all possible pronunciations. For hog -> haog, haag.

  bog => ([[AA1] bog] [[AO1] bog])
  "
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (fn [word]
              (let [phones (phonetics/get-phones word)]
                (map #(vector % word) phones))))
       ;; Lots of nesting here.
       ;; We have phrase -> word pronunciations -> word pronunciation -> [phones word]
       ;; The rest will be easier if we get rid of a level of nesting
       ;; by mapcatting the cross product of pronunciations.
       (apply combinatorics/cartesian-product)
       ;; Now we have [phrases [pronunciations [[phones] word]]]
       (map (fn [pronunciations]
              (map (fn [[phones word]]
                     [(syllabify/syllabify phones) word])
                   pronunciations)))
       (map (fn [pronunciations]
              (map (fn [[syllables word]]
                     [(map (fn [phones]
                             (->> phones
                                  (filter
                                   (partial re-find #"\d"))
                                  (into [])))
                           syllables)
                      word])
                   pronunciations)))
       (map (fn [pronunciations]
              (reduce
               (fn [[syllable-vowel-sounds words] [syllables word]]
                 [(into
                   syllable-vowel-sounds
                   (map #(string/replace % #"[02-9]" "")
                        (reduce into [] syllables)))
                  (into words [word])])
               [[] []]
               pronunciations)))
       (map (fn [[phones words]]
              [phones (string/join " " words)]))))

(comment
  (phrase->all-flex-rhyme-phones "bog hopscotch")
  ;; => ([["AA1" "AA1" "AA"] "bog hopscotch"]
  ;;     [["AO1" "AA1" "AA"] "bog hopscotch"])


  (let [result (map phrase->all-flex-rhyme-phones ["dog" "hog" "hop" "bog hopscotch"])]
    result)

  )


(defn vowel?-ignoring-stress
  [phone]
  (phonetics/vowel (string/replace phone #"\d" "")))

(defn primary-stress?
  [phone]
  (re-find #"1" phone))

(defn last-primary-stress?
  [phones]
  (and (seq phones)
       (primary-stress? (first phones))
       (not-any? primary-stress? (rest phones))))

(defn take-vowels-and-tail-consonants
  "HOPSCOTCH -> AA1 AA2 CH

  Useful for finding rhymes where the last vowel and tailing consonants
  are the same and preceding vowels are the same."
  ([phones]
   (take-vowels-and-tail-consonants phones '() false))
  ([phones result taken-vowel?]
   (cond
     (empty? phones) result
     (and taken-vowel? (not (vowel?-ignoring-stress (last phones))))
     (recur (butlast phones) result taken-vowel?)
     (vowel?-ignoring-stress (last phones))
     (recur (butlast phones) (cons (last phones) result) true)
     :else (recur (butlast phones) (cons (last phones) result) taken-vowel?))))

(comment
  (take-vowels-and-tail-consonants (first (phonetics/get-phones "robot")))
  ;; => ("OW1" "AA2" "T")
  )

(defn remove-all-stress
  [phones]
  (mapv
   #(string/replace % #"\d" "")
   phones))

(defn remove-non-primary-stress
  [phones]
  (mapv
   #(string/replace % #"[02-9]" "")
   phones))

(defn phones->rhyme-vowels-sans-stress
  [phones]
  (remove-all-stress phones))

(defn phones->all-flex-rhyme-tailing-consonants-phones
  "Removes all but the tail consonants.
  Removes all non-primary stress from vowels."
  [phones]
  (->> phones
       take-vowels-and-tail-consonants
       remove-non-primary-stress))

(defn phrase->all-phones
  "Since each word in a phrase might have several different possible pronunciations,
  this function returns a cartesian product of all possible phones of the phrase.

  (phrase->all-phones hog in a bog)
  ;; => ([(HH AA1 G IH0 N AH0 B AA1 G) hog in a bog]
  ;;     [(HH AA1 G IH0 N AH0 B AO1 G) hog in a bog]
  ;;     [(HH AA1 G IH0 N EY1 B AA1 G) hog in a bog]
  ;;     [(HH AA1 G IH0 N EY1 B AO1 G) hog in a bog]
  ;;     [(HH AA1 G IH1 N AH0 B AA1 G) hog in a bog]
  ;;     [(HH AA1 G IH1 N AH0 B AO1 G) hog in a bog]
  ;;     [(HH AA1 G IH1 N EY1 B AA1 G) hog in a bog]
  ;;     [(HH AA1 G IH1 N EY1 B AO1 G) hog in a bog])
  "
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (fn [word]
              (let [phones (phonetics/get-phones word)]
                (map #(vector % word) phones))))
       ;; Lots of nesting here.
       ;; We have phrase -> word pronunciations -> word pronunciation -> [phones word]
       ;; The rest will be easier if we get rid of a level of nesting
       ;; by mapcatting the cross product of pronunciations.
       (apply combinatorics/cartesian-product)
       ;; Now we have [phrases [pronunciations [[phones] word]]]
       (map (partial apply map vector))
       (map (fn [[phones words]]
              [(apply concat phones)
               (string/join " " words)]))))

(comment
  (phrase->all-phones "hog in a bog")
  ;; => ([("HH" "AA1" "G" "IH0" "N" "AH0" "B" "AA1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH0" "N" "AH0" "B" "AO1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH0" "N" "EY1" "B" "AA1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH0" "N" "EY1" "B" "AO1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH1" "N" "AH0" "B" "AA1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH1" "N" "AH0" "B" "AO1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH1" "N" "EY1" "B" "AA1" "G") "hog in a bog"]
  ;;     [("HH" "AA1" "G" "IH1" "N" "EY1" "B" "AO1" "G") "hog in a bog"])
  )

(defn phrase->unstressed-vowels-and-tailing-consonants
  "an => ([[AE N] an] [[AH N] an])"
  [phrase]
  (->> phrase
       phrase->all-phones
       (map (fn [[phones word]]
              [(->> phones
                    take-vowels-and-tail-consonants
                    remove-all-stress)
               word]))
       distinct))

(comment
  (phrase->unstressed-vowels-and-tailing-consonants
   "an")
  ;; => ([["AE" "N"] "an"] [["AH" "N"] "an"])
  )

(defn phrase->all-flex-rhyme-tailing-consonants-phones
  "Takes a space-seperated string of words
  and returns the concatenation of the words
  vowel phones.

  Returns them in reversed order so they
  are ready to be used in a lookup of a rhyme trie.

  Returns all possible pronunciations. For hog -> haog, haag.

  ROBOT => '([(OW1 AA T) robot] [(OW1 AH T) robot])"
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (fn [word]
              (let [phones (phonetics/get-phones word)]
                (map #(vector % word) phones))))
       ;; Lots of nesting here.
       ;; We have phrase -> word pronunciations -> word pronunciation -> [phones word]
       ;; The rest will be easier if we get rid of a level of nesting
       ;; by mapcatting the cross product of pronunciations.
       (apply combinatorics/cartesian-product)
       ;; Now we have [phrases [pronunciations [[phones] word]]]
       (map (partial apply map vector))
       (map (fn [[phones words]]
              [(apply concat phones)
               (string/join " " words)]))
       (map #(update % 0 take-vowels-and-tail-consonants))
       (map #(update % 0 remove-non-primary-stress))))

(comment
  (phrase->all-flex-rhyme-tailing-consonants-phones "robot")

  ;; => ([("OW1" "AA" "T") "robot"] [("OW1" "AH" "T") "robot"])
  )


(defn words->rhyme-trie
  [rhyme-type-fn words]
  (->> words
       (mapcat rhyme-type-fn)
       (map #(update % 0 reverse))
       (reduce
        (fn [trie [phones word]]
          ;; Use a set? If rhyme-type-fn filters out
          ;; phones that make a word with different pronunciations
          ;; have the same phones, then the word will be duplicated.
          ;; Alternatively, place word and pronunciation in value of trie.
          (update trie phones (fnil conj #{}) word))
        (trie/make-trie))))


(comment
  (let [words ["tightnit" "tarpit"]
        trie (words->rhyme-trie phrase->all-flex-rhyme-tailing-consonants-phones words)]
    (->> (trie/lookup trie ["AA"]))
    trie)
  ;; => {("T" "IH1" "AA") ("tarpit"),
  ;;     ("T" "IH1" "AY1") ("tightnit"),
  ;;     ("T" "IH1") nil,
  ;;     ("T") nil}

  )



(defn phrase->flex-rhyme-phones
  "Takes a space-seperated string of words
  and returns the concatenation of the words
  vowel phones.

  Returns them in reversed order so they
  are ready to be used in a lookup of a rhyme trie.
  "
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (comp syllabify/syllabify first phonetics/get-phones))
       (map (partial reduce into []))
       (map #(filter (partial re-find #"\d") %))
       (flatten)
       (map #(string/replace % #"\d" ""))
       (reverse)))

(comment
  (phrase->flex-rhyme-phones "bother me");; => ("IY" "ER" "AA")
  (phrase->flex-rhyme-phones "hog")
  )


;;;; Utilities
;;
;;

(defn phrase->flex-rhyme-phones
  "Takes a space-seperated string of words
  and returns the concatenation of the words
  vowel phones.

  Returns them in reversed order so they
  are ready to be used in a lookup of a rhyme trie.
  "
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (comp syllabify/syllabify first phonetics/get-phones))
       (map (partial reduce into []))
       (map #(filter (partial re-find #"\d") %))
       (flatten)
       (map #(string/replace % #"\d" ""))
       (reverse)))

(defn take-words-amounting-to-at-least-n-syllables
  "This function is nice to grab the tail end of a sentence for making a good rhyme.
  If the sentence ends with a single-syllable word, like 'me', but a more
  interesting n-gram like 'bother me', then you might want to explore the rhymes
  available for the last N syllables. Sure, a word like 'poverty' would show up if you
  got all rhymes for 'me'. But you'd have to filter through a lot more less-than-great
  rhymes before you see it."
  [phrase n]
  (letfn [(phones [word]
            [word (first (phonetics/get-phones word))])
          (syllables [[word phones]]
            [word (syllabify/syllabify phones)])]
    (->> phrase
         (#(string/split % #" "))
         (map phones)
         (map syllables)
         (reduce
          (fn [result [word syllables]]
            (if (<= n (count (mapcat second result)))
              (reduced result)
              (conj result [word syllables])))
          [])
         (map first)
         (string/join " "))))

(comment
  (take-words-amounting-to-at-least-n-syllables
   "police can bother me" 3);; => "police can"
  (take-words-amounting-to-at-least-n-syllables
   "police can bother me" 4);; => "police can bother"
  )

(defn take-n-syllables
  "Returns the vowel sounds that make up the last n syllables.
  Doesn't return stress."
  [phrase n]
  (if (string? phrase)
    (->> phrase
         (phrase->flex-rhyme-phones)
         (take n)
         (reverse))
    (take-last n phrase)))

(comment
  (take-n-syllables "bother me" 2);; => ("ER" "IY")
  )

(defn count-syllables-of-phrase
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map phonetics/get-phones)
       (map first)
       (mapcat syllabify/syllabify)
       (remove empty?) ;; Handle empty string.
       count))

(comment
  (count-syllables-of-phrase "police can bother me") ;; => 6
  (count-syllables-of-phrase "to be, or not... to be");; => 6
  (count-syllables-of-phrase "");; => 1
  )

(defn words-by-rime* [words]
  (let [words-with-rime (->> words
                             (map rest)
                             (map s/syllabify)
                             (map #(map reverse %))
                             (map #(map
                                    (fn [syllable]
                                      (first (u/take-through phonetics/vowel syllable))) %))
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

(defn prhyme-words-by-rime* [words]
  (loop [by-rime {}
         words words]
    (let [key (reverse (:rimes (first words)))
          val (first words)
          existing (get-in by-rime key {:words '()})]
      (cond
        (empty? words) by-rime
        :else (recur (assoc-in by-rime
                               (concat key [:words])
                               (cons val (:words existing)))
                     (rest words))))))

(defn words-by-onset-nucleus* [words]
  (let [words-with-onset-nucleus (->> words
                                      (map rest)
                                      (map s/syllabify)
                                      (map #(map
                                             (fn [syllable]
                                               (first (u/take-through phonetics/vowel syllable)))
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

(defn words-by-nucleus* [words]
  (let [words-with-nucleus (->> words
                                (map rest)
                                (map s/syllabify)
                                (map #(map
                                       (fn [syllable]
                                         (list
                                          (last
                                           (first (u/take-through phonetics/vowel syllable)))))
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

(defn rhyme-node [rhyme-tree phonemes]
  (let [phonemes (reverse phonemes)
        node (get-in rhyme-tree phonemes)]
    node))

(defn filter-to-syllable-count [n words]
  (filter (fn [word] (= n (count (s/syllabify (rest word))))) words))

(defn rhymes?
  "What does it mean for something to rhyme?"
  [a b]
  (cond
    (and (= 1 (count (last (:rimes a))))
         (= 1 (count (last (:rimes b))))
         (or (= (last (:rimes a)) '("ER"))
             (= (last (:rimes a)) '("AA"))
             (= (last (:rimes a)) '("AE"))
             (= (last (:rimes a)) '("AO"))
             (= (last (:rimes a)) '("AW"))
             (= (last (:rimes a)) '("EH"))
             (= (last (:rimes a)) '("IH"))
             (= (last (:rimes a)) '("UH"))
             (= (last (:rimes a)) '("AH"))))
    (= (list (first (take-last 2 (:nuclei a)))
             (last (:onsets a)))
       (list (first (take-last 2 (:nuclei b)))
             (last (:onsets b))))

    (and (= 1 (count (last (:rimes a))))
         (= 1 (count (last (:rimes b)))))
    (= (last (:onsets a)) (last (:onsets b)))

    :else (= (last (:rimes a)) (last (:rimes b)))))

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

(defn flatten-node [node]
  (let [zipper (zip/zipper
                (fn branch? [node]
                  (or (map? node) (map? (nth node 1))))
                (fn children [node]
                  (seq (if (map? node) node (nth node 1))))
                (fn make-node [node children]
                  (if (map? node)
                    (into {} children)
                    (assoc node 1 (into {} children))))
                node)]
    (->> zipper
         (iterate zip/next)
         (take-while #(not (zip/end? %)))
         (drop 1)
         (map zip/node)
         (map #(apply hash-map %))
         (map :words)
         (remove nil?)
         flatten)))

(defn node-merge [result-value latter-value]
  (cond
    (map? result-value)
    (merge-with node-merge result-value latter-value )

    :else (concat result-value latter-value)))

(comment
  (node-merge {:b 2 :c [1]} {:c [2]})

  (let [m1 {:a {:a1 {:a2 2 :a3 4 :a4 {:a6 7 :a5 5}}}}
        m2 {:a {:a1 {:a3 3 :a2 99 :a4 {:a5 195}}} :b 4}]
    #_(merge-with
     (fn [& maps]
       (apply merge-with merge maps))
     m1 m2)
    (deep-merge m1 m2))

  (let [phones ["D" "R" "IY" "M" "S" "AE" "N" "D" "HH" "OW" "P" "S"]]
    (s/syllabify phones))
  (let [r (rimes (s/syllabify ["R" "OY" "AH" "L" "W" "IH" "TH" "CH" "IY" "Z"]))]
    (remove #(some nil? %) (map rhyming-words (u/partitions r))))

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

(defn perfect-rhyme
  [phones]
  (->> phones
       reverse
       (util/take-through stress-manip/primary-stress?)
       first
       reverse
       (#(cons (first %)
               (stress-manip/remove-any-stress-signifiers (rest %))))))

(comment
  (perfect-rhyme (first (phonetics/get-phones "technology")))
  ;; => ("AA1" "L" "AH" "JH" "IY")
  )

(defn perfect-rhyme-sans-consonants
  [phones]
  (->> phones
       perfect-rhyme
       (remove phonetics/consonant)))

(comment
  (perfect-rhyme-sans-consonants (first (phonetics/get-phones "technology")))
  ;; => ("AA1" "AH" "IY")
  )

(defn perfect-rhyme?
  [phones1 phones2]
  (apply = (map perfect-rhyme [phones1 phones2])))

(defn perfect-rhyme-sans-consonants?
  [phones1 phones2]
  (apply = (map perfect-rhyme-sans-consonants [phones1 phones2])))

(comment
  (apply perfect-rhyme? (map (comp first phonetics/get-phones) ["technology" "ecology"]));; => true
  (apply perfect-rhyme? (map (comp first phonetics/get-phones) ["technology" "economy"]));; => false
  (apply perfect-rhyme-sans-consonants? (map (comp first phonetics/get-phones) ["technology" "economy"]));; => true
  (apply perfect-rhyme-sans-consonants? (map (comp first phonetics/get-phones) ["technology" "trilogy"]));; => false
  (apply perfect-rhyme? (map (comp first phonetics/get-phones) ["bother me" "poverty"]))
  (apply perfect-rhyme? (map (comp first phonetics/phrase-phones) ["bother me" "poverty"]))
  (phonetics/phrase-phones "bother me");; => [["B" "AA1" "DH" "ER0" "M" "IY1"]]
  (phonetics/phrase-phones "poverty");; => [["P" "AA1" "V" "ER0" "T" "IY0"]]
  )

(defn number-of-matching-vowels-with-stress
  [phones1 phones2]
  (let [[vowels1 vowels2] (map (partial filter phonetics/vowel?) [phones1 phones2])]
    (->> [vowels1 vowels2]
         (map reverse)
         (apply map vector)
         (filter (partial apply =))
         (filter (comp (partial re-find #"1") first))
         count)))

(comment
  (apply
   number-of-matching-vowels-with-stress
   (map first (map phonetics/get-phones ["technology" "ecology"])))

  (apply
   number-of-matching-vowels-with-stress
   (map first (map phonetics/get-phones ["biology" "ecology"])))

  )

(defn number-of-matching-vowels-any-stress
  [phones1 phones2]
  (let [[vowels1 vowels2] (map (partial filter phonetics/vowel?) [phones1 phones2])]
    (->> [vowels1 vowels2]
         (map (partial map phonetics/remove-stress))
         (map reverse)
         (apply map vector)
         (filter (partial apply =))
         count)))

(comment
  (apply
   number-of-matching-vowels-any-stress
   (map first (map phonetics/get-phones ["economy" "ecology"])))

  (apply
   number-of-matching-vowels-any-stress
   (map first (map phonetics/get-phones ["biology" "ecology"])))
  )

(defn same-number-of-syllables?
  [phones1 phones2]
  (apply = (map (comp count syllabify/syllabify) [phones1 phones2])))

(comment
  (apply
   same-number-of-syllables?
   (map first (map phonetics/get-phones ["economy" "ecology"])))

  (apply
   same-number-of-syllables?
   (map first (map phonetics/get-phones ["numerology" "ecology"])))
  )

(defn quality-of-rhyme-phones
  "Points for:
  - Perfect rhyme
  - Perfect rhyme sans consonants
  - Number of matching stressed vowels
  - Number of matching any-stressed vowels
  - Same number of syllables
  "
  [phones1 phones2]
  (let [perfect? (if (perfect-rhyme? phones1 phones2) 1 0)
        perfect-sans-consonants? (if (perfect-rhyme-sans-consonants? phones1 phones2) 1 0)
        num-matching-stressed (number-of-matching-vowels-with-stress phones1 phones2)
        num-matching-any-stress (number-of-matching-vowels-any-stress phones1 phones2)
        same-number-of-syllables (if (same-number-of-syllables? phones1 phones2) 1 0)]
    (+ perfect?
       perfect-sans-consonants?
       num-matching-stressed
       num-matching-any-stress
       same-number-of-syllables)))

(comment
  (->> [["economy" "ecology"]
        ["biology" "ecology"]
        ["bother me" "poverty"]
        ["property" "properly"]
        ["bother me" "invincibility"]
        ["invincibility" "bother me"]]
       (map (partial map (comp first phonetics/phrase-phones)))
       (map (partial apply quality-of-rhyme-phones)))

  (phonetics/phrase-phones "bother me")
  (phonetics/phrase-phones "invincibility")

  (let [phones1 ["B" "AA1" "DH" "ER0" "M" "IY1"]
        phones2 ["IH2" "N" "V" "IH2" "N" "S" "AH0" "B" "IH1" "L" "IH0" "T" "IY0"]]
    (perfect-rhyme-sans-consonants? phones1 phones2))

  )
