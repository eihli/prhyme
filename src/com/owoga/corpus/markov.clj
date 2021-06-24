(ns com.owoga.corpus.markov
  (:require [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data-transform :as data-transform]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.syllabify :as syllabify]
            [taoensso.nippy :as nippy]
            [clojure.math.combinatorics :as combinatorics]
            [com.owoga.prhyme.nlp.core :as nlp]))

(defrecord RhymeSet [phones words])

; Since we're dealing with phonetics, a word consists of the spelling as well as all possible phonetic pronunciations.
(defrecord UnpronouncedWord [word pronunciations])

(defrecord PronouncedWord [word pronunciation])

(defn all-pronunciations
  [words]
  (let [pronunciations (apply combinatorics/cartesian-product (map :pronunciations words))]
    (map
     (fn [pronunciation]
       (map ->PronouncedWord (map :word words) pronunciation))
     pronunciations)))

(let [input-words ["bog" "hog"]
      words (map (fn [word] (->UnpronouncedWord word (phonetics/get-phones word))) input-words)
      pronunciations (all-pronunciations words)]
  pronunciations)

(defn clean-text [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]" "")))

(defn xf-file-seq [start end]
  (comp (remove #(.isDirectory %))
        (drop start)
        (take end)))

;;;; Efficient Tries with Database
;; To make a more memory-efficient trie, and
;; to more easily support the conversion of a trie
;; to a tightly packed trie, convert all keys and values
;; to integers.
;;
;; Also, create a database to map integer IDs back to
;; their string values and string values to integer IDs.

;;;; The difference between a forwards and a backwards
;;   markov is that the backwards markov has its tokens
;;   reversed and has the </s> tokens padded by a number
;;   equal to the markov rank (rather than the <s> padded).

(defn file-seq->markov-trie
  "For forwards markov."
  [database files n m]
  (transduce
   (comp
    (map slurp)
    (map #(string/split % #"[\n+\?\.]"))
    (map (partial transduce data-transform/xf-tokenize conj))
    (map (partial transduce data-transform/xf-filter-english conj))
    (map (partial remove empty?))
    (map (partial into [] (data-transform/xf-pad-tokens (dec m) "<s>" 1 "</s>")))
    (map (partial mapcat (partial data-transform/n-to-m-partitions n (inc m))))
    (mapcat (partial mapv (data-transform/make-database-processor database))))
   (completing
    (fn [trie lookup]
      (update trie lookup (fnil #(update % 1 inc) [lookup 0]))))
   (trie/make-trie)
   files))

(comment
  (let [files (->> "dark-corpus"
                   io/file
                   file-seq
                   (eduction (xf-file-seq 501 2)))
        database (atom {:next-id 1})
        trie (file-seq->markov-trie database files 1 3)]
    [(take 20 trie)
     (count trie)
     (get @database 1)
     (take 10 @database)])
  )

(defn file-seq->backwards-markov-trie
  "For backwards markov."
  [database files n m]
  (transduce
   (comp
    (map slurp)
    (map #(string/split % #"[\n+\?\.]"))
    (map (partial transduce data-transform/xf-tokenize conj))
    (map (partial transduce data-transform/xf-filter-english conj))
    (map (partial remove empty?))
    (map (partial map (comp vec reverse)))
    ;; xf-pad-tokens needs vectors to properly pad due to `into`
    (map (partial into [] (data-transform/xf-pad-tokens (dec m) "</s>" 1 "<s>")))
    (map (partial mapcat (partial data-transform/n-to-m-partitions n (inc m))))
    (mapcat (partial mapv (data-transform/make-database-processor database))))
   (completing
    (fn [trie lookup]
      (update trie lookup (fnil #(update % 1 inc) [(peek lookup) 0]))))
   (trie/make-trie)
   files))

(comment
  (let [files (->> "dark-corpus"
                   io/file
                   file-seq
                   (eduction (xf-file-seq 501 2)))
        database (atom {:next-id 1})
        trie (file-seq->backwards-markov-trie database files 1 3)]
    [(take 5 trie)
     (->> (trie/children-at-depth trie 0 1)
          (map
           (fn [[k v]]
             [(map @database k) v]))
          (sort-by (comp - second second))
          (take 5))])
  ;; => [([(1 1 2) [[1 1 2] 55]]
  ;;      [(1 1) [[1 1] 55]]
  ;;      [(1 2 3) [[1 2 3] 1]]
  ;;      [(1 2 7) [[1 2 7] 1]]
  ;;      [(1 2 12) [[1 2 12] 1]])
  ;;     ([("</s>") [[1] 110]]
  ;;      [("<s>") [[2] 55]]
  ;;      [(",") [[19] 14]]
  ;;      [("you") [[63] 11]]
  ;;      [("to") [[15] 7]])]
  )


;;;; Packing the trie into a small memory footprint

(defn encode-fn [v]
  (let [[value count] (if (seqable? v) v [nil nil])]
    (if (nil? value)
      (encoding/encode 0)
      (byte-array
       (concat (encoding/encode value)
               (encoding/encode count))))))

(defn decode-fn [db]
  (fn [byte-buffer]
    (let [value (encoding/decode byte-buffer)]
      (if (zero? value)
        nil
        [value (encoding/decode byte-buffer)]))))

(defn save-tightly-packed-trie
  [trie database filepath]
  (let [tightly-packed-trie
        (tpt/tightly-packed-trie
         trie
         encode-fn
         (decode-fn @database))]
    (tpt/save-tightly-packed-trie-to-file
     filepath
     tightly-packed-trie)))

(defn load-tightly-packed-trie
  [filepath database]
  (tpt/load-tightly-packed-trie-from-file
   filepath
   (decode-fn @database)))


;;;; Training

(defn train-backwards
  "For building lines backwards so they can be seeded with a target rhyme."
  [files n m trie-filepath database-filepath tightly-packed-trie-filepath]
  (let [database (atom {:next-id 1})
        trie (file-seq->backwards-markov-trie database files n m)]
    (nippy/freeze-to-file trie-filepath (seq trie))
    (println "Froze" trie-filepath)
    (nippy/freeze-to-file database-filepath @database)
    (println "Froze" database-filepath)
    (save-tightly-packed-trie trie database tightly-packed-trie-filepath)
    (let [loaded-trie (->> trie-filepath
                           nippy/thaw-from-file
                           (into (trie/make-trie)))
          loaded-db (->> database-filepath
                         nippy/thaw-from-file)
          loaded-tightly-packed-trie (tpt/load-tightly-packed-trie-from-file
                                      tightly-packed-trie-filepath
                                      (decode-fn loaded-db))]
      (println "Loaded trie:" (take 5 loaded-trie))
      (println "Loaded database:" (take 5 loaded-db))
      (println "Loaded tightly-packed-trie:" (take 5 loaded-tightly-packed-trie))
      (println "Successfully loaded trie and database."))))

(comment
  (time
   (let [files (->> "dark-corpus"
                    io/file
                    file-seq
                    (eduction (xf-file-seq 0 250000)))
         [trie database] (train-backwards
                          files
                          1
                          5
                          "/home/eihli/.models/markov-trie-4-gram-backwards.bin"
                          "/home/eihli/.models/markov-database-4-gram-backwards.bin"
                          "/home/eihli/.models/markov-tightly-packed-trie-4-gram-backwards.bin")]))

  (time
   (def markov-trie (into (trie/make-trie) (nippy/thaw-from-file "/home/eihli/.models/markov-trie-4-gram-backwards.bin"))))
  (time
   (def database (nippy/thaw-from-file "/home/eihli/.models/markov-database-4-gram-backwards.bin")))
  (time
   (def markov-tight-trie
     (tpt/load-tightly-packed-trie-from-file
      "/home/eihli/.models/markov-tightly-packed-trie-4-gram-backwards.bin"
      (decode-fn database))))
  (take 20 markov-tight-trie)
  )


(defn gen-rhyme-model
  [rhyme-type-fn database database-filepath]
  (let [words (filter string? (keys @database))
        rhyme-trie (prhyme/words->rhyme-trie rhyme-type-fn words)]
    (nippy/freeze-to-file database-filepath (seq rhyme-trie))
    (let [loaded-trie (->> (nippy/thaw-from-file database-filepath)
                           (into (trie/make-trie)))]
      (println "Successfully loaded rhyme model")
      (println (take 5 loaded-trie)))))

(comment
  (time
   (let [database (atom (nippy/thaw-from-file "/home/eihli/.models/markov-database-4-gram-backwards.bin"))]
     (gen-rhyme-model
      prhyme/phrase->all-flex-rhyme-tailing-consonants-phones
      database
      "/home/eihli/.models/rhyme-trie-primary-stressed-vowels-and-trailing-consonants.bin")
     (gen-rhyme-model
      prhyme/phrase->unstressed-vowels-and-tailing-consonants
      database
      "/home/eihli/.models/rhyme-trie-unstressed-vowels-and-trailing-consonants.bin")))

  (def rhyme-trie
    (into
     (trie/make-trie)
     (nippy/thaw-from-file
      "/home/eihli/.models/rhyme-trie-unstressed-vowels-and-trailing-consonants.bin")))

  )


(defn choice->n-gram
  [{:keys [database]} choice]
  (map database (first choice)))

(defn weighted-selection-from-choices
  [choices]
  (math/weighted-selection
   (comp second second)
   choices))

(ns-unmap (find-ns 'com.owoga.corpus.markov) 'rhyme-choices)

(defmulti rhyme-choices
  "Returns a list of words that end with the same phones
  as the target. If the target is a string, converts the string to phones."
  (fn [trie target] (class target)))

(defmethod rhyme-choices String
  [trie phrase]
  (let [phones (phonetics/get-phones phrase)]
    (->> phones
         (map reverse)
         (mapcat (partial rhyme-choices trie))
         (remove empty?))))

(defmethod rhyme-choices :default
  [trie phones]
  (->> (trie/lookup trie phones)
       (remove (comp nil? second))
       (map #(update % 0 into (reverse phones)))
       (map #(update % 0 vec))))

(comment
  (->>  (prhyme/phrase->all-flex-rhyme-tailing-consonants-phones "bog")
        (map first)
        (map reverse)
        (mapcat (partial rhyme-choices rhyme-trie)))

  (let [rhyme-trie (trie/make-trie ["G" "AA1" "B"] "bog" ["G" "AO1" "B"] "bog"
                                   ["T" "AA1" "H"] "hot" ["G" "AO1" "F"] "fog")]
    [(rhyme-choices rhyme-trie ["G" "AO1"])
     (rhyme-choices rhyme-trie "fog")
     (rhyme-choices rhyme-trie "bog")])
  ;; => [([("G" "AO1" "B") "bog"] [("G" "AO1" "F") "fog"])
  ;;     ([("G" "AO1" "F") "fog"])
  ;;     ([("G" "AA1" "B") "bog"] [("G" "AO1" "B") "bog"])]

  (->> (rhyme-choices-walking-target-rhyme
        rhyme-trie
        ["N" "AH1" "F"]
        identity)
       rand-nth
       ((fn [[phones words]]
          [[phones] (rand-nth (vec words))])))

  )

(defn rhyme-choices-walking-target-rhyme-with-stop
  "All target rhymes need to be in phone form.

  `target-rhyme`: [N UH1 F]
  If we try to turn string form into phone form,
  we'd sometimes be forced to deal with multiple pronunciations.
  By only handling phone form here, the caller can handle multiple pronunciations.
  Makes for a cleaner API.

  `words-fn` gets passed the result of `rhyme-choices` which has this structures
  ([(G AO1 B) bog] [(G AO1 F) fog])

  `stop?` gets passed the remaining target-rhyme phones and the current choices.
  "
  ([trie stop? target-rhyme]
   (rhyme-choices-walking-target-rhyme-with-stop
    trie
    stop?
    target-rhyme
    identity))
  ([trie stop? target-rhyme words-fn]
   (loop [target-rhyme target-rhyme
          result []]
     (let [choices (words-fn (rhyme-choices trie target-rhyme))]
       (if (stop? target-rhyme choices)
         (into result choices)
         (recur (butlast target-rhyme)
                (into result choices)))))))

(defn rhyme-choices-walking-target-rhyme
  "All target rhymes need to be in phone form.

  `target-rhyme`: [N UH1 F]
  If we try to turn string form into phone form,
  we'd sometimes be forced to deal with multiple pronunciations.
  By only handling phone form here, the caller can handle multiple pronunciations.
  Makes for a cleaner API.

  `words-fn` gets passed the result of `rhyme-choices` which has this structures
  ([(G AO1 B) bog] [(G AO1 F) fog])
  "
  ([trie target-rhyme]
   (rhyme-choices-walking-target-rhyme
    trie
    target-rhyme
    identity))
  ([trie target-rhyme words-fn]
   (loop [target-rhyme target-rhyme
          result []]
     (let [choices (words-fn (rhyme-choices trie target-rhyme))]
       (if (or (empty? target-rhyme)
               (and (not-empty choices)
                    (prhyme/last-primary-stress? (reverse target-rhyme))))
         (into result choices)
         (recur (butlast target-rhyme)
                (into result choices)))))))

(comment
  (let [words ["bloodclot" "woodrot" "moonshot" "dot" "bog" "pat" "pot" "lot"]
        phones (mapcat prhyme/phrase->all-flex-rhyme-tailing-consonants-phones words)
        rhyme-trie (reduce
                    (fn [trie [phones word]]
                      (update trie phones (fnil conj #{}) [phones word]))
                    (trie/make-trie)
                    (map #(update % 0 reverse) phones))]
    (rhyme-choices-walking-target-rhyme
     rhyme-trie
     (reverse (first (first (prhyme/phrase->all-flex-rhyme-tailing-consonants-phones "tight knot"))))))
  ;; => [[("T" "AA1" "AH1") #{[("T" "AA1" "AH1") "bloodclot"]}]
  ;;     [("T" "AA1" "UH1") #{[("T" "AA1" "UH1") "woodrot"]}]
  ;;     [("T" "AA1" "UW1") #{[("T" "AA1" "UW1") "moonshot"]}]
  ;;     [("T" "AA1")
  ;;      #{[("T" "AA1") "dot"] [("T" "AA1") "pot"] [("T" "AA1") "lot"]}]]

  (rhyme-choices-walking-target-rhyme
   rhyme-trie
   (reverse '("UH1" "AA1" "R" "T")))

  )

(defn get-next-markov
  "Weighted selection from markov model with backoff.
  Expects markov key/values to be [k1 k2 k3] [<value> freq]."
  ([markov-trie seed]
   (get-next-markov markov-trie seed identity))
  ([markov-trie seed process-children-fn]
   (let [seed (take-last 3 seed)
         node (trie/lookup markov-trie seed)
         children (and node
                       (->> node
                            trie/children
                            process-children-fn
                            (map (fn [^com.owoga.trie.ITrie child]
                                        ; Get key and frequency of each child
                                   [(.key child)
                                    (get child [])]))
                            (remove (comp nil? second))))]
     (cond
                                        ; If we've never seen this n-gram, fallback to n-1-gram
       (nil? node) (recur markov-trie (rest seed) process-children-fn)
       (seq children)
       (if (< (rand) (/ (apply max (map (comp second second) children))
                        (apply + (map (comp second second) children))))
         (recur markov-trie (rest seed) process-children-fn)
         (first (math/weighted-selection (comp second second) children)))
       (> (count seed) 0)
       (recur markov-trie (rest seed) process-children-fn)
                                        ; If we have a node but no children, or if we don't have a seed,
                                        ; we don't know how to handle that situation.
       :else (throw (Exception. "Error"))))))

(defn normalized-frequencies
  [coll]
  (let [freqs (frequencies coll)
        total (apply + (vals freqs))]
    (reduce
     (fn [freqs [k v]]
       (assoc freqs k (float (/ v total))))
     {}
     freqs)))

(comment
  (let [markov-trie (trie/make-trie ["see" "dog" "run"] [["see" "dog" "run"] 1]
                                    ["see" "cat" "eat"] [["see" "cat" "eat"] 1]
                                    ["see" "dog"] [["see" "dog"] 1]
                                    ["dog" "eat"] [["dog" "eat"] 1]
                                    ["see" "cat"] [["see" "cat"] 1]
                                    ["cat" "eat"] [["cat" "eat"] 1]
                                    ["see"] [["see"] 2]
                                    ["dog"] [["dog"] 1]
                                    ["run"] [["run"] 1]
                                    ["cat"] [["cat"] 1]
                                    ["eat"] [["eat"] 1])
        seed ["see"]
        node (trie/lookup markov-trie seed)]
    [(normalized-frequencies
      (repeatedly 1000 #(get-next-markov markov-trie ["see"])))
     (normalized-frequencies
      (repeatedly 1000 #(get-next-markov markov-trie ["see dog"])))])
  ;; => [{"cat" 0.336, "dog" 0.308, "eat" 0.088, "see" 0.178, "run" 0.09}
  ;;     {"cat" 0.141, "dog" 0.176, "see" 0.32, "eat" 0.187, "run" 0.176}]
  )

(defn generate-n-syllable-sentence-rhyming-with
  [markov-trie
   rhyme-trie
   target-rhyme
   n-gram-rank
   target-rhyme-syllable-count
   target-sentence-syllable-count]
  (let [rhyme (->> (rhyme-choices-walking-target-rhyme rhyme-trie target-rhyme)
                   rand-nth
                   ((fn [[phones words]]
                      [[phones] (rand-nth (vec words))])))]
    (loop [phrase [rhyme]]
      (if (or (= prhyme/BOS (second (peek phrase)))
              (<= target-sentence-syllable-count
                  (prhyme/count-syllables-of-phrase
                   (string/join " " (map second phrase)))))
        phrase
        (recur
         (conj
          phrase
          (let [word (get-next-markov
                      markov-trie
                      (into (mapv second phrase)
                            (vec (repeat (dec n-gram-rank) prhyme/EOS))))]
            [(phonetics/get-phones word) word])))))))

(comment
  (let [words  [["see" "dog" "run"] [["see" "dog" "run"] 1]
                ["see" "cat" "eat"] [["see" "cat" "eat"] 1]
                ["dog" "has" "fun"] [["dog" "has" "fun"] 1]
                ["see" "dog"] [["see" "dog"] 1]
                ["dog" "eat"] [["dog" "eat"] 1]
                ["see" "cat"] [["see" "cat"] 1]
                ["cat" "eat"] [["cat" "eat"] 1]
                ["has" "fun"] [["has" "fun"] 1]
                ["see"] [["see"] 2]
                ["dog"] [["dog"] 2]
                ["run"] [["run"] 1]
                ["cat"] [["cat"] 1]
                ["eat"] [["eat"] 1]
                ["has"] [["has"] 1]
                ["fun"] [["fun"] 1]]
        words (map
               (fn [[k [v f]]]
                 [(reverse k) [(reverse v) f]])
               (partition 2 words))
        markov-trie (into (trie/make-trie) words)
        words ["see" "dog" "run" "cat" "eat" "has" "fun"]
        rhyme-trie (prhyme/words->rhyme-trie
                    prhyme/phrase->all-flex-rhyme-tailing-consonants-phones
                    words)
        target-rhyme ["N" "AH1" "F"]]
    (sort-by
     (comp - second)
     (normalized-frequencies
      (repeatedly
       10
       #(map
         second
         (generate-n-syllable-sentence-rhyming-with
          markov-trie
          rhyme-trie
          target-rhyme
          3
          1
          3))))))
  ;; => ([("fun" "see" "see") 0.027]
  ;;     [("fun" "dog" "see") 0.026]
  ;;     [("fun" "see" "dog") 0.026]
  ;;     ,,,
  ;;     [("fun" "run" "has") 0.001])

  )

(defn tightly-generate-n-syllable-sentence
  "It's difficult to mix a tight trie with rhymes. You need
  to convert ids using the database.

  This is going to generate sentences backwards.

  Generates the following structure:

    [[[[S K AY1]] sky]
     [[[DH AH0] [DH AH1] [DH IY0]] the]
     [[[K R AE1 K S]] cracks]
     [[[G R AW1 N D]] ground]
     [[[DH AH0] [DH AH1] [DH IY0]] the]
     [[[T UW1] [T IH0] [T AH0]] to]
     [[[K IH1 NG D AH0 M]] kingdom]
     [[[DH AH0] [DH AH1] [DH IY0]] the]
     [[[D IH0 S T R OY1]] destroy]]
  "
  ([database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count]
   (tightly-generate-n-syllable-sentence
    database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count
    identity))
  ([database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count
    process-markov-children]
   (let [eos (database prhyme/EOS)
         bos (database prhyme/BOS)]
     (loop [phrase []]
       (if (<= target-sentence-syllable-count
               (prhyme/count-syllables-of-phrase
                (string/join " " (map second phrase))))
         phrase
         (recur
          (conj
           phrase
           (let [word (database
                       (get-next-markov
                        markov-trie
                        ; Pad sentence with eos markers since we're working backwards
                        (into (vec (repeat (dec n-gram-rank) eos))
                              (mapv (comp database second) phrase))
                        process-markov-children))]
             [(phonetics/get-phones word) word]))))))))

(comment
  (tightly-generate-n-syllable-sentence
   database
   markov-trie
   3
   10
   (fn [children]
     (remove
      (fn [child]
        (let [lookup (.key child)
              [word freq] (get child [])]
          (#{(database prhyme/EOS) (database prhyme/BOS)} word)))
      children)))
  ;; [[[["HH" "ER1" "T" "S"]] "hurts"]
  ;;  [[["IH1" "T"] ["IH0" "T"]] "it"]
  ;;  [[["AH0" "N" "D"] ["AE1" "N" "D"]] "and"]
  ;;  [[["F" "EY1" "S"]] "face"]
  ;;  [[["M" "AY1"]] "my"]
  ;;  [[["AH0" "G" "EH1" "N" "S" "T"] ["AH0" "G" "EY1" "N" "S" "T"]] "against"]
  ;;  [[["L" "AY1" "F"]] "life"]
  ;;  [[["M" "AY1"]] "my"]
  ;;  [[["L" "AY1" "V"] ["L" "IH1" "V"]] "live"]]

  )

(defn tightly-generate-n-syllable-sentence-rhyming-with
  "It's difficult to mix a tight trie with rhymes. You need
  to convert ids using the database.

  `rhyme-wordset-fn` will take something that looks like
  ([(G AO1 B) bog] [(G AO1 F) fog])
  "
  ([database
    markov-trie
    rhyme-trie
    target-rhyme
    n-gram-rank
    target-rhyme-syllable-count
    target-sentence-syllable-count]
   (tightly-generate-n-syllable-sentence-rhyming-with
    database
    markov-trie
    rhyme-trie
    target-rhyme
    n-gram-rank
    target-rhyme-syllable-count
    target-sentence-syllable-count
    identity
    identity))
  ([database
    markov-trie
    rhyme-trie
    target-rhyme
    n-gram-rank
    target-rhyme-syllable-count
    target-sentence-syllable-count
    markov-process-children
    rhyme-process-words]
   (let [eos (database prhyme/EOS)
         bos (database prhyme/BOS)
         choices (rhyme-choices-walking-target-rhyme
                  rhyme-trie
                  target-rhyme
                  rhyme-process-words)
         [rhyming-phones rhyming-word] (update (rand-nth choices) 1 (comp rand-nth vec))
         ;; The rhyme only has the rhyming phones. Grab full pronunciation.
         rhyming-word-phones (rand-nth (phonetics/get-phones rhyming-word))]
     (loop [phrase [[rhyming-word-phones rhyming-word]]]
       (if (<= target-sentence-syllable-count
               (prhyme/count-syllables-of-phrase
                (string/join " " (map second phrase))))
         phrase
         (recur
          (conj
           phrase
           (let [word (database
                       (get-next-markov
                        markov-trie
                        (into (vec (repeat (dec n-gram-rank) eos))
                              (mapv (comp database second) phrase))
                        markov-process-children))]
             [(rand-nth (phonetics/get-phones word)) word]))))))))

(defn make-markov-filter
  "Specifically works with markovs with entries of the format:
  [lookup [index freq]]
  "
  [words-to-remove]
  (let [words-to-remove (into #{} words-to-remove)]
    (fn [children]
      (remove
       (fn [child]
         (let [[word _] (get child [])]
           (words-to-remove word)))
       children))))

(defn make-rhyme-filter
  [words-to-remove]
  (let [words-to-remove (into #{} words-to-remove)]
    (fn [rhyming-words]
      (->> (map (fn [[phones wordset]]
                  [phones (set/difference wordset words-to-remove)])
                rhyming-words)
           (remove (fn [[phones wordset]]
                     (empty? wordset)))))))

(defn tightly-generate-n-syllable-sentence-v2
  "
  If you want to generate a sentence targeting a rhyme, generate the rhyming tail out-of-band
  and then pass it as a seed to this function.
  "
  ([database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count
    seed]
   (tightly-generate-n-syllable-sentence-v2
    database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count
    identity
    seed))
  ([database
    markov-trie
    n-gram-rank
    target-sentence-syllable-count
    markov-process-children
    seed]
   (let [[eos bos] (map database [prhyme/EOS prhyme/BOS])]
     (loop [phrase seed]
       (if (<= target-sentence-syllable-count
               (prhyme/count-syllables-of-phrase
                (string/join " " (map second phrase))))
         phrase
         (recur
          (conj
           phrase
           (let [word (database
                       (get-next-markov
                        markov-trie
                        (into (vec (repeat (dec n-gram-rank) eos))
                              (mapv (comp database second) phrase))
                        markov-process-children))]
             [(rand-nth (phonetics/get-phones word)) word]))))))))


;;;; Demo
;;;;
(comment
  (let [target-rhyme ["N" "AH1" "F"]]
    (->> (repeatedly
          10
          #(->> (tightly-generate-n-syllable-sentence-rhyming-with
                 database
                 markov-trie
                 rhyme-trie
                 target-rhyme
                 3
                 3
                 7
                 (make-markov-filter (map database ["overdone" "outdone" "rerun" "undone" "</s>" "<s>"]))
                 (make-rhyme-filter ["begun"]))))))
  ;; => ("darkness the lost souls will run"
  ;;     "predominant thunder , scream comes undone"
  ;;     "only day to come undone"
  ;;     "i denounce the bad undone"
  ;;     "me before i will outrun"
  ;;     "convictions of the just will outrun"
  ;;     "you mean stained are overdone"
  ;;     "fight has begun overdone"
  ;;     "are being skinned keep are one"
  ;;     "demise , lift you up overdone")


  (let [target-rhyme (->(prhyme/phrase->all-flex-rhyme-tailing-consonants-phones
                         "filling")
                        first
                        first
                        reverse)]
    (->> (repeatedly
          10
          #(->> (tightly-generate-n-syllable-sentence-rhyming-with
                 database
                 markov-trie
                 rhyme-trie
                 target-rhyme
                 3
                 3
                 7
                 )
                (map second)
                reverse))
         (map (partial remove #{prhyme/BOS}))
         (map data-transform/untokenize)))
  )

(defn take-until
  "Repeately calls some stateful function until predicate is met."
  [take? f]
  (loop [result (f)]
    (if (take? result)
      (first (take? result))
      (recur (f)))))

(comment
  (let [val (atom 0)]
    (letfn [(foo []
              (swap! val inc)
              @val)]
      (take-until even? foo)))
  ;; => 2
  )

(defn valid-or-best-sentence?
  [max-iterations]
  (fn []
    (let [context (atom {:current-best nil :iteration 0})]
      (fn [text]
        (let [sentence (string/join " " (map (comp map second) text))]
          (swap! context update :iteration inc)
          (let [current-best (:current-best @context)
                log-prob (second (nlp/most-likely-parse sentence))]
            (when (or (nil? current-best)
                      (> log-prob (nth current-best 1)))
              (swap! context assoc :current-best [text log-prob]))
            (if (or (> log-prob -1)
                    (>= (:iteration @context) max-iterations))
              (:current-best @context)
              false)))))))

(def best-of-20 (valid-or-best-sentence? 20))

(comment
  (take-until (best-of-20) (constantly "my name sky does eat"))
  )

(comment
  (take-until
   (best-of-10)
   #(->> (tightly-generate-n-syllable-sentence
          database
          markov-trie
          3
          7
          (make-markov-filter (map database [prhyme/BOS prhyme/EOS])))
         (map second)
         reverse
         (string/join " ")))

  )
(defn sentence->phones
  "Sentence is of the format

[[[[F L OW1]] flow]
 [[[AH0 N D] [AE1 N D]] and]
 [[[S IY1 K]] seek]
 [[[F IH1 NG G ER0 Z]] fingers]
 [[[Y AO1 R] [Y UH1 R]] your]
 [[[TH R UW1]] through]
 [[[S T R EH1 NG K TH] [S T R EH1 NG TH]]
  strength]
 [[[F AY1 N D]] find]
 [[[K AE1 N] [K AH0 N]] can]]

  Returns the concatenated list of phones so you can pluck some off and find
  rhymes.

  Note that each word in the sentence can have more than one pronunciation.
  This function chooses one randomly.
  "
  [sentence]
  (->> sentence
       (remove (comp empty? first))  ; Commas have no phones so rand-nth breaks
       (map #(update % 0 rand-nth))
       (apply map vector)
       ((fn [[phones words]]
          [(reduce into [] phones) (string/join " " words)]))
       (first)))

(comment
  (let [sentence '[[[[F L OW1]] flow]
                   [[[AH0 N D] [AE1 N D]] and]
                   [[[S IY1 K]] seek]
                   [[[F IH1 NG G ER0 Z]] fingers]
                   [[[Y AO1 R] [Y UH1 R]] your]
                   [[[TH R UW1]] through]
                   [[[S T R EH1 NG K TH] [S T R EH1 NG TH]]
                    strength]
                   [[[F AY1 N D]] find]
                   [[[K AE1 N] [K AH0 N]] can]]]
    (sentence->phones sentence))

  )

(defn rhyme-from-scheme
  "scheme of format [[A 9] [A 9] [B 5] [B 5] [A 9]]

  Will include as many syllables as possible in finding rhymes
  and will choose randomly with equal chance from all possible rhymes."
  [scheme database markov-trie rhyme-trie]
  (loop [scheme scheme
         line-phones {}
         result []]
    (cond
      (empty? scheme) result
      :else
      (let [[pattern syllable-count] (first scheme)
            banned-words (into #{} (->> result
                                        (map (comp last first))))
            line (if (nil? (get line-phones pattern))
                                        ; Here, we need to make a choice about which pronunciation
                                        ; we want to use to build line-phones. Choose randomly.
                   (take-until
                    (best-of-20)
                    #(tightly-generate-n-syllable-sentence
                      database
                      markov-trie
                      3
                      syllable-count
                      (make-markov-filter (map database [prhyme/BOS prhyme/EOS]))))
                   (take-until
                    (best-of-20)
                    #(tightly-generate-n-syllable-sentence-rhyming-with
                      database
                      markov-trie
                      rhyme-trie
                      (reverse
                       (take-last 4 (prhyme/phones->all-flex-rhyme-tailing-consonants-phones
                                     (get line-phones pattern))))
                      3
                      3
                      syllable-count
                      (make-markov-filter (map database [prhyme/BOS prhyme/EOS]))
                      (make-rhyme-filter banned-words))))
            rhyme (sentence->phones (reverse line))]
        (recur (rest scheme)
               (if (nil? (get line-phones pattern))
                 (assoc line-phones pattern rhyme)
                 line-phones)
               (conj result line))))))

(comment
  (tightly-generate-n-syllable-sentence
   database
   markov-trie
   3
   10)

  (repeatedly
   2
   #(->> (rhyme-from-scheme
          '[[A 8] [A 8] [B 5] [B 5] [A 8]]
          database
          markov-tight-trie
          rhyme-trie)
         (map reverse)
         (map (partial map second))
         (map data-transform/untokenize)))

  (->> "overdrive"
       (prhyme/phrase->unstressed-vowels-and-tailing-consonants)
       (map first)
       (map reverse)
       (map (partial
             rhyme-choices-walking-target-rhyme-with-stop
             rhyme-trie
             (fn [phones choices]
               (every? phonetics/consonant (butlast phones))))))



  (trie/lookup rhyme-trie ["V" "AY1"])
  (trie/lookup markov-tight-trie nil)
  (tightly-generate-n-syllable-sentence-rhyming-with
   database
   markov-trie
   rhyme-trie
   (first
    (first
     (prhyme/phrase->all-flex-rhyme-tailing-consonants-phones
      "bother me")))
   3
   3
   9
   (constantly false)
   (fn [[phones wordset]]
     (set/difference wordset (into #{} []))))

  )

#_(ns-unmap (find-ns 'com.owoga.corpus.markov) 'RhymeTrie)
(defprotocol IRhymeTrie
  (rhymes [this phones] [this phones preprocess-rhymes]))

(deftype RhymeTrie [trie prep-phones end-walk]
  IRhymeTrie
  (rhymes [this phones]
    (rhymes this phones identity))
  (rhymes [this phones preprocess-rhymes]
    (let [prepped-phones (reverse (prep-phones phones))]
      (rhyme-choices-walking-target-rhyme-with-stop
       trie
       end-walk
       prepped-phones
       preprocess-rhymes))))

(comment
  (def rhymetrie
    (->RhymeTrie
     rhyme-trie
     (fn [phones]
       (->> phones
            prhyme/take-vowels-and-tail-consonants
            prhyme/remove-all-stress))
     (fn [phones choices]
       (every? phonetics/consonant (butlast phones)))))

  (rhymes rhymetrie ["AY" "V"])

  (time
   (count
    (trie/children-at-depth markov-tight-trie 2 3)))

  )

(defn rhyme-from-scheme-v2
  "scheme of format [[A 9] [A 9] [B 5] [B 5] [A 9]]

  Will include as many syllables as possible in finding rhymes
  and will choose randomly with equal chance from all possible rhymes.

  Result will be a map of schemes to vectors of lines.
  {[A 9] [[[you [Y UW]] [are [AA R]] [so [S OH]]]
          [[and [AE N D]] [we [W IY]] [go [G OH]]]]
   [B 5] [[[hey [H AY]]]
          [[bay [B AY]]]]}

  Currently hard-coded to work with 4-gram.
  "
  [scheme database markov-trie rhyme-trie]
  (let [[eos bos] (map database [prhyme/EOS prhyme/BOS])]
    (loop [scheme scheme
           result {}]
      (if (empty? scheme)
        result
        (let [[pattern syllable-count] (first scheme)
              existing-lines (result (first scheme))
              banned-words
              (into #{} (->> existing-lines
                             (map (comp last last))))

              line
              (take-until
               (best-of-20)
               (fn []
                 (let [seed (if existing-lines
                              (->> existing-lines
                                   rand-nth
                                   reverse
                                   (map first)
                                   (apply concat)
                                   (#(rhymes
                                      rhyme-trie
                                      %
                                      (fn [choices]
                                        (->> choices
                                             (map (fn [[phones wordset]]
                                                    [phones
                                                     (set/difference
                                                      wordset
                                                      banned-words)]))
                                             (remove (comp empty? second))))))
                                   rand-nth
                                   ((fn [[phones wordset]]
                                      (let [word (rand-nth (vec wordset))]
                                        [(rand-nth (phonetics/get-phones word))
                                         word])))
                                   vector)
                              (->> (get-next-markov
                                    markov-trie
                                    [eos eos eos]
                                    (fn [children]
                                      (remove
                                       #(#{eos bos} (.key %)) children)))
                                   database
                                   (#(vector (rand-nth (phonetics/get-phones %)) %))
                                   vector))
                       line (tightly-generate-n-syllable-sentence-v2
                             database
                             markov-trie
                             4
                             syllable-count
                             (make-markov-filter [eos bos])
                             seed)]
                   line)))]
          (recur (rest scheme)
                 (update result (first scheme) (fnil conj []) line)))))))

(comment
  (let [scheme '[[a 8] [a 8] [b 5] [b 5] [a 8]]]
    (rhyme-from-scheme-v2
     scheme database markov-tight-trie rhymetrie))

  (phonetics/get-phones "unleashed")
  (rhymes
   rhymetrie
   ["IY" "SH" "T"]
   (fn [choices]
     (->> choices
          (map (fn [[phones wordset]]
                 [phones
                  (set/difference
                   wordset
                   #{"unleashed"})]))
          (remove (comp empty? second)))))

  )

(comment
  (let [existing-lines '([[["K" "AA" "AH"] "unlock"]
                          [["M" "EH1" "M" "ER0" "IY0" "Z"] "memories"]
                          [["D" "IH0" "Z" "AO1" "L" "V" "IH0" "NG"] "dissolving"]])]
    (->> existing-lines
         rand-nth
         reverse
         (map first)
         (mapcat reverse)))

  )
