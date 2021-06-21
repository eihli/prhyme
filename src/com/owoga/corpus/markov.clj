(ns com.owoga.corpus.markov
  (:require [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data-transform :as data-transform]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.syllabify :as syllabify]
            [taoensso.nippy :as nippy]))

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
    (nippy/freeze-to-file database-filepath @database)
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
                    (eduction (xf-file-seq 0 5000)))
         [trie database] (train-backwards files 1 4 "/tmp/trie.bin" "/tmp/database.bin" "/tmp/tpt.bin")]))

  (def markov-trie (into (trie/make-trie) (nippy/thaw-from-file "/tmp/trie.bin")))
  (def database (nippy/thaw-from-file "/tmp/database.bin"))
  (def markov-tight-trie (tpt/tightly-packed-trie markov-trie encode-fn (decode-fn db)))
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
  (let [database (atom (nippy/thaw-from-file "/tmp/database.bin"))]
    (gen-rhyme-model prhyme/phrase->all-flex-rhyme-tailing-consonants-phones database "/tmp/rhyme-trie.bin"))

  (def rhyme-trie (into (trie/make-trie) (nippy/thaw-from-file "/tmp/rhyme-trie.bin")))
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
  (let [rhyme-trie (trie/make-trie ["G" "AA1" "B"] "bog" ["G" "AO1" "B"] "bog"
                                   ["T" "AA1" "H"] "hot" ["G" "AO1" "F"] "fog")]
    [(rhyme-choices rhyme-trie ["G" "AO1"])
     (rhyme-choices rhyme-trie "fog")
     (rhyme-choices rhyme-trie "bog")])
  ;; => [([("G" "AO1" "B") "bog"] [("G" "AO1" "F") "fog"])
  ;;     ([("G" "AO1" "F") "fog"])
  ;;     ([("G" "AA1" "B") "bog"] [("G" "AO1" "B") "bog"])]
  )

(defn rhyme-choices-walking-target-rhyme
  "All target rhymes need to be in phone form.
  If we try to turn string form into phone form,
  we'd sometimes be forced to deal with multiple pronunciations.
  By only handling phone form here, the caller can handle multiple pronunciations.
  Makes for a cleaner API."
  [trie target-rhyme]
  (loop [target-rhyme target-rhyme
         result []]
    (let [choices (rhyme-choices trie target-rhyme)]
      (if (or (empty? target-rhyme) (prhyme/last-primary-stress? (reverse target-rhyme)))
        (into result choices)
        (recur (butlast target-rhyme)
               (into result choices))))))

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
  )

(defn get-next-markov
  "Weighted selection from markov model with backoff.
  Expects markov key/values to be [k1 k2 k3] [<value> freq]."
  ([markov-trie seed]
   (get-next-markov markov-trie seed (constantly false)))
  ([markov-trie seed remove-fn]
   (let [seed (take-last 3 seed)
         node (trie/lookup markov-trie seed)
         children (and node
                       (->> node
                            trie/children
                            (map (fn [^com.owoga.trie.ITrie child]
                                        ; Get key and frequency of each child
                                   [(.key child)
                                    (get child [])]))
                            (remove (comp nil? second))
                            (remove remove-fn)))]
     (cond
                                        ; If we've never seen this n-gram, fallback to n-1-gram
       (nil? node) (recur markov-trie (rest seed) remove-fn)
       (seq children)
       (if (< (rand) (/ (apply max (map (comp second second) children))
                        (apply + (map (comp second second) children))))
         (recur markov-trie (rest seed) remove-fn)
         (first (math/weighted-selection (comp second second) children)))
       (> (count seed) 0)
       (recur markov-trie (rest seed) remove-fn)
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

(defn tightly-generate-n-syllable-sentence-rhyming-with
  "It's difficult to mix a tight trie with rhymes. You need
  to convert ids using the database."
  [database
   markov-trie
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
          (let [word (database
                      (get-next-markov
                       markov-trie
                       (into (vec (repeat (dec n-gram-rank) (database prhyme/EOS)))
                             (mapv (comp database second) phrase))
                       (fn [[lookup [word frequency]]]
                         (= (database prhyme/EOS) word))))]
            [(phonetics/get-phones word) word])))))))


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
                 7)
                (map second)
                reverse))
         (map (partial remove #{prhyme/BOS}))
         (map (partial string/join " "))))

  )
