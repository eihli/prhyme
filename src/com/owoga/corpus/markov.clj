(ns com.owoga.corpus.markov
  (:require [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.nlp.core :as nlp]
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

(defn make-markov [tokens n]
  (reduce
   (fn [a w]
     (let [k (butlast w)
           v (last w)]
       (update-in a [k v] (fnil inc 0))))
   {}
   ((util/window (inc n)) tokens)))

(defn merge-markov [& maps]
  (apply
   merge-with
   (fn [a-possibilities b-possibilities]
     (merge-with
      (fn [a b]
        ((fnil + 0) a b))
      a-possibilities
      b-possibilities))
   maps))

(comment
  (merge-markov
   {'("away") {"her" 1
               "foo" 7}}
   {'("away") {"her" 2
               "them" 1
               "bar" 8}}
    {'("away") {"her" 10
               "them" 50
                "baz" 99}})

  )


(defn slurp-file-to-read-string
  "Returns the value of read-string of the contents of the file.
  Useful for reading into memory a saved database of n-grams to identifiers
  and identifiers to n-grams."
  [filepath]
  (read-string (slurp filepath)))

(defn spit-edn-to-file
  [filepath data]
  (spit filepath (pr-str data)))

(comment
  (do
    (spit-edn-to-file
     "/tmp/spit-edn-test.txt"
     {:a {:b :c}})
    (slurp-file-to-read-string "/tmp/spit-edn-test.txt"));; => {:a {:b :c}}
  )

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

(defn stateful-transducer
  "Stateful transform that crates a trie and populates an `atom` database."
  [database xf]
  (let [trie (volatile! (trie/make-trie))
        next-id (volatile! 1)]
    (fn
      ([] (xf))
      ([result]
       (xf result))
      ([result map-entries-in]
       (let [map-entries-out
             (mapv
              (fn [[lookup v]]
                (mapv
                 (fn [key]
                   (let [key-id (get @database key @next-id)]
                     (when (.equals key-id @next-id)
                       (swap! database
                              #(-> %
                                   (assoc key key-id)
                                   (assoc key-id key)))
                       (vswap! next-id inc))
                     (mapv @database lookup)))
                 lookup))
              map-entries-in)]
         (vswap!
          trie
          (fn [trie map-entries-out]
            (reduce
             (fn [trie [lookup _]]
               (update trie lookup (fnil #(update % 1 inc) [(peek lookup) 0])))
             trie
             map-entries-out))
          map-entries-out))))))

(defn pad-tokens
  "Pads the beginning with n - 1 <s> tokens and
  the end with 1 </s> token."
  [tokens n]
  (vec (concat (vec (repeat (max 1 (dec n)) "<s>")) tokens ["</s>"])))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn tokenize-line
  [line]
  (->> line
       (string/trim)
       (re-seq re-word)
       (mapv second)
       (mapv string/lower-case)))

(defn text->ngrams
  "Takes text from a file, including newlines.
  Pads lines with <s> and </s> for start/end of line.
  Pads beginning with n - 1 <s>s"
  [text n]
  (->> text
       util/clean-text
       (#(string/split % #"\n+"))
       (remove empty?)
       (mapv tokenize-line)
       (mapv #(pad-tokens % n))
       (mapv #(partition n 1 %))
       (mapv #(mapv vec %))
       (reduce #(into %1 %2) [])))

(defn n-to-m-grams
  "Exclusive of m, similar to range."
  [n m text]
  (loop [i n
         r []]
    (cond
      (= i m)
      r
      :else
      (recur (inc i)
             (into r (text->ngrams text i))))))

(comment
  (n-to-m-grams 1 3 "The quick brown fox jumps over the lazy dog.")
  ;; => [["<s>"]
  ;;     ["the"]
  ;;     ["quick"]
  ;;     ,,,
  ;;     ["the" "lazy"]
  ;;     ["lazy" "dog"]
  ;;     ["dog" "</s>"]]

  )

(defn text->backwards-ngrams
  "Takes text from a file, including newlines.
  Pads lines with <s> and </s> for start/end of line.
  Pads beginning with n - 1 <s>s"
  [text n]
  (->> text
       util/clean-text
       (#(string/split % #"\n+"))
       (remove empty?)
       (mapv tokenize-line)
       (mapv #(pad-tokens % n))
       reverse
       (mapv reverse)
       (mapv #(partition n 1 %))
       (mapv #(mapv vec %))
       (reduce #(into %1 %2) [])))

(defn n-to-m-backwards-grams
  "Exclusive of m, similar to range."
  [n m text]
  (loop [i n
         r []]
    (cond
      (= i m)
      r
      :else
      (recur (inc i)
             (into r (text->backwards-ngrams text i))))))

(defn prep-ngram-for-trie
  "The tpt/trie expects values conjed into an ngram
  to be of format '[[k1 k2 k3] value]."
  [ngram]
  (clojure.lang.MapEntry. (vec ngram) ngram))

(defn make-trie-and-database
  "Takes a file seq, like (file-seq (io/file \"dark-corpus\"))"
  [file-seq]
  (let [database (atom {})
        trie (transduce (comp (xf-file-seq 501 2)
                              (map slurp)
                              (map (partial n-to-m-grams 1 4))
                              (map (fn [ngrams] (mapv #(prep-ngram-for-trie %) ngrams)))
                              (partial stateful-transducer database))
                        conj
                        file-seq)]
    [trie database]))

(defn make-backwards-trie-and-database
  [file-seq]
  (let [database (atom {})
        trie (transduce (comp (xf-file-seq 0 1000)
                              (map slurp)
                              (map (partial n-to-m-backwards-grams 1 4))
                              (map (fn [ngrams] (mapv #(prep-ngram-for-trie %) ngrams)))
                              (partial stateful-transducer database))
                        conj
                        (file-seq (io/file "dark-corpus")))]
    [trie database]))



(comment

  (take 20 trie)
  (take 20 @trie-database)
  (->> (map #(get % []) (trie/children (trie/lookup trie [1])))
       (map first)
       (map @trie-database))

  )


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
                    (eduction (xf-file-seq 0 1000)))
         [trie database] (train-backwards files 1 4 "/tmp/trie.bin" "/tmp/database.bin" "/tmp/tpt.bin")]))

  (def trie (into (trie/make-trie) (nippy/thaw-from-file "/tmp/trie.bin")))

  (take 5 trie)
  ;; => ([(0 0 0 1) [1 2]]
  ;;     [(0 0 0 3) [3 1]]
  ;;     [(0 0 0 4) [4 1]]
  ;;     [(0 0 0 5) [5 8]]
  ;;     [(0 0 0 10) [10 1]])
  (def tight (tpt/tightly-packed-trie trie encode-fn (decode-fn db)))
  tight
  (def db (nippy/thaw-from-file "/tmp/database.bin"))

  (db 4)
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

  (def rt (into (trie/make-trie) (nippy/thaw-from-file "/tmp/rhyme-trie.bin")))

  (take 100 rt)

  (prhyme/phrase->all-flex-rhyme-tailing-consonants-phones "brasilia")
  (phonetics/get-phones "brasilia")

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
       (map #(update % 0 into (reverse phones)))))

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
      (println target-rhyme choices result)
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
  [{:keys [trie database] :as context} seed]
  (let [seed (take-last 3 seed)
        node (trie/lookup trie seed)
        children (and node
                      (->> node
                           trie/children
                           (map (fn [^com.owoga.trie.ITrie child]
                                  [(.key child)
                                   (get child [])]))
                           (remove (comp nil? second))
                           (remove
                            (fn [[k v]]
                              (#{1 38} k)))))]
    (cond
      (nil? node) (recur context (rest seed))
      (seq children)
      (if (< (rand) (/ (apply max (map (comp second second) children))
                       (apply + (map (comp second second) children))))
        (recur context (rest seed))
        (first (math/weighted-selection (comp second second) children)))
      (> (count seed) 0)
      (recur context (rest seed))
      :else (throw (Exception. "Error")))))

(defn get-next-markov-from-phrase-backwards
  [{:keys [database trie] :as context} phrase n]
  (let [word-ids (->> phrase
                      (#(string/split % #" "))
                      (take n)
                      (reverse)
                      (map database))]
    (database (get-next-markov context word-ids))))

(defn generate-n-syllable-sentence-rhyming-with
  [context target-phrase n-gram-rank target-rhyme-syllable-count target-sentence-syllable-count]
  (if (string? target-phrase)
    (let [target-phrase-words (string/split target-phrase #" ")
          reversed-target-phrase (string/join " " (reverse target-phrase-words))
          target-rhyme
          (->> (prhyme/take-words-amounting-to-at-least-n-syllables
                reversed-target-phrase
                target-rhyme-syllable-count)
               (#(string/split % #" "))
               reverse
               (string/join " "))
          rhyming-n-gram (->> (rhyming-n-gram-choices context target-rhyme)
                              (weighted-selection-from-choices)
                              (choice->n-gram context)
                              (string/join " "))]
      (loop [phrase rhyming-n-gram]
        (if (<= target-sentence-syllable-count (prhyme/count-syllables-of-phrase phrase))
          phrase
          (recur
           (str (get-next-markov-from-phrase-backwards context phrase n-gram-rank)
                " "
                phrase)))))
    (let [target-rhyme
          (->> (prhyme/take-n-syllables target-phrase target-rhyme-syllable-count))
          rhyming-n-gram (->> (rhyming-n-gram-choices context target-rhyme)
                              (weighted-selection-from-choices)
                              (choice->n-gram context)
                              (string/join " "))]
      (loop [phrase rhyming-n-gram]
        (if (<= target-sentence-syllable-count (prhyme/count-syllables-of-phrase phrase))
          phrase
          (recur
           (str (get-next-markov-from-phrase-backwards context phrase n-gram-rank)
                " "
                phrase)))))))
