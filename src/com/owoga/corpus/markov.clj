(ns com.owoga.corpus.markov
  (:require [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.nlp.core :as nlp]
            [com.owoga.prhyme.data-transform :as data-transform]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.phonetics :as phonetics]))

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
    (map (partial map reverse))
    (map (partial into [] (data-transform/xf-pad-tokens (dec m) "</s>" 1 "<s>")))
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



(defn initialize
  "Takes an atom as a context. Swaps in :database, :trie, :rhyme-trie"
  [context]

  (swap!
   context
   assoc
   :rhyme-trie
   (transduce
    (comp
     (map first)
     (filter string?)
     (map (fn [word]
            (let [phones-coll (phonetics/get-phones)]
              (map
               #(vector (reverse (phonetics/get-phones %)) word)
               phones-coll)))))
    (completing
     (fn [trie [k v]]
       (update trie k (fnil #(update % 1 inc) [v 0]))))
    (trie/make-trie)
    (@context :database)))

  (swap!
   context
   assoc
   :flex-rhyme-trie
   (transduce
    (comp
     (map first)
     (filter string?)
     (map #(vector (reverse (prhyme/phrase->flex-rhyme-phones %)) %)))
    (completing
     (fn [trie [k v]]
       (update trie k (fnil conj [v]) v)))
    (trie/make-trie)
    (@context :database)))
  nil)
