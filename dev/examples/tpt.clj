(ns examples.tpt
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.trie.math :as math]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [clojure.zip :as zip]
            [cljol.dig9 :as d]))

(tufte/add-basic-println-handler! {})

(defn xf-file-seq [start end]
  (comp (remove #(.isDirectory %))
        (drop start)
        (take end)))

(defn dark-corpus-file-seq [start end]
  (let [xf (comp (remove #(.isDirectory %))
                 (drop start)
                 (take end))
        documents (file-seq (io/file "dark-corpus"))]
    (transduce xf conj documents)))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn pad-tokens
  "Pads the beginning with n - 1 <s> tokens and
  the end with 1 </s> token."
  [tokens n]
  (vec (concat (vec (repeat (max 1 (dec n)) "<s>")) tokens ["</s>"])))

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

(declare ->TrieKey)

(deftype TrieKey [key]
  clojure.lang.IPersistentStack
  (peek [self]
    (let [x (last (seq self))]
      (if (.equals "" x)
        nil
        (Integer/parseInt x))))
  (pop [self]
    (TrieKey. (string/replace key #"(.*):.*$" "$1")))

  clojure.lang.ISeq
  (first [self]
    (let [x (first (seq self))]
      (if (.equals x "")
        nil
        (Integer/parseInt x))))
  (next [self]
    (TrieKey. (string/replace key #".*?:(.*)" "$1")))
  (more [self]
    (let [xs (string/split key #":")]
      (if (.equals xs "") '() (into (->TrieKey "") (rest xs)))))
  (cons [self o]
    (TrieKey.
     (cond
       (.equals key "") ":"
       (.equals key ":") (str key o)
       :else (str key ":" o))))

  clojure.lang.IPersistentCollection
  (count [self]
    (count (seq self)))
  (empty [self]
    (TrieKey. ""))
  (equiv [self o]
    (.equals self o))

  clojure.lang.Seqable
  (seq [self]
    (if (.equals "" key)
      nil
      (seq (string/split key #":")))))

(defmethod print-method TrieKey [trie-key ^java.io.Writer w]
  (print-method (.key trie-key) w))

(defmethod print-dup TrieKey [trie-key ^java.io.Writer w]
  (print-ctor trie-key (fn [o w] (print-dup (.key trie-key) w)) w))

(defn trie-key
  ([]
   (->TrieKey ""))
  ([coll]
   (->TrieKey (string/join ":" coll))))


(def trie-database (atom nil))

(defn stateful-transducer [xf]
  (let [trie (volatile! (trie/make-trie))
        database (atom {})
        next-id (volatile! 1)]
    (fn
      ([] (xf))
      ([result]
       (reset! trie-database @database)
       (xf result))
      ([result input]
       (let [ngrams-ids
             (mapv
              (fn [ngrams]
                (mapv
                 (fn [ngram]
                   (let [gram-ids (mapv
                                   (fn [gram]
                                     (let [gram-id (get @database gram @next-id)]
                                       (when (.equals gram-id @next-id)
                                         (swap! database
                                                #(-> %
                                                     (assoc gram gram-id)
                                                     (assoc gram-id gram)))
                                         (vswap! next-id inc))
                                       gram-id))
                                   ngram)
                         ngram-id (get database gram-ids @next-id)]
                     gram-ids))
                 ngrams))
              input)]
         (vswap!
          trie
          (fn [trie ngrams-ids]
            (reduce
             (fn [trie [ngram-ids _]]
               (update trie ngram-ids (fnil #(update % 1 inc) [(peek ngram-ids) 0])))
             trie
             ngrams-ids))
          ngrams-ids))))))

(defn prep-ngram-for-trie
  "The tpt/trie expects values conjed into an ngram
  to be of format '(k1 k2 k3 value)."
  [ngram]
  (clojure.lang.MapEntry. (vec ngram) ngram))

(defn seq-of-nodes->sorted-by-count
  "Sorted first by the rank of the ngram, lowest ranks first.
  Sorted second by the frequency of the ngram, highest frequencies first.
  This is the order that you'd populate a mapping of keys to IDs."
  [trie]
  (->> trie
       trie/children
       (map #(get % []))
       (sort-by :count)
       reverse))

(defn rhyme-trie-transducer [xf]
  (let [trie (volatile! (trie/make-trie))
        database (atom {})
        next-id (volatile! 1)]
    (fn
      ([] (xf))
      ([result]
       (reset! trie-database @database)
       (xf result))
      ([result input]
       (let [ngrams-ids
             (mapv
              (fn [ngrams]
                (mapv
                 (fn [ngram]
                   (let [gram-ids (mapv
                                   (fn [gram]
                                     (let [gram-id (get @database gram @next-id)]
                                       (when (.equals gram-id @next-id)
                                         (swap! database
                                                #(-> %
                                                     (assoc gram gram-id)
                                                     (assoc gram-id gram)))
                                         (vswap! next-id inc))
                                       gram-id))
                                   ngram)
                         ngram-id (get database gram-ids @next-id)]
                     gram-ids))
                 ngrams))
              input)]
         (vswap!
          trie
          (fn [trie ngrams-ids]
            (reduce
             (fn [trie [ngram-ids _]]
               (update trie ngram-ids (fnil #(update % 1 inc) [(peek ngram-ids) 0])))
             trie
             ngrams-ids))
          ngrams-ids))))))

(comment
  (time
   (def trie
     (transduce (comp (xf-file-seq 0 250000)
                      (map slurp)
                      (map (partial n-to-m-grams 1 4))
                      (map (fn [ngrams] (map #(prep-ngram-for-trie %) ngrams)))
                      stateful-transducer)
                conj
                (file-seq (io/file "dark-corpus")))))

  (time
   (def backwards-trie
     (transduce (comp (xf-file-seq 0 1000)
                      (map slurp)
                      (map (partial n-to-m-backwards-grams 1 4))
                      (map (fn [ngrams] (map #(prep-ngram-for-trie %) ngrams)))
                      stateful-transducer)
                conj
                (file-seq (io/file "dark-corpus")))))

  )

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

(comment
  (time
   (def tightly-packed-trie
     (tpt/tightly-packed-trie
      trie
      encode-fn
      (decode-fn @trie-database))))

  (time
   (def tightly-packed-backwards-trie
     (tpt/tightly-packed-trie
      backwards-trie
      encode-fn
      (decode-fn @trie-database))))

  )

(defn key-get-in-tpt [tpt db ks]
  (let [id (map #(get-in db [(list %) :id]) ks)
        v (get tpt id)]
    {id v}))

(defn id-get-in-tpt [tpt db ids]
  (let [ks (apply concat (map #(get db %) ids))
        v (get tpt ids)
        id (get-in db [ks :id])]
    {ks (assoc v :value (get db id))}))



(defn word->phones [word]
  (or (dict/word->cmu-phones word)
      (util/get-phones-with-stress word)))

(defn perfect-rhymes [rhyme-trie phones]
  (let [rhyme-suffix (first
                      (util/take-through
                       #(= (last %) \1)
                       (reverse phones)))]
    (trie/lookup rhyme-trie rhyme-suffix)))

(defn n+1grams [trie k]
  (->> (trie/lookup trie k)
       (trie/children)
       (map #(get % []))))

(comment
  ;; Bigrams of rhyme
  (->> (perfect-rhymes rhyme-trie (or (dict/cmu-with-stress-map "pleasing")
                                      (util/get-phones-with-stress "pleasing")))
       (map (comp first second))
       (remove nil?)
       (map @trie-database)
       (map #(vector [%] (n+1grams
                          tightly-packed-backwards-trie
                          [%])))
       (map (fn [[w1 w2s]]
              (mapv #(into w1 [(nth % 0)]) w2s)))
       (reduce into [])
       (map (fn [k]
              (let [children (->> (n+1grams tightly-packed-backwards-trie k)
                                  (mapv first))]
                (mapv #(into k [%]) children))))
       (reduce into [])
       (map #(map @trie-database %)))

  )
(comment
  (do
    (time
     (def backwards-trie
       (transduce (comp (xf-file-seq 0 250000)
                        (map slurp)
                        (map (partial n-to-m-backwards-grams 1 4))
                        (map (fn [ngrams] (map #(prep-ngram-for-trie %) ngrams)))
                        stateful-transducer)
                  conj
                  (file-seq (io/file "dark-corpus")))))
    (time
     (def tightly-packed-backwards-trie
       (tpt/tightly-packed-trie
        backwards-trie
        encode-fn
        (decode-fn @trie-database))))
    (tpt/save-tightly-packed-trie-to-file
     "resources/dark-corpus-backwards-tpt.bin"
     tightly-packed-backwards-trie)
    (with-open [wtr (clojure.java.io/writer "resources/backwards-database.bin")]
      (let [lines (->> (seq @trie-database)
                       (map pr-str)
                       (map #(str % "\n")))]
        (doseq [line lines]
          (.write wtr line))))
    (def loaded-backwards-trie
      (tpt/load-tightly-packed-trie-from-file
       "resources/dark-corpus-backwards-tpt.bin"
       (decode-fn @trie-database)))
    (def loaded-database
      (atom (with-open [rdr (clojure.java.io/reader "resources/backwards-database.bin")]
              (into {} (map read-string (line-seq rdr))))))
    (->> (take 20 loaded-backwards-trie)
         (map first)
         (map (partial map @loaded-database)))
    (def rhyme-database (atom {}))
    (def rhyme-trie
      (transduce
       (comp
        (map first)
        (filter string?)
        (map #(vector % (reverse (word->phones %))))
        (map reverse))
       (completing
        (fn [trie [k v]]
          (update trie k (fnil #(update % 1 inc) [v 0]))))
       (trie/make-trie)
       @loaded-database))

    (trie/lookup rhyme-trie '("IY0" "JH"))

    )

  (tpt/save-tightly-packed-trie-to-file "dark-corpus-tpt.bin" tightly-packed-trie)

  (def loaded-tightly-packed-trie (tpt/load-tightly-packed-trie-from-file
                                   "dark-corpus-tpt.bin"
                                   (decode-fn @trie-database)))

  [(first tightly-packed-trie)
   (first loaded-tightly-packed-trie)]

  (take-last 10 (.array (.byte-buffer loaded-tightly-packed-trie)))
  ;; => (-127 -124 -42 -23 28 -127 -124 -41 -90 9)
  ;; => (0 0 0 0 0 37 0 6 -124 -56 -128 -121 1 -17 -128 -118 -117 -128 -115 2)

  (take-last 10 (.array (.byte-buffer tightly-packed-trie)))
  ;; => (-127 -124 -42 -23 28 -127 -124 -41 -90 9)
  ;; => (0 0 0 0 0 37 0 6 -124 -56 -128 -121 1 -17 -128 -118 -117 -128 -115 2)
  (.byte-buffer loaded-tightly-packed-trie)
  ;; => #object[java.nio.HeapByteBuffer 0x21b8291a "java.nio.HeapByteBuffer[pos=8 lim=2548630 cap=2548630]"]

  (.byte-buffer tightly-packed-trie)
  ;; => #object[java.nio.HeapByteBuffer 0x7dc15357 "java.nio.HeapByteBuffer[pos=8 lim=2548630 cap=2548630]"]

  [(.key loaded-tightly-packed-trie)
   (.address loaded-tightly-packed-trie)
   (.limit loaded-tightly-packed-trie)]
  ;; => [0 2424838 2548630]

  [(.key tightly-packed-trie)
   (.address tightly-packed-trie)
   (.limit tightly-packed-trie)]
  ;; => [0 2424838 2548630]

  (->> (trie/lookup tightly-packed-trie [1])
       (trie/children)
       (map #(get % []))
       (remove nil?)
       (math/weighted-selection #(nth % 1))
       first
       (@trie-database))

  (with-open [wtr (clojure.java.io/writer "database.bin")]
    (let [lines (->> (seq @trie-database)
                     (map pr-str)
                     (map #(str % "\n")))]
      (doseq [line lines]
        (.write wtr line))))

  (def trie-database
    (atom (with-open [rdr (clojure.java.io/reader "database.bin")]
            (into {} (map read-string (line-seq rdr))))))

  (->> loaded-tightly-packed-trie
       (take 20)
       (map first)
       (map (partial map @trie-database)))

  (profile
   {}
   (def example-story
     (loop [generated-text [(get @trie-database "<s>")]
            i              0]
       (if (> i 20)
         generated-text
         (let [children (loop [i 4]
                          (let [node (p :lookup
                                        (trie/lookup
                                         loaded-tightly-packed-trie
                                         (vec (take-last i generated-text))))
                                children (p :seq-children (and node (trie/children node)))]
                            (cond
                              (nil? node)    (recur (dec i))
                              (< i 0)        (throw (Exception. "Error"))
                              (seq children) children
                              :else          (recur (dec i)))))]
           (recur
            (conj
             generated-text
             (->> children
                  (map #(get % []))
                  (remove nil?)
                  (#(p :weighted-selection (math/weighted-selection
                                            (fn [[_ c]] c)
                                            %)))
                  first))
            (inc i)))))))

  (->> example-story
       (map (fn [v] (get-in @trie-database [v])))
       (string/join " ")
       (#(string/replace % #" ([\.,\?])" "$1"))
       ((fn [txt]
          (string/replace txt #"(^|\. |\? )([a-z])" (fn [[a b c]]
                                                      (str b (.toUpperCase c)))))))

  (key-get-in-tpt
   tightly-packed-trie
   trie-database
   '("<s>" "<s>" "the"))
  ;; => {(2 2 3) {:value 3263, :count 462}}
  (id-get-in-tpt
   tightly-packed-trie
   trie-database
   '(2 2 3)))
  ;; => {("<s>" "<s>" "the") {:value ("<s>" "<s>" "the"), :count 462}}


