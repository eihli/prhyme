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
                     (when (.equals ngram-id @next-id)
                       (swap! database #(-> % (assoc gram-ids @next-id)))
                       (vswap! next-id inc))
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

(defn create-trie-from-texts [texts]
  (->> texts
       (map #(n-to-m-grams 1 5 %))
       (apply concat)
       (map prep-ngram-for-trie)
       (reduce
        (fn [[trie i db] [k v]]
          (let [[db i] (reduce
                        (fn [[db i] k]
                          (let [id (get db k i)
                                i (if (= id i) (inc i) i)
                                db (-> db
                                       (assoc id k)
                                       (assoc k id))]
                            [db i]))
                        [db i]
                        k)
                k' (map #(get db %) k)]
            (if-let [existing (get trie k')]
              (let [[val count] existing
                    trie (assoc trie k' [val (inc count)])]
                [trie i db])
              [(assoc trie k' [i 1])
               (inc i)
               (assoc db i k')])))
        [(trie/make-trie) 1 {}])))

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

(time
 (def trie
   (transduce (comp (xf-file-seq 0 10)
                    (map slurp)
                    (map (partial n-to-m-grams 1 4))
                    (map (fn [ngrams] (map #(prep-ngram-for-trie %) ngrams)))
                    stateful-transducer)
              conj
              (file-seq (io/file "dark-corpus")))))

(comment
  (let [texts (->> (dark-corpus-file-seq 0 5)
                   (map slurp))
        [trie _ db] (create-trie-from-texts texts)]
    texts)

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

(time
 (def tightly-packed-trie
   (tpt/tightly-packed-trie
    trie
    encode-fn
    (decode-fn @trie-database))))

(defn key-get-in-tpt [tpt db ks]
  (let [id (map #(get-in db [(list %) :id]) ks)
        v (get tpt id)]
    {id v}))

(defn id-get-in-tpt [tpt db ids]
  (let [ks (apply concat (map #(get db %) ids))
        v (get tpt ids)
        id (get-in db [ks :id])]
    {ks (assoc v :value (get db id))}))



(comment
  (->> (trie/lookup tightly-packed-trie [1])
       (trie/children)
       (map #(get % []))
       (remove #(nil? (first %)))
       (math/weighted-selection second))

  (->> trie
       (#(trie/lookup % [1]))
       (trie/children)
       (map #(get % []))
       (remove nil?)
       (map first)
       (map #(trie-database %))
       (map #(map trie-database %)))

  (->> tightly-packed-trie
       (#(trie/lookup % [1]))
       (trie/children)
       (map #(get % []))
       (remove nil?)
       (math/weighted-selection second)
       first)

  (->> trie
       (#(trie/lookup % [1]))
       (trie/children)
       (map #(get % []))
       (remove nil?)
       (math/weighted-selection second)
       first)

  (take 20 (seq @trie-database))
  (take 20 trie)
  (take 20 tightly-packed-trie)

  (->> (trie/lookup tightly-packed-trie [1])
       (trie/children)
       (map #(get % []))
       (remove nil?)
       (math/weighted-selection #(nth % 1))
       first
       (@trie-database))

  (profile
   {}
   (def example-story
     (loop [generated-text [(get @trie-database "<s>")]
            i              0]
       (println generated-text)
       (if (> i 100)
         generated-text
         (let [children (loop [i 4]
                          (let [node (p :lookup
                                        (trie/lookup
                                         tightly-packed-trie
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
   '(2 2 3))
  ;; => {("<s>" "<s>" "the") {:value ("<s>" "<s>" "the"), :count 462}}
  )

(comment
  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (tpt/as-map (transform-trie->ids trie)))

  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)
        tightly-packed-trie (tpt/tightly-packed-trie
                             (transform-trie->ids trie))]
    (get tightly-packed-trie '(2 2 3)))


  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (tpt/as-map trie))

  (let [text (slurp (first (dark-corpus-file-seq 500 1)))]
    (->> text
         util/clean-text
         (#(string/split % #"\n+"))))

  )
