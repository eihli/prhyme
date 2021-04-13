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

(defn dark-corpus-file-seq [start end]
  (let [documents (->> "dark-corpus"
                       io/file
                       file-seq
                       (remove #(.isDirectory %))
                       (drop start)
                       (take end))]
    documents))

(defn remove-sentences-with-words-not-in-dictionary
  "This gets rid of a lot of good words. All contractions for example... I'll, They'll...
  possessives like morning's...

  Might not end up using it."
  [dictionary]
  (let [dictionary (into #{} dictionary)]
    (fn [sentences]
      (->> sentences
           (map #(string/split % #" "))
           (remove #(some (complement dictionary) %))
           (remove #(some string/blank? %))
           (map #(string/join " " %))))))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn pad-tokens
  "Pads the beginning with n - 1 <s> tokens and
  the end with 1 </s> token."
  [tokens n]
  (concat (repeat (max 1 (dec n)) "<s>") tokens ["</s>"]))

(defn tokenize-line
  [line]
  (->> line
       (string/trim)
       (re-seq re-word)
       (map second)
       (map string/lower-case)))

(defn process-files-for-trie
  "Expects an entire song, lines seperated by \n."
  [files]
  (->> files
       (map slurp)
       (filter dict/english?)
       (map util/clean-text)
       (map #(string/split % #"\n+"))
       (map (remove-sentences-with-words-not-in-dictionary dict/popular))
       (remove empty?)
       (remove #(some empty? %))
       (map (fn [lines]
              (map tokenize-line lines)))
       (map (fn [lines]
              (map #(pad-tokens % 1) lines)))
       (map (fn [lines]
              (map #(partition 2 1 %) lines)))))

(defn text->ngrams
  "Takes text from a file, including newlines.
  Pads lines with <s> and </s> for start/end of line.
  Pads beginning with n - 1 <s>s"
  [text n]
  (let [words-not-in-dict-filter (remove-sentences-with-words-not-in-dictionary dict/popular)]
    (->> text
         util/clean-text
         (#(string/split % #"\n+"))
         (remove empty?)
         (map tokenize-line)
         (map #(pad-tokens % n))
         (map #(partition n 1 %))
         (apply concat))))

(defn n-to-m-grams
  "Exclusive of m, similar to range."
  [n m text]
  (loop [i n
         r '()]
    (cond
      (= i m)
      (apply concat r)
      :else
      (recur (inc i) (cons (text->ngrams text i) r)))))

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
                        (let [k' (get db k i)
                              i' (if (= i k') (inc i) i)
                              db' (-> db
                                      (assoc k' k)
                                      (assoc k k'))]
                          [db' i']))
                      [db i]
                      k)]
            (let [k' (map #(get db %) k)]
              (if-let [existing (get trie k')]
                (let [[val count] existing
                      trie (assoc trie k' [val (inc count)])]
                  [trie i db])
                [(assoc trie k' [i 1]) i db]))))
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
 (let [texts (->> (dark-corpus-file-seq 0 250000)
                  (map slurp))
       [trie _ db] (create-trie-from-texts texts)]
   (def trie trie)
   (def trie-database db)))

(defn encode-fn [v]
  (let [[value count] (if (seqable? v) v [nil nil])]
    (if (nil? value)
      (encoding/encode 0)
      (byte-array
       (concat (encoding/encode value)
               (encoding/encode count))))))

(defn decode-fn [byte-buffer]
  (let [value (encoding/decode byte-buffer)]
    (if (zero? value)
      [nil nil]
      [value (encoding/decode byte-buffer)])))

(time
 (def tightly-packed-trie
   (tpt/tightly-packed-trie
    trie
    encode-fn
    decode-fn)))

(take 20 tightly-packed-trie)

(defn key-get-in-tpt [tpt db ks]
  (let [id (map #(get-in db [(list %) :id]) ks)
        v (get tpt id)]
    {id v}))

(defn id-get-in-tpt [tpt db ids]
  (let [ks (apply concat (map #(get db %) ids))
        v (get tpt ids)
        id (get-in db [ks :id])]
    {ks (assoc v :value (get db id))}))

(time (count (map #(get % []) (trie/children (trie/lookup tightly-packed-trie [1])))))
(time (count (trie/children (trie/lookup tightly-packed-trie [1]))))

(comment
  (profile {}
   (def example-story
     (loop [generated-text [(get trie-database "<s>")]
            i 0]
       (if (> i 10)
         generated-text
         (let [children (loop [i 4]
                          (let [node (p :lookup
                                        (trie/lookup
                                         tightly-packed-trie
                                         (vec (take-last i generated-text))))
                                children (p :seq-children (doall (seq (and node (trie/children node)))))]
                            (cond
                              (nil? node) (recur (dec i))
                              (< i 0) (throw (Exception. "Error"))
                              children children
                              :else (recur (dec i)))))]
           (recur
            (conj
             generated-text
             (->> children
                  (map #(get % []))
                  (remove nil?)
                  (#(p :weighted-selection (math/weighted-selection :count %)))
                  :value
                  (get trie-database)))
            (inc i)))))))

  (->> example-story
       (map #(get-in trie-database [% :value]))
       (concat)
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
