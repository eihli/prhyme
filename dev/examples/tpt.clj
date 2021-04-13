(ns examples.tpt
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.trie.math :as math]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [clojure.zip :as zip]))

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
       (map #(n-to-m-grams 1 4 %))
       (apply concat)
       (map prep-ngram-for-trie)
       (reduce
        (fn [trie [k v]]
          (let [existing (or (get trie k) {:count 0 :value (last v)})]
            (conj trie [k (update existing :count inc)])))
        (trie/make-trie))))

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

(defn trie->database [trie]
  (let [sorted-keys
        (seq-of-nodes->sorted-by-count trie)]
    (loop [sorted-keys sorted-keys
           database {}
           i 1]
      (if (empty? sorted-keys)
        database
        (recur
         (rest sorted-keys)
         (-> database
             (assoc i {:count (:count (first sorted-keys))
                       :value (:value (first sorted-keys))})
             (assoc (:value (first sorted-keys)) i))
         (inc i))))))

(def trie
  (let [texts (->> (dark-corpus-file-seq 500 500)
                   (map slurp))]
    (create-trie-from-texts texts)))

(def trie-database
  (trie->database trie))

(defn encode-fn [v]
  (let [{:keys [count value]} v]
    (if (and (number? v) (not (zero? v)))
      (byte-array
       (concat (encoding/encode (trie-database value))
               (encoding/encode count)))
      (encoding/encode 0))))

(defn decode-fn [byte-buffer]
  (let [v (encoding/decode byte-buffer)]
    (if (and (number? v) (zero? v))
      nil
      (trie-database v))))

(comment
  (def tight-ready-trie
    (->> trie
         (map (fn [[k v]]
                (let [k (map #(get trie-database %) k)]
                  [k v])))
         (into (trie/make-trie))))
  )

(def tightly-packed-trie
  (let [tight-ready-trie
        (->> trie
             (map (fn [[k v]]
                    (let [k (map #(get trie-database %) k)]
                      [k v])))
             (into (trie/make-trie)))
        tightly-packed-trie
        (tpt/tightly-packed-trie
         tight-ready-trie
         encode-fn
         decode-fn)]
    tight-ready-trie))

(defn key-get-in-tpt [tpt db ks]
  (let [id (map #(get-in db [(list %) :id]) ks)
        v (get tpt id)]
    (println id ks)
    {id v}))

(defn id-get-in-tpt [tpt db ids]
  (let [ks (apply concat (map #(get db %) ids))
        v (get tpt ids)
        id (get-in db [ks :id])]
    {ks (assoc v :value (get db id))}))

(comment
  (trie/lookup tightly-packed-trie [1 28 9])


  (def example-story
    (loop [generated-text [(get trie-database "<s>")]
           i 0]
      (if (> i 100)
        generated-text
        (let [node (loop [i 3]
                     (let [node (trie/lookup
                                 tightly-packed-trie
                                 (vec (take-last i generated-text)))]
                       (cond
                         (nil? node) (recur (dec i))
                         (< i 0) (throw (Exception. "Error"))
                         (seq (trie/children node)) node
                         :else (recur (dec i)))))]
          (recur
           (conj
            generated-text
            (->> node
                 trie/children
                 (map #(get % []))
                 (remove nil?)
                 (math/weighted-selection :count)
                 :value
                 (get trie-database)))
           (inc i))))))

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
  ;; database
  (let [texts (->> (dark-corpus-file-seq 500 2)
                   (map slurp))
        trie (create-trie-from-texts texts)]
    (->> (trie->database trie)
         (#(get % 3))))

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
