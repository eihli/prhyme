(ns com.owoga.prhyme.data-transform
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [taoensso.nippy :as nippy]))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn xf-file-seq [start end]
  (comp (remove #(.isDirectory %))
        (drop start)
        (take end)))

(defn make-token-padder
  [beginning-token
   end-token
   number-of-beginning-tokens
   number-of-end-tokens]
  (let [beginning-pad (repeat number-of-beginning-tokens beginning-token)
        end-pad (repeat number-of-end-tokens end-token)]
    (fn [tokens]
      (into (vec beginning-pad) (into tokens end-pad)))))

(defn xf-pad-tokens [num-beg beg-tok num-end end-tok]
  (map (make-token-padder beg-tok end-tok num-beg num-end)))

(def xf-tokenize
  (comp
   (map string/trim)
   (map (partial re-seq re-word))
   (map (partial map second))
   (map (partial mapv string/lower-case))))

(def xf-filter-english
  (let [word? (fn [x] (or (#{"." "?" ","} x)
                          (dict/cmu-with-stress-map x)))]
    (filter (partial every? word?))))

(defn n-to-m-partitions
  "Exclusive of m, similar to range."
  [n m partitions]
  (mapcat
   #(partition % 1 partitions)
   (range n m)))

(defn new-key [database k]
  (let [next-id (@database :next-id)]
    (swap!
     database
     #(-> %
          (assoc k next-id)
          (assoc next-id k)
          (update :next-id inc)))
    next-id))

(defn make-database-processor
  "Takes an atom and returns a function that takes a Trie key/value.
  When the returned function is called, it checks to see
  if the key is in the database and if so it returns the associated id.
  If not, it increments the id (which is stored in the database
  under :next-id) and returns that new id."
  [database]
  (fn [[k v]]
    (let [k' (map (fn [kn]
                    (if-let [id (get @database kn)]
                      id
                      (new-key database kn)))
                  k)]
      [k' 1])))

(def encode-fn
  "Encodes a number as a variable-length encoded value.
  nil gets encoded as 0."
  (fnil encoding/encode 0))

(defn decode-fn
  "Decodes a variable-length encoded number from a byte-buffer.
  Zero gets decoded to nil."
  [byte-buffer]
  (let [value (encoding/decode byte-buffer)]
    (if (zero? value)
      nil
      value)))

(defn file-seq->trie
  "Transduces text files into an n-to-m-gram trie.

  Takes an atom as a database and updates it to
  map integer ids to their string representations and
  strings to their integer ids.

  Splits text on newline, question marks, and periods. Pads
  each split with <s> and </s>.

  Currently configured for backwards trie for generation starting from the
  end of a sentence. To reconfigure, remove the map partial map reverse and change
  the count of the beginning/end pads."
  [database files n m]
  (transduce
   (comp
    (map slurp)
    (map #(string/split % #"[\n+\?\.]"))
    (map (partial transduce xf-tokenize conj))
    (map (partial transduce xf-filter-english conj))
    (remove empty?)
    (map (partial transduce (xf-pad-tokens 1 "<s>" (dec m) "</s>") conj))
    (map (partial map reverse))
    (mapcat (partial map (partial n-to-m-partitions n (inc m))))
    (mapcat (partial into []))
    (map #(clojure.lang.MapEntry. (vec %) %))
    (map (make-database-processor database)))
   (completing
    (fn [trie [k v]]
      (update trie k (fnil inc 0))))
   (trie/make-trie)
   files))

(defn trie->tightly-packed-trie
  [trie encode-fn decode-fn]
  (tpt/tightly-packed-trie trie encode-fn decode-fn))

(comment
  (time
   (let [database (atom {:next-id 1})
         trie (transduce
               (comp (xf-file-seq 0 250000)
                     (map slurp)
                     (map #(string/split % #"[\n+\?\.]"))
                     (map (partial transduce xf-tokenize conj))
                     (map (partial transduce xf-filter-english conj))
                     (remove empty?)
                     (map (partial transduce (xf-pad-tokens 1 "<s>" 3 "</s>") conj))
                     (map (partial map reverse))
                     (mapcat (partial map (partial n-to-m-partitions 1 5)))
                     (mapcat (partial into []))
                     (map #(clojure.lang.MapEntry. (vec %) %))
                     (map (make-database-processor database)))
               (completing
                (fn [trie [k v]]
                  (update trie k (fnil inc 0))))
               (trie/make-trie)
               (file-seq (io/file "dark-corpus")))
         tpt (tpt/tightly-packed-trie trie encode-fn decode-fn)]
     (tpt/save-tightly-packed-trie-to-file "/tmp/tpt.bin" tpt)
     (nippy/freeze-to-file "/tmp/db.bin" @database)))

  (time
   (let [database (nippy/thaw-from-file "/tmp/db.bin")
         tpt (tpt/load-tightly-packed-trie-from-file "/tmp/tpt.bin" decode-fn)]
     (->> tpt
          (take-last 10)
          (map (fn [[k v]] [k (map database k) v])))))

  )
