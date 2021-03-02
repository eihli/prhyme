(ns com.owoga.prhyme.rhyme-trie
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [com.owoga.prhyme.data.tpt :as tpt])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream
                    DataOutputStream DataInputStream)))

(defprotocol IByteArrayCursor
  (loc [_] "Current index location of cursor.")
  (jump [_ loc] "Moves location of cursor to specified index.")
  (forward [_] [_ n] "Jump forward n bytes.")
  (backward [_] [_ n] "Jump backward n bytes.")
  (slice [_ end] "A view of the byte array between loc and end.")
  (ba= [_ other-ba] "Compares for identical equality the backing byte-array."))

(deftype ByteArrayCursor [ba loc]
  clojure.lang.Indexed
  (nth [_ i]
    (if (and (>= i 0)
             (< i (count ba)))
      (ByteArrayCursor. ba i)
      (throw (ex-info "Index out of bounds."))))
  (nth [self i not-found]
    (if (and (>= i 0)
             (< i (count ba)))
      (ByteArrayCursor. ba i)
      not-found))

  IByteArrayCursor
  (loc [_] loc)
  (jump [_ loc] (ByteArrayCursor. ba loc))
  (forward [_ n] (ByteArrayCursor. ba (+ loc n)))
  (backward [_ n] (ByteArrayCursor. ba (- loc n)))
  (slice [_ n]
    (loop [i 0 r []]
      (if (or (= i n)
              (>= (+ loc i) (count ba)))
        (ByteArrayCursor. (byte-array r) 0)
        (recur (inc i) (conj r (aget ba (+ i loc)))))))

  clojure.lang.ILookup
  (valAt [_ i]
    (if (or (>= i (count ba))
            (< i 0))
      (throw "Index out of boundes.")
      (aget ba i)))
  (valAt [_ i not-found]
     (if (or (>= i (count ba))
            (< i 0))
      not-found
      (aget ba i)))

  clojure.lang.IPersistentCollection
  (count [_] (count ba))
  (cons [_ o] (ByteArrayCursor. (byte-array (concat ba [o])) loc))
  (empty [_] (ByteArrayCursor. (byte-array 0) 0))
  (equiv [self o] (and (ba= o ba)
                       (= loc (loc o))))

  clojure.lang.ISeq
  (first [self] (self))
  (next [self] (ByteArrayCursor. ba (inc loc)))
  (more [_] (ByteArrayCursor. ba (inc loc)))

  clojure.lang.IPersistentStack
  (peek [self] (aget ba loc))
  (pop [_] (ByteArrayCursor. ba (inc loc)))

  clojure.lang.Seqable
  (seq [_] (let [seeker (fn seeker [ba i]
                          (if (>= i (count ba))
                            nil
                            (lazy-seq
                             (cons
                              (aget ba i)
                              (seeker ba (inc i))))))]
             (seeker ba loc))))

(comment
  (let [bac1 (->ByteArrayCursor (byte-array (range 100)) 0)
        bac2 (->ByteArrayCursor (byte-array (range 100 200)) 0)
        bac3 (take-last 5 (concat bac1 bac2))
        bac4 (take-last 5 (conj bac1 111))]
    [(peek bac1)
     (take 5 (next bac1))
     (take 5 (rest bac1))
     bac3
     bac4])

  )

(defn branch? [node]
  (and (map? node)
       (:children (first (vals node)))))

(defn children [node]
  (map (partial apply hash-map) (seq (:children (first (vals node))))))

(defn without-children [node]
  {(first (keys node))
   (dissoc (get node (first (keys node))) :children)})

(defn map-trie->seq-trie
  [trie]
  [(vec (map map-trie->seq-trie (children trie)))
   (without-children trie)])

(let [m {:root
         {:children
          {"T"
           {:children
            {"A" {:children
                  {"T" {:value "TAT", :count 1}
                   "S" {:value "SAT" :count 1}}
                  :value "AT"
                  :count 1},
             "U" {:children {"T" {:value "TUT", :count 1}}}}}}}}]
  (let [z (zip/vector-zip (map-trie->seq-trie m))]
    (->> z
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node))))

(defn vec-trie->map-trie
  [trie]
  (let [children (first trie)
        parent (second trie)
        [parent-key parent-val] (first (seq parent))]
    (sorted-map
     parent-key
     (assoc parent-val :children (into (sorted-map) (map vec-trie->map-trie children))))))

(comment
  (let [vect [[[[[[[[] {"T" {:value "TAT", :count 1}}]] {"A" {:value "AT", :count 1}}]
                 [[[[] {"T" {:value "TUT", :count 1}}]] {"U" {}}]]
                {"T" {}}]]
              {:root {}}]]
    (vec-trie->map-trie vect))

  )

(comment
  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)
        vect (as-vec t3)]
    vect)

  )

(defn parent?
  [node]
  (and (vector? (first node))
       (map? (second node))))

(defn child-seq
  "Takes a zipper loc and returns seq of children locs."
  [loc]
  (if (and (zip/left loc)
           (zip/down (zip/left loc)))
    ((fn inner [child]
       (if child
         (lazy-seq
          (cons child
                (inner (zip/right child))))
         nil))
     (->> loc zip/left zip/down))
    '()))

(defn zip-visitor
  ([visitor zipper]
   (loop [zipper zipper]
     (if (zip/end? zipper)
       (zip/root zipper)
       (recur (zip/next (visitor zipper)))))))

(comment
  (let [m {:root
           {:children
            {"T"
             {:children
              {"A" {:children
                    {"T" {:value "TAT", :count 1}
                     "S" {:value "SAT" :count 1}}
                    :value "AT"
                    :count 1},
               "U" {:children {"T" {:value "TUT", :count 1}}}}}}}}]
    (let [z (zip/vector-zip (map-trie->seq-trie m))
          pred (fn [loc]
                 (map? (zip/node loc)))]
      (zip-visitor
       (fn [loc]
         (if (pred loc)
           (zip/edit
            loc
            (fn [node]
              (let [[k v] (first (seq node))
                    children-counts (->> (child-seq loc)
                                         (map zip/node)
                                         (map (comp :count second first seq second)))]
                (if (not-empty children-counts)
                  (update-in node [k :count] (partial apply (fnil + 0)) children-counts)
                  node))))
           loc))
       z)))

  )

;;;; Tightly Packed Trie
;; The following functions are helpers for converting
;; a trie into a tightly-packed byte-array.

(defn previous-node [loc]
  (loop [loc (zip/prev loc)]
    (cond
      (nil? loc) nil
      (map? (zip/node loc)) loc
      :else (recur (zip/prev loc)))))

(defn loc->byte-address
  "Given a loc without a byte-address, calculate it from the previous loc."
  ([loc]
   (loc->byte-address loc 0))
  ([loc starting-offset]
   (let [prev (previous-node loc)]
       (if prev
         (let [[k {:keys [byte-address byte-array]}] (first (seq (zip/node prev)))]
           (+ byte-address (count byte-array)))
         starting-offset))))

(defn child->index
  "Given a child gets a map with info needed to build an index."
  [child]
  (let [[k {:keys [byte-address byte-array] :as v}] (first (seq child))]
    {:byte-address byte-address
     :key k
     :byte-array byte-array}))

(defn visitor-filter [pred visitor]
  (fn [loc]
    (if (pred loc)
      (visitor loc)
      loc)))

(def offset-flag 0)

(defn offset-byte? [b]
  (= offset-flag (tpt/nth-bit 8 b)))

(defn encode-offset
  "0-padded"
  [n]
  (tpt/encode-with-flag-bits "0" n))

(defn decode-offset
  [ba]
  (tpt/decode-with-flag-bits "0" ba))

(defn decode-offset-cursor [cursor]
  (let [bytes (take-while offset-byte? cursor)
        [decoded byte-count] (tpt/decode-with-flag-bits "0" (byte-array bytes))]
    [decoded (forward cursor byte-count)]))

(defn encode-key
  "1-padded"
  [n]
  (tpt/encode-with-flag-bits "1" n))

(def key-flag 1)

(defn key-byte? [b]
  (= key-flag (tpt/nth-bit 8 b)))

(defn decode-key [ba]
  (let [bytes (take-while key-byte? ba)]
    (tpt/decode-with-flag-bits "1" (byte-array bytes))))

(defn decode-key-cursor [cursor]
  (let [bytes (take-while key-byte? cursor)
        [decoded byte-count] (tpt/decode-with-flag-bits "1" (byte-array bytes))]
    [decoded (forward cursor byte-count)]))

(comment
  ;; Test encoding and decoding key/offset pairs.
  ;; Cursor works just the same as a byte-array but
  ;; instead of the decodes returning a byte-count
  ;; it returns a cursor with a pointer to the new location.
  (let [key (encode-key 128)
        offset (encode-offset 65537)
        ba (byte-array (concat key offset))
        cursor (->ByteArrayCursor
                (byte-array (concat (byte-array 3) ba (byte-array (repeat 3 -1))))
                3)
        [decoded-key key-bytecount] (decode-key ba)
        [decoded-offset offset-bytecount] (decode-offset (drop key-bytecount ba))
        [decoded-key-cursor new-cursor] (decode-key-cursor cursor)
        [decoded-offset-cursor new-cursor] (decode-offset-cursor new-cursor)]
    [decoded-key
     decoded-offset
     decoded-key-cursor
     decoded-offset-cursor])
  ;; => [128 65537 128 65537]
  (let [key (encode-key 128)
        offset (encode-offset 65537)
        bac (->ByteArrayCursor (byte-array (concat key offset)) 0)]
    (take 2 ba))
  )

(defn pack-node-value
  "Returns byte-array of node value.
  Byte-array is 2 variable-length encoded integers.
  For a markov trie, this would be an integer ID
  of the n-gram and an integer of the frequency.

  Nodes without terminal values get a value and count of 0."
  [node]
  (let [baos (ByteArrayOutputStream.)]
    (.write baos (tpt/vb-encode (get node :value 0)))
    (.write baos (tpt/vb-encode (get node :count 0)))
    (.toByteArray baos)))

(defn unpack-node-value
  [])
(defn pack-index-entry
  [child]
  (let [baos (ByteArrayOutputStream.)]
    (.write baos (encode-key (:key child)))
    (.write baos (encode-offset (:offset child)))
    (.toByteArray baos)))

(defn unpack-index-entry
  [ba i]
  (let [cursor (->ByteArrayCursor ba i)
        [key offset] (decode-key )]))

(let [some-array (make-array Integer/TYPE 3)]
  (drop 1 some-array))

(defn pack-index
  "Reserves 4 bytes for offset of root node.
  Lots of mutation going on here as we write to baos."
  [loc]
  (let [baos (ByteArrayOutputStream.)
        byte-address (loc->byte-address loc 4)
        child-nodes (->> loc
                         child-seq
                         (map (comp second zip/node)))
        children (map
                  (fn [child-node]
                    (let [child-index (child->index child-node)]
                      (assoc
                       child-index
                       :offset
                       (- byte-address (:byte-address child-index)))))
                  child-nodes)
        index-ba (let [index-baos (ByteArrayOutputStream.)
                       child-byte-arrays (map pack-index-entry children)]
                   (loop [bas child-byte-arrays]
                     (if (empty? bas)
                       (.toByteArray index-baos)
                       (do (.write index-baos (first bas))
                           (recur (rest bas))))))]
    (zip/edit
     loc
     (fn [node]
       (let [[k v] (first (seq node))]
         (.write baos (pack-node-value v))
         (.write baos (tpt/vb-encode (count index-ba)))
         (.write baos index-ba)
         {k (conj v {:byte-address byte-address
                     :byte-array (.toByteArray baos)})})))))

(let [baos (ByteArrayOutputStream.)]
  (.write baos (.toByteArray (ByteArrayOutputStream.))))

(defn update-in-sorted
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  {:added "1.0"
   :static true}
  ([m ks f & args]
     (let [up (fn up [m ks f args]
                (let [m (or m (sorted-map))
                      [k & ks] ks]
                  (assert (instance? clojure.lang.PersistentTreeMap m)
                          (apply str
                                 "A non-sorted hash-map in a sorted"
                                 "hash-map will probably be the seed of some problems."))
                  (if ks
                    (assoc m k (up (get m k) ks f args))
                    (assoc m k (apply f (get m k) args)))))]
       (up m ks f args))))

(defprotocol ITrie
  (as-map [this] "Map that underlies trie.")
  (as-vec [this] "Depth-first post-order vector.")
  (as-byte-array [this] "Tightly-packed byte-array.")
  (transform [this f] "Depth-first post-order apply each function to each node."))

;; Seq offers a depth-first post-order traversal
;; with children ordered by key.
(deftype Trie [trie]
  ITrie
  (as-map [_] trie)
  (as-vec [_] (map-trie->seq-trie trie))
  (as-byte-array [self]
    (->> (transform self (visitor-filter #(map? (zip/node %)) pack-index))
         as-vec
         vec-trie->map-trie
         (Trie.)))
  (transform [self f]
    (->> self
         as-vec
         zip/vector-zip
         (zip-visitor f)
         (vec-trie->map-trie)
         (Trie.)))

  clojure.lang.ILookup
  (valAt [_ k]
    (get-in trie (cons :root (interleave (repeat :children) k))))
  (valAt [_ k not-found]
    (get-in trie (cons :root (interleave (repeat :children) k)) not-found))

  clojure.lang.IPersistentCollection
  (seq [self]
    (->> self
         as-vec
         zip/vector-zip
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (filter map?)
         (filter (comp :value second first seq))))
  (cons [_ o]
    (let [path (cons :root (interleave (repeat :children) (butlast o)))
          id (last o)
          node (get-in trie path)]
      (Trie.
       (update-in-sorted
        trie
        path
        (fn [prev]
          (if (nil? prev)
            {:value id
             :count 1}
            (-> prev
                (assoc :value id)       ; Assert value same?
                (update :count (fnil inc 0)))))))))
  (empty [_] (Trie. {}))
  (equiv [_ o]
    (and (isa? (class o) Trie)
         (= (as-map o) trie))))

(defn trie
  ([] (->Trie (sorted-map)))
  ([& entries]
   (reduce
    (fn [t entry]
      (conj t entry))
    (trie)
    entries)))

(defn mapped-byte-array-trie->byte-array
  [trie]
  (let [baos (ByteArrayOutputStream.)]
    (->> trie
         as-vec
         zip/vector-zip
         (iterate zip/next)
         (take-while (complement zip/end?))
         (filter (comp map? zip/node))
         ((fn [nodes]
            (loop [nodes nodes]
              (if (empty? nodes)
                baos
                (let [node (first nodes)]
                  (.write baos (:byte-array node))
                  (recur (rest nodes))))))))))


(defn bytes->int [byte-arr]
  (reduce
   (fn [acc b]
     (bit-or
      (bit-and 0xFF b)
      (bit-shift-left acc 8)))
   0
   byte-arr))

(defn decode-key [bb max-position]
  (let [slice (partial tpt/bit-slice 0 7)
        combine (partial tpt/combine-significant-bits 7)]
    (loop [bytes []]
      (cond
        (or (< max-position (.position bb))
            (zero? (.remaining bb)))
        (apply combine (map slice bytes))

        (offset-byte? (.get bb (.position bb)))
        (apply combine (map slice bytes))

        :else
        (recur (conj bytes (.get bb)))))))

(defn decode-offset [bb max-position]
  (let [slice (partial tpt/bit-slice 0 7)
        combine (partial tpt/combine-significant-bits 7)]
    (loop [bytes []]
      (cond
        (or (< max-position (.position bb))
            (zero? (.remaining bb)))
        (apply combine (map slice bytes))

        (key-byte? (.get bb (.position bb)))
        (apply combine (map slice bytes))

        :else
        (recur (conj bytes (.get bb)))))))

(defn rewind-to-key [bb stop]
  (loop []
    (let [current (.get bb (.position bb))
          previous (.get bb (dec (.position bb)))]
      (if (or (= stop (.position bb))
              (and (key-byte? current)
                   (offset-byte? previous)))
        bb
        (do (.position bb (dec (.position bb)))
            (recur))))))

(defn find-key-in-index
  [bb target-key max-address not-found]
  (loop [previous-key nil
         min-position (.position bb)
         max-position max-address]
    (if (zero? (- max-position min-position))
      not-found
      (let [mid-position (+ min-position (quot 2 (- max-position min-position)))]
        (.position bb mid-position)
        (let [bb (rewind-to-key bb min-position)
              current-key (decode-key bb max-position)]
          (cond
            (= current-key target-key)
            (decode-offset bb max-position)

            (= current-key previous-key)
            (throw "Key not found.")

            (< current-key target-key)
            (recur
             current-key
             max-position
             (+ mid-position (quot 2 (- max-position mid-position))))

            (> current-key target-key)
            (recur
             current-key
             min-position
             (+ min-position (quot 2 (- mid-position min-position))))))))))

(deftype TightlyPackedTrie [byte-buffer]
  clojure.lang.ILookup
  (valAt [_ ks]
    (let [root-address (.getInt byte-buffer 0)
          orig-ks ks]
      (.position byte-buffer root-address)
      (loop [ks ks]
        (let [current-address (.position byte-buffer)]
          (if (empty? ks)
            (let [value (tpt/byte-buffer-variable-length-decode byte-buffer)
                  freq (tpt/byte-buffer-variable-length-decode byte-buffer)]
              {:value value
               :count freq})
            (let [val (tpt/byte-buffer-variable-length-decode byte-buffer)
                  freq (tpt/byte-buffer-variable-length-decode byte-buffer)
                  size-of-index (tpt/byte-buffer-variable-length-decode byte-buffer)
                  offset (find-key-in-index
                          byte-buffer
                          (first ks)
                          (+ (.position byte-buffer) size-of-index)
                          :not-found)]
              (if (= offset :not-found)
                (throw (Exception. (format "Index not found %s" orig-ks)))
                (do (.position byte-buffer (- current-address offset))
                    (recur (rest ks))))))))))
  (valAt [_ ks not-found]
    (let [root-address (.getInt byte-buffer 0)
          orig-ks ks]
      (.position byte-buffer root-address)
      (loop [ks ks]
        (let [current-address (.position byte-buffer)]
          (if (empty? ks)
            (let [value (tpt/byte-buffer-variable-length-decode byte-buffer)
                  freq (tpt/byte-buffer-variable-length-decode byte-buffer)]
              {:value value
               :count freq})
            (let [val (tpt/byte-buffer-variable-length-decode byte-buffer)
                  freq (tpt/byte-buffer-variable-length-decode byte-buffer)
                  size-of-index (tpt/byte-buffer-variable-length-decode byte-buffer)
                  offset (find-key-in-index
                          byte-buffer
                          (first ks)
                          (+ (.position byte-buffer) size-of-index)
                          :not-found)]
              (if (= offset :not-found)
                not-found
                (do (.position byte-buffer (- current-address offset))
                    (recur (rest ks)))))))))))


(comment
  (let [v1 '(1 2 1 121)
        v2 '(1 3 1 131)
        v3 '(1 2 12)
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie '(1 2 2 122) v1 '(1 2 3 123) v2 v3)
        vect (as-vec t3)
        packed (tightly-packed-trie t3)
        tpt (->TightlyPackedTrie packed)]
    (as-map (as-byte-array t3))
    (get tpt '(1 2 1)))

  )

(defn tightly-packed-trie
  "Assumes 4 bytes for root byte address."
  [trie]
  (let [baos (ByteArrayOutputStream.)
        trie (as-byte-array trie)]
    (transform
     trie
     (visitor-filter
      #(map? (zip/node %))
      (fn [loc]
        (let [{:keys [byte-array]} (second (first (seq (zip/node loc))))]
          (.write baos byte-array)
          loc))))
    (let [ba (.toByteArray baos)
          root-address (get-in (as-map trie) [:root :byte-address])
          byte-buf (java.nio.ByteBuffer/allocate (+ 4 (count ba)))]
      (.putInt byte-buf root-address)
      (.put byte-buf ba)
      byte-buf)))

(comment
  (let [v1 '(1 2 1 121)
        v2 '(1 3 1 131)
        v3 '(1 2 12)
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie '(1 2 2 122) v1 '(1 2 3 123) v2 v3)
        vect (as-vec t3)
        packed (tightly-packed-trie t3)]
    (map byte (.array packed)))

  )

(comment
  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)]
    (seq t3))

  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)
        pred (fn [loc]
               (map? (zip/node loc)))]
    (transform
     t3
     (fn [loc]
       (if (pred loc)
         (zip/edit
          loc
          (fn [node]
            (let [[k v] (first (seq node))
                  children-counts (->> (child-seq loc)
                                       (map zip/node)
                                       (map (comp :count second first seq second)))]
              (if (not-empty children-counts)
                (update-in node [k :count] (partial apply (fnil + 0)) children-counts)
                node))))
         loc))))

  )

(defn vec->trie
  [v]
  (let [zipper (zip/vector-zip v)]
    (->> zipper
         (iterate zip/next)
         (take-while (complement zip/end?))
         (filter (comp map? zip/node))
         #_(map #(concat (zip/path %) [(->> % zip/node keys first)
                                       (->> % zip/node vals first :value)]))
         (map zip/path))))

(comment
  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)
        vect (as-vec t3)]
    (->> (zip/vector-zip (as-vec t3))
         (iterate zip/next)
         (take-while (complement zip/end?))
         (filter (comp map? zip/node))
         first
         ))

  )



(comment
  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)
        vect (as-vec t3)]
    (->> (zip/vector-zip (as-vec t3))
         (iterate zip/next)
         (take-while (complement zip/end?))
         (filter (comp map? zip/node))
         second
         loc->byte-address))

  )

(defn pack-nodes [trie]
  (let [leaf? (comp map? zip/node)]
    (transform
     trie
     (fn [loc]
       (if (leaf? loc)
         (zip/edit
          (fn [node]
            (let [[k {:keys [count value]}] (first (seq node))
                  encoded-value (tpt/vb-encode value)
                  encoded-count (tpt/vb-encode count)
                  child-bytes (->> loc
                                   child-seq
                                   (map (comp first seq second zip/node)))]
              ))))))))

(defn write-node [baos node])
(defn write-index [baos children])

(defn pack-index-to-children [children]
  (let [baos (ByteArrayOutputStream.)]
    (run!
     (fn [[index-key byte-address]]
       (.write baos index-key)
       (.write baos byte-address))
     children)
    (.toByteArray baos)))

(defn node->byte-array [index-key node-value children]
  (let [baos (ByteArrayOutputStream.)
        child-index (pack-index-to-children children)]
    (.write baos node-value)
    (.write baos (count child-index))
    (.writeBytes baos child-index)
    (.toByteArray baos)))

