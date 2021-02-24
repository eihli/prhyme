(ns com.owoga.prhyme.rhyme-trie
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [com.owoga.prhyme.data.tpt :as tpt])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream
                    DataOutputStream DataInputStream)))

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
    {parent-key (assoc parent-val :children (into {} (map vec-trie->map-trie children)))}))

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

(defprotocol ITrie
  (as-map [this] "Map that underlies trie.")
  (as-vec [this] "Depth-first post-order vector")
  (transform [this f] "Depth-first post-order apply each function to each node."))

;; Seq offers a depth-first post-order traversal
;; with children ordered by key.
(deftype Trie [trie]
  ITrie
  (as-map [_] trie)
  (as-vec [_] (map-trie->seq-trie trie))
  (transform [self f]
    (->> self
         as-vec
         zip/vector-zip
         (zip-visitor f)
         (vec-trie->map-trie)
         (Trie.)))

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
       (update-in
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
  ([] (->Trie {}))
  ([& entries]
   (reduce
    (fn [t entry]
      (conj t entry))
    (trie)
    entries)))

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

(defn previous-node [loc]
  (loop [loc (zip/prev loc)]
    (cond
      (nil? loc) nil
      (map? (zip/node loc)) loc
      :else (recur (zip/prev loc)))))

(defn loc->byte-address
  "Given a loc without a byte-address, calculate it from the previous loc."
  [loc]
  (let [prev (previous-node loc)]
    (if prev
      (let [[k {:keys [byte-address bytes]}] (first (seq (zip/node prev)))]
        (+ byte-address (count bytes)))
      0)))

(defn child->index
  "Given a child gets a map with info needed to build an index."
  [child]
  (let [[k {:keys [byte-address] :as v}] (first (seq child))]
    {:byte-address byte-address
     :key k}))

(child->index {"T" {:byte-address 20}})

(defn pack-index
  ""
  [loc]
  (println "pack" (zip/node loc))
  (if (map? (zip/node loc))
    (let [parent-byte-address (loc->byte-address loc)
          _ (println parent-byte-address)
          child-nodes (->> loc
                           child-seq
                           (map (comp second zip/node)))
          _ (println child-nodes)
          child-key-offsets (map
                             (fn [child-node]
                               (let [{child-byte-address :byte-address
                                      child-key :key} (child->index child-node)]
                                 {:key child-key
                                  :offset (- parent-byte-address child-byte-address)}))
                             child-nodes)]
      (zip/edit
       loc
       (fn [node]
         (println "zip" node)
         (let [[k v] (first (seq node))]
           {k {:byte-address parent-byte-address
               :bytes (byte-array [1 2 3])}}))))
    loc))

(comment
  (let [v1 '("T" "A" "T" "TAT")
        v2 '("T" "U" "T" "TUT")
        v3 '("T" "A" "AT")
        t1 (trie v1)
        t2 (trie v2)
        t3 (trie v1 v2 v3)
        vect (as-vec t3)]
    (transform t3 pack-index))

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

