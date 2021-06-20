(ns com.owoga.prhyme.data-transform
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [taoensso.nippy :as nippy]
            [com.owoga.prhyme.nlp.tag-sets.treebank-ii :as tb2]
            [clojure.zip :as zip]
            [cljol.dig9 :as d]
            [com.owoga.prhyme.util.math :as math]))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\?|\n)")

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

(def xf-untokenize
  (comp
   (map #(string/join " " %))
   (map #(string/replace % #" (['\-,\?\.] ?)" "$1"))))

(comment
  (let [tokens (transduce
                xf-tokenize
                conj
                ["Eric's name, is Bond." "James, bond? Yes."])]
    [tokens
     (map #(string/join " " %) tokens)
     (transduce
      xf-untokenize
      conj
      tokens)])

  )

(def xf-filter-english
  (let [word? (fn [x] (or (#{"." "?" ","} x)
                          (dict/cmu-with-stress-map x)))]
    (filter (partial every? word?))))

(defn n-to-m-partitions
  "Returns a concatenated list of n-partitions, n+1-partitions, ..., m-1-partitions of coll.
  Exclusive of m, similar to range."
  [n m coll]
  (mapcat
   (fn [partition-size]
     (partition partition-size 1 coll))
   (range n m)))

(comment
  (n-to-m-partitions 1 4 (range 6))
  ;; => ((0)
  ;;     (1)
  ;;     ,,,
  ;;     (3 4)
  ;;     (4 5)
  ;;     ,,,
  ;;     (2 3 4)
  ;;     (3 4 5))
  )

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

  Expects `database` to have a `:next-id` key, which should start at 1
  so that 0 can remain the id for the root node of the trie. That is important
  for the encode/decode functions.

  When the returned function is called, it checks to see
  if the key is in the database and if so it returns the associated id.
  If not, it increments the id (which is stored in the database
  under :next-id) and returns that new id."
  [database]
  (fn [[k v]]
    (let [k' (mapv (fn [kn]
                     (if-let [id (get @database kn)]
                       id
                       (new-key database kn)))
                   k)]
      [k' 1])))

(comment
  ;; TODO: Move to nlp.core
  (defn xf-part-of-speech-database
   [database]
   (fn [sentence]
     (let [leafs (->> sentence
                      nlp/treebank-zipper
                      nlp/leaf-pos-path-word-freqs)]
       (run!
        (fn [[k v]]
          (swap!
           database
           assoc
           k
           (merge-with + (@database k) v)))
        leafs)
       sentence))))

(comment
  (let [database (atom {:next-id 0})]
    (transduce
     (map (partial mapv (part-of-speech-database database)))
     conj
     []
     [["this test is difficult"]
      ["this foot is sore"]])
    @database)

  )

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
      (update trie k (fnil #(update % 1 inc) [k 0]))))
   (trie/make-trie)
   files))

(comment
  (def trie
    (let [database (atom {:next-id 0})
          files (->> (file-seq (io/file "dark-corpus"))
                     (remove #(.isDirectory %))
                     (drop 501)
                     (take 2))
          trie (file-seq->trie database files 1 3)]
      trie))

  (take 20 trie)

  )

(defn trie->tightly-packed-trie
  [trie encode-fn decode-fn]
  (tpt/tightly-packed-trie trie encode-fn decode-fn))

(def texts (eduction
            (comp (xf-file-seq 0 250000)
                  (map slurp))
            (file-seq (io/file "dark-corpus"))))

(defn split-text-into-sentences
  [text]
  (->> text
       (#(string/replace % #"([\.\?\!\n]+)" "$1\n"))
       (string/split-lines)))

(defn mapmap
  [fn & body]
  (apply map (partial map fn) body))

(defn normalize-text
  [[k v]]
  (if (string? (first v))
    [k (string/lower-case (first v))]
    [k v]))

(defn flatten-trie-entry-to-all-subkeys
  [[k v]]
  (loop [result []
         k k]
    (if (empty? k)
      result
      (recur (conj result [k v])
             (rest k)))))

(comment
  ;; TODO: Move to nlp.core
  (defn process-text
   "Processes text into key value pairs where
  the keys are parts-of-speech paths and the values
  are the children at that path.

  Ready to be inserted into a trie."
   [text]
   (->> text
        (split-text-into-sentences)
        (map string/trim)
        (remove empty?)
        (mapv nlp/treebank-zipper)
        (remove nil?)
        (map nlp/parts-of-speech-trie-entries)
        (reduce into [])
        (map flatten-trie-entry-to-all-subkeys)
        (reduce into [])
        (mapv normalize-text)
        (mapv (fn [[k v]]
                (clojure.lang.MapEntry. (into (vec k) [v]) v))))))

(comment
  (process-text (first texts))
  (flatten-trie-entry-to-all-subkeys
   '[(TOP S NP) (NP PP)])
  ;; => [[(TOP S NP) (NP PP)] [(S NP) (NP PP)] [(NP) (NP PP)]]
  )



(comment
  (->> (first texts)
       (split-text-into-sentences)
       (map string/trim)
       (remove empty?)
       (mapv nlp/treebank-zipper)
       (remove nil?)
       (map nlp/parts-of-speech-trie-entries)
       (reduce into [])
       (mapv normalize-text)
       )

  ;; TODO: MOST-RECENT-STOPPING-POINT
  ;; TODO: Pick BACK UP HERE and clean up the code in the future
  ;; so you know where you're working.
  (take 10 (map process-text texts))

  (def test-database (atom {:next-id 1}))

  (transduce
   (comp
    (map process-text))
   conj
   []
   texts)

  (take 20 @test-database)
  ;; => ([[DT JJR] 394]
  ;;     [558 "progress"]
  ;;     [453 "peace"]
  ;;     [584 "rather"]
  ;;     [487 "avoid"]
  ;;     ["teaches" 315]
  ;;     [519 [NP NP]]
  ;;     [[VB ADJP] 482]
  ;;     [357 INTJ]
  ;;     [275 [VBP NP S]]
  ;;     [NP 10]
  ;;     [[NN .] 358]
  ;;     ["skin" 384]
  ;;     [530 "yourself"]
  ;;     [[VBD NP] 173]
  ;;     ["strikes" 101]
  ;;     [389 "his"]
  ;;     ["look" 259]
  ;;     [[RB JJ] 196]
  ;;     ["products" 179])
  (time
   (def test-trie
     (transduce
      (comp
       (map
        (fn [text]
          (try
            (process-text text)
            (catch Exception e
              (println text)
              (throw e)))))
       (map (partial map (make-database-processor test-database))))
      (completing
       (fn [trie entries]
         (reduce
          (fn [trie [k v]]
            (update trie k (fnil inc 0)))
          trie
          entries)))
      (trie/make-trie)
      (take 3000 texts))))

  (nippy/freeze-to-file "/tmp/test-trie.bin" (seq test-trie))
  (time
   (def test-load-trie
     (into (trie/make-trie) (nippy/thaw-from-file "/tmp/test-trie.bin"))))

  (take 20 test-load-trie)
  (time (do
          (d/sum [test-trie])
          nil))
  (float (/ 17511624 (Math/pow 2 20)))
  (->> test-trie
       (take 20)
       (map (fn [[k v]]
              [k
               (map @test-database k)
               (last v)])))

  (->> (trie/lookup test-trie [22])
       (trie/children)
       (map #(get % []))
       (map (comp (partial map @test-database) first)))

  (->> (take 100 test-trie))

  (let [start 'TOP
        start-id (@test-database start)]
    (->> (map
          #(get % [])
          (trie/children (trie/lookup test-trie [start-id])))
         (remove nil?)
         (map (fn [[k v]]
                [k (map @test-database k) v])))
    #_(trie/children (trie/lookup test-trie [start-id])))

  (defn lookup [syms]
    (->> (map @test-database syms)
         (trie/lookup test-trie)
         ((fn [node]
            (if node (trie/children node) '())))
         (map
          #(get % []))
         (remove nil?)
         (sort-by (comp - second))
         (map
          (fn [[k v]]
            [k (map @test-database k) v]))))

  (lookup [(symbol ":")])

  (->> (map #(get % [])
            (trie/children (trie/lookup test-trie [7 8 10 22])))
       (remove nil?)
       (sort-by (comp - second))
       (map
        (fn [[k v]]
          [k (map @test-database k) v])))

  (@test-database (symbol "NN"))
  (@test-database (symbol ":"))

  (trie/lookup test-trie [7 8 3163])
  (let [start '[TOP [S]]
        start-id (map @test-database start)]
    (->> (trie/children (trie/lookup test-trie start-id))
         #_(remove nil?)
         #_(map (fn [[k v]]
                  [(map @test-database k) v]))))

  )

(defn key? [x] (symbol? x) #_(and (vector? x) (integer? (nth x 0))))

(defn pick-grammar
  [trie database]
  (loop [zipper (zip/vector-zip '[[TOP]])]
    (Thread/sleep 50)
    (println (zip/node zipper))
    (cond
      (zip/end? zipper)
      (zip/root zipper)
      (key? (zip/node zipper))
      (let [path (->> (zip/path zipper)
                      (map first)
                      (filter key?))
            key (map database path)
            _ (println "key" key "path" path)
            _ (println (zip/node (zip/root zipper)))
            children (->> (trie/lookup trie key)
                          (trie/children)
                          (map #(get % []))
                          (remove nil?))
            _ (println (first children))
            [path _] (first children)
            child (database (last path))]
        (println path child)
        (recur (zip/next (zip/next (zip/append-child (zip/up zipper) [child])))))
      :else
      (recur (zip/next zipper)))))

(defn grammar-children
  [database trie k]
  (sort-by
   (comp - last)
   (map #(vector (.key %) (database (.key %)) (get % []))
        (remove (comp nil? #(get % [])) (trie/children (trie/lookup trie k))))))

(defn grammar-branch?
  [trie database k]
  (vector (database (last k))))

(defn children
  [trie database k]
  (->> (trie/lookup trie k)
       (trie/children)
       (map #(vector (.key %) (get % [])))
       (remove (comp nil? second))
       (sort-by (comp - second))))

(defn choose
  [trie database k]
  (math/weighted-selection
   second
   (children trie database k)))

(comment
  (choose test-trie @test-database [1])
  (let [z1 (zip/vector-zip [1 [2]])
       z2 (zip/vector-zip [3 [4]])]
   (->> z1
        zip/down
        zip/right
        zip/down
        (#(zip/insert-right % (zip/node z2)))
        (zip/root))))

(defn generate
  [trie database zipper]
  (cond
    (zip/end? zipper)
    (zip/root zipper)

    (seqable? (zip/node zipper))
    (recur trie database (zip/next zipper))

    (symbol? (zip/node zipper))
    (recur trie database (zip/next zipper))

    (symbol? (database (zip/node zipper)))
    (let [sym (database (zip/node zipper))
          sym-path  (->> (map first (zip/path zipper))
                         butlast
                         (filter symbol?)
                         (#(concat % (list sym))))
          path (map database sym-path)
          choice (first (choose trie database path))]
      (recur
       trie
       database
       (-> zipper
           (zip/replace
            [sym choice])
           (zip/root)
           (zip/vector-zip))))

    (string? (database (zip/node zipper)))
    (let [terminal (database (zip/node zipper))
          path (->> (map first (zip/path zipper))
                    butlast
                    (filter symbol?))]
      (recur
       trie
       database
       (-> zipper
           (zip/replace
            terminal)
           (zip/next)
           (zip/root)
           (zip/vector-zip))))

    :else
    (recur
     trie
     database
     (-> zipper
         (zip/replace
          (mapv
           database
           (database (zip/node zipper))))
         (zip/next)
         (zip/root)
         (zip/vector-zip)))))

(comment
  (trie/lookup test-trie [1])
  (repeatedly
   20
   #(->> (generate test-trie @test-database (zip/vector-zip [1]))
        (zip/vector-zip)
        (iterate zip/next)
        (take-while (complement zip/end?))
        (map zip/node)
        (filter string?)))

  (-> [:a [:b] [:b]]
      zip/vector-zip
      zip/down
      zip/right
      zip/right
      zip/down
      zip/path)
  ;; => [[:a [:b] [:b]] [:b]]
  (-> [:a [:b] [:b]]
      zip/vector-zip
      zip/down
      zip/right
      zip/down
      zip/path)
  ;; => [[:a [:b] [:b]] [:b]]

  (@test-database 2)

  (->> [1 [2]]
       zip/vector-zip
       zip/down
       zip/right
       zip/path
       #_(map first))

  (pick-grammar test-trie @test-database)
  (get test-trie [1 3 62])

  (map @test-database ['NN])
  (@test-database "time")

  (take 5 test-trie)
  (->> (trie/lookup test-trie [1])
       (trie/children)
       (map #(vector (.key %) (get % [])))
       (remove nil?))

  (->> (trie/lookup test-trie [7])
       (trie/children)
       (map #(get % []))
       (remove nil?)) ;; => ([[7 2] 3484]
  ;;     [[7 4] 2027]
  ;;     [[7 6] 91]
  ;;     [[7 16] 25]
  ;;     [[7 21] 9]

  (@test-database 2)
  (->> [[1 [[2 [[3]]]]]]
       (zip/vector-zip)
       zip/down
       zip/down
       zip/right
       zip/down
       zip/down
       zip/right
       zip/down
       zip/down
       zip/path
       (map first)
       (filter integer?)
       )

  (let [queue (clojure.lang.PersistentQueue/EMPTY)]
    (peek (pop (into queue [1 2 3]))))


  (->> (map #(get % []) (trie/children (trie/lookup test-trie [7])))
       (remove nil?))

  (get @test-database 7)
  (take 10 (pick-grammar test-trie)))

(comment
  (let [database (atom {:next-id 1})
        trie (file-seq->trie
              database
              (transduce
               (xf-file-seq 0 2)
               conj
               (file-seq (io/file "dark-corpus")))
              1 4)]
    trie)

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

(comment
  ;; TODO: Move to nlp.core
  (defn xf-grammar-database
    [database]
    (fn [sentence]
      (let [leafs (->> sentence
                       nlp/treebank-zipper
                       nlp/leaf-pos-path-word-freqs)]
        (run!
         (fn [[k v]]
           (swap!
            database
            assoc
            k
            (merge-with + (@database k) v)))
         leafs)
        sentence))))

(comment
  ;; TODO: remove or move to nlp.core
  (defn file-seq->grammar-tree
    [files]
    (transduce
     (comp
      (xf-file-seq 0 1000)
      (map slurp)
      (map #(string/split % #"[\n+\?\.]"))
      (map (partial transduce xf-tokenize conj))
      (map (partial transduce xf-filter-english conj))
      (map (partial remove empty?))
      (remove empty?)
      (map (partial transduce xf-untokenize conj))
      (map nlp/grammar-tree-frequencies)
      (map (partial into {})))
     (fn
       ([acc]
        (sort-by (comp - second) acc))
       ([acc m]
        (merge-with + acc m)))
     {}
     files)))

(comment
  (time
   (->> (file-seq->grammar-tree
         (file-seq (io/file "dark-corpus")))
        (take 100)
        (nippy/freeze-to-file "/tmp/grammar-freqs-top-100.bin")))

  (def grammar-freqs (nippy/thaw-from-file "/tmp/grammar-freqs-top-100.bin"))
  (take 10 grammar-freqs)

  )

(comment
  ;; TODO: Remove or move to nlp.core
  (defn file-seq->part-of-speech-freqs
    [files]
    (transduce
     (comp
      (xf-file-seq 0 1000)
      (map slurp)
      (map #(string/split % #"[\n+\?\.]"))
      (map (partial transduce xf-tokenize conj))
      (map (partial transduce xf-filter-english conj))
      (map (partial remove empty?))
      (remove empty?)
      (map (partial transduce xf-untokenize conj))
      (map (partial map nlp/treebank-zipper))
      (map (partial map nlp/leaf-pos-path-word-freqs))
      (map (partial reduce (fn [acc m]
                             (nlp/deep-merge-with + acc m)) {})))
     (completing
      (fn [result input]
        (nlp/deep-merge-with + result input)))
     {}
     files)))

(comment
  (time (->> (file-seq->part-of-speech-freqs
              (file-seq (io/file "dark-corpus")))
             (nippy/freeze-to-file "/tmp/part-of-speech-freqs.bin")))

  (def parts-of-speech-freqs
    (nippy/thaw-from-file "/tmp/part-of-speech-freqs.bin"))
  (take 20 parts-of-speech-freqs)
  )


(comment
  ;; TODO: Remove or move to nlp.core
  (defn file-seq->parts-of-speech-trie
    [files]
    (transduce
     (comp
      (xf-file-seq 0 1000)
      (map slurp)
      (map #(string/split % #"[\n+\?\.]"))
      (map (partial transduce xf-tokenize conj))
      (map (partial transduce xf-filter-english conj))
      (map (partial remove empty?))
      (remove empty?)
      (map (partial transduce xf-untokenize conj))
      (map nlp/grammar-tree-frequencies)
      (map (partial into {})))
     (fn
       ([acc]
        (sort-by (comp - second) acc))
       ([acc m]
        (merge-with + acc m)))
     {}
     files)))
