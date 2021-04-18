(ns examples.tpt
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.prhyme.nlp.core :as nlp]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.trie.math :as math]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [clojure.zip :as zip]
            [cljol.dig9 :as d]
            [com.owoga.prhyme.data.phonetics :as phonetics]))

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


(defn clone-consonants [phones]
  (map
   #(if (phonetics/vowel (string/replace % #"\d" ""))
      %
      "?")
   phones))

(defn word->phones [word]
  (or (dict/word->cmu-phones word)
      (util/get-phones-with-stress word)))

(defn perfect-rhymes [rhyme-trie phones]
  (let [rhyme-suffix (first
                      (util/take-through
                       #(= (last %) \1)
                       (reverse phones)))]
    (trie/lookup rhyme-trie rhyme-suffix)))

(defn vowel-rhymes [rhyme-trie phones]
  (let [rhyme-suffix (->> (reverse phones)
                          (clone-consonants)
                          (util/take-through #(= (last %) \1))
                          (first))]
    (trie/lookup rhyme-trie rhyme-suffix)))

(defn n+1grams [trie k]
  (->> (trie/lookup trie k)
       (trie/children)
       (map #(get % []))))

(defn word->n+1grams [trie database word]
  (->> word
       database
       (#(trie/lookup trie [%]))
       trie/children
       (map #(get % []))
       (map (fn [[id fr]] [(database id) fr]))
       (sort-by (comp - #(nth % 1)))
       (remove #({"<s>" "</s>"} (nth % 0)))))

(comment
  (let [trie (@context :trie)
        db (@context :database)]
    (word->n+1grams trie db "technology"))

  )

(defonce context (atom {}))

(defn initialize []
  (swap!
   context
   assoc
   :database
   (with-open [rdr (clojure.java.io/reader "resources/backwards-database.bin")]
     (into {} (map read-string (line-seq rdr)))))
  (swap!
   context
   assoc
   :trie
   (tpt/load-tightly-packed-trie-from-file
    "resources/dark-corpus-backwards-tpt.bin"
    (decode-fn (@context :database))))
  (swap!
   context
   assoc
   :perfect-rhyme-trie
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
    (@context :database)))
  (swap!
   context
   assoc
   :vowel-rhyme-trie
   (transduce
    (comp
     (map first)
     (filter string?)
     (map #(vector % (reverse (word->phones %))))
     (map reverse)
     (map (fn [[phones v]]
            [(map #(if (phonetics/vowel
                        (string/replace % #"\d" ""))
                     %
                     "?")
                  phones)
             v])))
    (completing
     (fn [trie [k v]]
       (update trie k (fnil #(update % 1 inc) [v 0]))))
    (trie/make-trie)
    (@context :database)))
  nil)

(defn find-rhymes
  "Takes a rhyme-trie (perfect or vowel only, for example)
  and a word. Returns list of rhyming words."
  [trie word]
  (->> (perfect-rhymes trie (or (dict/cmu-with-stress-map word)
                                (util/get-phones-with-stress word)))
       (map (comp first second))
       (remove nil?)
       (map (@context :database))
       (map #(get (@context :trie) [%]))
       (sort-by #(nth % 1))
       (reverse)
       (map
        (fn [[word-id freq]]
          [((@context :database) word-id)
           freq]))
       (remove #(= word (first %)))))

(defn choose-next-word
  "Given an n-gram of [[word1 freq1] [word2 freq2]] chooses
  the next word based on markove data in trie."
  [{:keys [database trie] :as context} n-gram]
  (let [n-gram-ids (->> n-gram (map first) (map database))
        node (trie/lookup trie n-gram-ids)]
    (cond
      (= 0 (count n-gram-ids))
      (let [children (map #(get % []) (trie/children trie))
            choice (math/weighted-selection second children)]
        [(database (first choice)) (second choice)])
      node
      (let [children (->> node (trie/children) (map #(get % [])))]
        (if (seq children)
          (let [children-freqs (into (sorted-map) (frequencies (map second children)))
                n-minus-1-gram-odds (/ (second (first children-freqs))
                                       (+ (second (get node []))
                                          (second (first children-freqs))))
                take-n-minus-1-gram? (and (< 1 (count n-gram-ids))
                                          (< (rand) n-minus-1-gram-odds))]
            (if take-n-minus-1-gram?
              (choose-next-word context (butlast n-gram))
              (let [choice (math/weighted-selection second children)]
                [(database (first choice)) (second choice)])))
          (choose-next-word context (butlast n-gram))))
      :else
      (choose-next-word context (butlast n-gram)))))

(defn remove-sentence-markers [phrase]
  (remove (fn [[word _]] (#{"<s>" "</s>"} word)) phrase))

(defn valid-sentence? [phrase]
  (->> phrase
       (map first)
       (string/join " ")
       (#(string/replace % #"(<s>|</s>)" ""))
       (nlp/valid-sentence?)))

(defn valid-sentences? [phrase]
  (let [sentences (->> (util/take-through
                        #(= (first %) "</s>")
                        phrase)
                       (map remove-sentence-markers))]
    sentences))

(defn generate-phrase [{:keys [database trie] :as context} phrase]
  (loop [phrase' (loop [phrase phrase]
                   (if (< 5 (count phrase))
                     phrase
                     (recur (cons (choose-next-word context (take 3 phrase))
                                  phrase))))]
    (if (valid-sentence? phrase')
      phrase'
      (recur (loop [phrase phrase]
               (if (< 5 (count phrase))
                 phrase
                 (recur (cons (choose-next-word context (take 3 phrase))
                              phrase))))))))

(defn generate-sentence-backwards
  "Given a phrase of [w1 w2 w3] generates a sentence
  using a backwards markov."
  ([{:keys [database trie] :as context} phrase]
   (let [phrase (map (fn [w]
                       (let [id (database w)]
                         [w (second (get trie [id]))]))
                     phrase)]
     (loop [phrase' (loop [phrase phrase]
                      (if (= "<s>" (first (first phrase)))
                        phrase
                        (recur (cons (choose-next-word context (take 3 phrase))
                                     phrase))))]
       (if (valid-sentence? phrase')
         phrase'
         (recur (loop [phrase phrase]
                  (if (= "<s>" (first (first phrase)))
                    phrase
                    (recur (cons (choose-next-word context (take 3 phrase))
                                 phrase)))))))))
  )

(defn generate-rhyme
  ([context]
   (generate-rhyme context ["</s>"]))
  ([{:keys [perfect-rhyme-trie] :as context} phrase]
   (let [phrase1 (generate-sentence-backwards context phrase)
         rhyme (second (find-rhymes perfect-rhyme-trie (first (first (take-last 2 phrase1)))))
         phrase2 (generate-sentence-backwards context [(first rhyme) "</s>"])]
     [phrase1 phrase2])))

(comment
  (initialize)
  (generate-rhyme @context)

  (let [{:keys [database trie rhyme-trie]} @context
        phrase ["</s>"]
        ids (map database phrase)]
    (get trie ids))
  (choose-next-word @context (take 3 [["</s>" 509]]))

  (generate-sentence-backwards @context ["</s>"])

  (valid-sentences? (generate-phrase @context '(["bitter" 41])))



  (choose-next-word @context (take 3 [["theology" 41]]))

  (choose-next-word @context [["and" 5] ["theology" 41]])

  (find-rhymes (@context :perfect-rhyme-trie) "theology")

  (trie/chil(trie/lookup (@context :trie) '(57 2477)))
  (take 5 (@context :trie))


  (->> (find-rhymes (@context :perfect-rhyme-trie) "technology")
       (map (fn [[word frq]]
              (let [n+1grams (word->n+1grams
                              (@context :trie)
                              (@context :database)
                              word)]
                (map vector n+1grams (repeat [word frq])))))
       (reduce into []))
























  (do
    #_(time
       (def backwards-trie
         (transduce (comp (xf-file-seq 0 250000)
                          (map slurp)
                          (map (partial n-to-m-backwards-grams 1 4))
                          (map (fn [ngrams] (map #(prep-ngram-for-trie %) ngrams)))
                          stateful-transducer)
                    conj
                    (file-seq (io/file "dark-corpus")))))

    #_(time
       (def tightly-packed-backwards-trie
         (tpt/tightly-packed-trie
          backwards-trie
          encode-fn
          (decode-fn @trie-database))))

    #_(tpt/save-tightly-packed-trie-to-file
       "resources/dark-corpus-backwards-tpt.bin"
       tightly-packed-backwards-trie)
    #_(with-open [wtr (clojure.java.io/writer "resources/backwards-database.bin")]
        (let [lines (->> (seq @trie-database)
                         (map pr-str)
                         (map #(str % "\n")))]
          (doseq [line lines]
            (.write wtr line))))

    (def loaded-backwards-trie
      (tpt/load-tightly-packed-trie-from-file
       "resources/dark-corpus-backwards-tpt.bin"
       (decode-fn @trie-database)))

    (def loaded-backwards-database
      (with-open [rdr (clojure.java.io/reader "resources/backwards-database.bin")]
        (into {} (map read-string (line-seq rdr)))))

    (def rhyme-database (atom {}))

    (def perfect-rhyme-trie
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
       @loaded-backwards-database))

    (def vowel-rhyme-trie
      (transduce
       (comp
        (map first)
        (filter string?)
        (map #(vector % (reverse (word->phones %))))
        (map reverse)
        (map (fn [[phones v]]
               [(map #(if (phonetics/vowel
                           (string/replace % #"\d" ""))
                        %
                        "?")
                     phones)
                v])))
       (completing
        (fn [trie [k v]]
          (update trie k (fnil #(update % 1 inc) [v 0]))))
       (trie/make-trie)
       @loaded-backwards-database))
    )

  #_(with-open [wtr (clojure.java.io/writer "database.bin")]
      (let [lines (->> (seq @trie-database)
                       (map pr-str)
                       (map #(str % "\n")))]
        (doseq [line lines]
          (.write wtr line))))

  (profile
   {}
   (def example-story
     (loop [generated-text [(get @trie-database "<s>")]
            i              0]
       (if (> i 20)
         generated-text
         (let [children (loop [i 4]
                          (let [node     (p :lookup
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


(comment
  (->> (perfect-rhymes perfect-rhyme-trie
                       (or (dict/cmu-with-stress-map "technology")
                           (util/get-phones-with-stress "technology")))
       (map (comp first second))
       (remove nil?)
       #_#_#_#_(map @loaded-backwards-database)
       (map #(vector [%] (n+1grams
                          loaded-backwards-trie
                          [%])))
       (map (fn [[w1 w2s]]
              (mapv #(into w1 [(nth % 0)]) w2s)))
       (take 10))

  (->> (perfect-rhymes perfect-rhyme-trie
                       (or (dict/cmu-with-stress-map "technology")
                           (util/get-phones-with-stress "technology")))
       (map (comp first second))
       (remove nil?)
       (map @loaded-backwards-database)
       (map #(vector [%] (n+1grams
                          loaded-backwards-trie
                          [%])))
       (map (fn [[w1 w2s]]
              (mapv #(into w1 [(nth % 0)]) w2s)))
       (reduce into [])
       (map (fn [k]
              (let [children (->> (n+1grams loaded-backwards-trie k)
                                  (mapv first))]
                (mapv #(into k [%]) children))))
       (reduce into [])
       #_#_#_#_(map #(map @loaded-backwards-database %))
       (filter (partial every? dict/english?))
       (take 100)
       (map reverse))


  (util/get-phones-with-stress "you") ;; => ("B" "AA1" "DH" "ER" "M")
  (def phones (or (dict/cmu-with-stress-map "sandman")
                  (util/get-phones-with-stress "sandman")))

  (take 20 vowel-rhyme-trie)
  (->> (vowel-rhymes vowel-rhyme-trie phones)
       (map (comp first second))
       (remove nil?)
       (take 20))

  ;; Bigrams of rhyme
  (->> (perfect-rhymes perfect-rhyme-trie
                       (or (dict/cmu-with-stress-map "technology")
                           (util/get-phones-with-stress "technology")))
       (map (comp first second))
       (remove nil?)
       (map @loaded-backwards-database)
       (map #(vector [%] (n+1grams
                          loaded-backwards-trie
                          [%])))
       (map (fn [[w1 w2s]]
              (mapv #(into w1 [(nth % 0)]) w2s)))
       (reduce into [])
       (map (fn [k]
              (let [children (->> (n+1grams loaded-backwards-trie k)
                                  (mapv first))]
                (mapv #(into k [%]) children))))
       (reduce into [])
       (map #(map @loaded-backwards-database %))
       (filter (partial every? dict/english?))
       (take 100)
       (map reverse))

  )
