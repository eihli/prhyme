(ns examples.core
  (:require [clojure.string :as string]
            [clojure.set]
            [clojure.java.io :as io]
            [taoensso.nippy :as nippy]
            [taoensso.timbre :as timbre]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.data.bigrams :as bigrams]
            [com.owoga.prhyme.gen :as gen]
            [com.owoga.prhyme.nlp.core :as nlp]
            [com.owoga.prhyme.nlg.core :as nlg]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.data.thesaurus :as thesaurus]
            [com.owoga.prhyme.data.darklyrics :as darklyrics]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [com.owoga.prhyme.generation.weighted-selection :as weighted]
            [clojure.set :as set]
            [clojure.zip :as zip]
            [clojure.walk :as walk]))

(defn weight-popular [word target result]
  (if (dict/popular (:normalized-word word))
    10
    1))

(defn pred-popular [word target result]
  (< 1 (weight-popular word target result)))

(def words-by-rime (prhyme/words-by-rime*
                      (filter
                       (fn [[word & _]]
                         (get
                          dict/popular
                          (string/lower-case word)))
                       dict/cmu-dict)))

(defn rime-1 [target]
  (let [rime (last (:rimes target))]
    (fn [x]
      (= rime (last (:rimes x))))))

(defn rime-2 [target]
  (let [rime (last (butlast (:rimes target)))]
    (fn [x]
      (= rime (last (butlast (:rimes x)))))))

(defn rime-member? [coll]
  (let [coll (into #{} coll)]
    (fn [x]
      (coll (:normalized-word x)))))

(defn rime-compare [& comparators]
  (let [juxtcomp (apply juxt comparators)]
    (fn [a b]
      (let [a (juxtcomp a)
            b (juxtcomp b)]
        (compare a b)))))
(def c
  (fn [a b]
    ((rime-compare
      (rime-1 {:rimes '(1 2)})
      (rime-2 {:rimes '(1 2)})
      (rime-member? ["foo" "bar"]))
     b a)))

(comment
  (let [coll [{:rimes '(3 2) :normalized-word "foo"}
              {:rimes '(1 2) :normalized-word "foo"}
              {:rimes '(4 5) :normalized-word "foo"}
              {:rimes '(1 2) :normalized-word "buzz"}]]
    (sort c coll))

  (let [coll '("woman"
               "union"
               "passion"
               "infatuation"
               "emotion"
               "disposition"
               "communion"
               "attraction"
               "affection"
               "adoration"
               "admiration")
        coll (map #(prhyme/phrase->word dict/popular %) coll)
        target (prhyme/phrase->word dict/popular "devotion")
        synonyms (thesaurus/synonyms "love" "heart")
        comparisons (fn [target]
                      (fn [a b]
                        ((rime-compare
                          (rime-1 target)
                          (rime-2 target)
                          (rime-member? synonyms))
                         b a)))]
    (sort (comparisons target) coll))

  )

(defn rhymestorm [& words]
  (let [synonyms (->> (apply thesaurus/synonyms words)
                      (filter #(get dict/popular %))
                      (into #{}))
        comparisons (fn [target]
                      (fn [a b]
                        ((rime-compare
                          (rime-1 target)
                          (rime-2 target)
                          (rime-member? synonyms))
                         b a)))]
    (->> synonyms
         (map
          (fn [synonym]
            (let [word (prhyme/phrase->word dict/prhyme-dict synonym)
                  rhymes (get words-by-rime (last (:rimes word)))]
              (when rhymes
                (let [rhyming-words (map string/lower-case (prhyme/flatten-node rhymes))
                      rhyming-synonyms (remove #{(:normalized-word word)} (filter synonyms rhyming-words))]
                  [(:normalized-word word) rhyming-synonyms])))))
         (remove (fn [[_ rhymes]]
                   (empty? rhymes)))
         (map (fn [[target rhymes]]
                [target (->> rhymes
                             (map prhyme/phrase->word dict/popular)
                             (sort (comparisons (prhyme/phrase->word dict/popular target)))
                             (map :normalized-word))]))
         (into {}))))

(comment
  (rhymestorm "love")
  (take 3 (drop 500 dict/prhyme-dict))
  (take 3 dict/cmu-dict)
  (take 3 dict/popular)

  (let [node (get-in words-by-rime ['("AH" "V")])]
    (->> (prhyme/flatten-node node)))

  (let  [love-synonyms (thesaurus/thesaurus "love")
         heart-synonyms (thesaurus/thesaurus "heart")]
    (->> (clojure.set/intersection
          (into #{} love-synonyms)
          (into #{} heart-synonyms))
         (map string/lower-case)
         (filter #(dict/popular %))))

  (let [synonyms (thesaurus/synonyms "love" "heart")]
    synonyms)

  (def love-rhymes
    (let [synonyms (->> (thesaurus/synonyms "love" "heart")
                        (filter #(get dict/popular %))
                        (into #{}))]
      (->>
       (map
        (fn [synonym]
          (let [word (prhyme/phrase->word dict/prhyme-dict synonym)
                rhymes (get words-by-rime (last (:rimes word)))]
            (when rhymes
              (let [rhyming-words (map string/lower-case (prhyme/flatten-node rhymes))
                    rhyming-synonyms (filter synonyms rhyming-words)]
                [(:normalized-word word) rhyming-synonyms]))))
        synonyms)
       (into {}))))

  (count love-rhymes)
  (get-in words-by-rime ['("AH" "V")])

  (weight-fn
   (first (filter #(= (:normalized-word %) "gotshal's") dict/prhyme-dict))
   (prhyme/phrase->word dict/prhyme-dict "bye bye")
   nil)


 

  )

(defn remove-sentences-with-words-not-in-dictionary [dictionary]
  (let [dictionary (into #{} dictionary)]
    (fn [sentences]
      (->> sentences
           (map #(string/split % #" "))
           (remove #(some (complement dictionary) %))
           (remove #(some string/blank? %))
           (map #(string/join " " %))))))

(defn pathed-part-of-speech-word-frequencies
  "Seq of pathed part-of-speech to word frequencies of each document.

  {(TOP NP NN) {'test' 2 'sample' 4 ,,,}
   (TOP VP VBZ) {'is' 5 'runs' 2 ,,,}
   ,,,}

  To reduce, deep merge with +."
  [documents]
  (->> documents
       (map slurp)
       (map util/clean-text)
       (filter dict/english?)
       (map #(string/split % #"\n+"))
       (map (remove-sentences-with-words-not-in-dictionary dict/popular))
       (remove empty?)
       (remove #(some empty? %))
       (map nlp/treebank-zipper)
       (map nlp/leaf-pos-path-word-freqs)))

(defn grammar-tree-frequencies
  "Seq of grammar tree frequencies of each document.

  {(TOP (NP (NN)) (VP (VBZ))) 23
   (TOP (NP (DT) (NN)) (VP (VBZ))) 18
   ,,,}

  To reduce, merge with +."
  [documents]
  (->> documents
       (map slurp)
       (map util/clean-text)
       (filter dict/english?)
       (map #(string/split % #"\n+"))
       (map (remove-sentences-with-words-not-in-dictionary dict/popular))
       (remove empty?)
       (remove #(some empty? %))
       (map nlp/parse-to-simple-tree)
       (map nlp/parse-tree-sans-leaf-words)
       (map
        (fn [lines]
          (map #(hash-map % 1) lines)))
       (map (partial apply merge-with +))))

(defn weighted-selection-from-map [m]
  (first (weighted-rand/weighted-selection second (seq m))))

(defn chunked-writing-pos-path-freqs
  [documents chunk-size]
  (let [chunks (range 0 (count documents) chunk-size)]
    (run!
     (fn [chunk]
       (let [structure (->> documents
                            (drop chunk)
                            (take chunk-size)
                            pathed-part-of-speech-word-frequencies
                            (reduce
                             (fn [a v]
                               (nlp/deep-merge-with + a v))
                             {}))
             filepath (format "resources/pos-freqs/%s.nip" chunk)]
         (timbre/info (format "Writing to %s." filepath))
         (nippy/freeze-to-file filepath structure)))
     chunks)))

(defn chunked-writing-structure-freqs
  [documents chunk-size]
  (let [chunks (range 0 (count documents) chunk-size)]
    (run!
     (fn [chunk]
       (let [structure (->> documents
                            (drop chunk)
                            (take chunk-size)
                            grammar-tree-frequencies
                            (reduce
                             (fn [a v]
                               (nlp/deep-merge-with + a v))
                             {}))
             filepath (format "resources/structure-freqs/%s.nip" chunk)]
         (timbre/info (format "Writing to %s." filepath))
         (nippy/freeze-to-file filepath structure)))
     chunks)))

(defn pos-paths->pos-freqs
  "Convert pos paths, like {(TOP S NP NN) {'test' 5 'car' 3 ,,,}}
  into a top-level pos freq map like {NN {'test' 25 'car' 8 ,,,}}.is"
  [pos-paths]
  (->> pos-paths
       (map
        (fn [[k v]]
          (hash-map (last k) v)))
       (reduce
        (fn [a v]
          (nlp/deep-merge-with + a v))
        {})))

(comment

 

  ;; => ([("profanity" "unholy") {"its" 2}]
  ;;     [("ants" "triumph") {nil 1}]
  ;;     [("hiding" "our") {"of" 1, "expose" 3, "above" 1}]
  ;;     [("won't" "intervention") {"divine" 1, "an" 1}]
  ;;     [("pines" "weeping") {"the" 1}])


  ;; Merge pos paths
  (def pos-freqs-data
    (let [documents (->> "resources/pos-freqs"
                         io/file
                         file-seq
                         (remove #(.isDirectory %)))]
      (reduce
       (fn [accum document]
         (let [data (nippy/thaw-from-file document)]
           (nlp/deep-merge-with + accum data)))
       {}
       documents)))

  (nippy/freeze-to-file "resources/corpus/darklyrics/pos-word-freqs.nippy" pos-freqs-data)
  (count pos-freqs-data)
  (take 20 pos-freqs-data)

  (time
   (def pos-freqs-data-3
     (reduce
      (fn [acc [k v]]
        (let [new-map (hash-map (take-last 3 k) v)]
          (nlp/deep-merge-with + acc new-map)))
      {}
      pos-freqs-data)))
  (count pos-freqs-data-3)
  (take 2 (reverse (sort-by #(count (second %)) pos-freqs-data-3)))

  (time
   (def
     pos-freqs-data-2
     (reduce
      (fn [acc [k v]]
        (let [new-map (hash-map (take-last 2 k) v)]
          (nlp/deep-merge-with + acc new-map)))
      {}
      pos-freqs-data-3)))

  (def structure-freq-data
    (let [documents (->> "resources/structure-freqs"
                         io/file
                         file-seq
                         (remove #(.isDirectory %)))]
      (reduce
       (fn [accum document]
         (let [data (nippy/thaw-from-file document)]
           (nlp/deep-merge-with + accum data)))
       {}
       documents)))

  (def dark-lyrics-structure-frequencies
    (nippy/thaw-from-file "resources/corpus/darklyrics/grammar-tree-freqs.nippy"))

  (def popular-structure-freq-data (into {} (take 500 (reverse (sort-by #(second %) structure-freq-data)))))

  (take 100 popular-structure-freq-data)
  (nippy/freeze-to-file "resources/corpus/darklyrics/grammar-tree-freqs.nippy" structure-freq-data)

  (def t1 (nippy/thaw-from-file "resources/structure-freqs/0.nip"))

  (do
    (let [documents (->> "dark-corpus"
                         io/file
                         file-seq
                         (remove #(.isDirectory %))
                         (drop 5000))
          chunk-size 5000]
      (chunked-writing-pos-path-freqs
       documents
       chunk-size))
    (let [documents (->> "dark-corpus"
                         io/file
                         file-seq
                         (remove #(.isDirectory %))
                         (drop 50000))
          chunk-size 5000]
      (chunked-writing-structure-freqs
       documents
       chunk-size)))

  (def t1 (nippy/thaw-from-file "resources/pos-freqs/0.nip"))
  (take 10 t1)
  (let [path-freqs (pos-paths->pos-freqs t1)]
    (take 10 path-freqs))

  (take 5 t1)
  (take 10 (reverse (sort-by #(count (second %)) t1)))
  (def t3 (nippy/thaw-from-file "resources/pos-freqs/400.nip"))
  (def t2 (nippy/thaw-from-file "resources/pos-freqs/800.nip"))
  (count (merge-with + t1 t2 t3))
  ;; => 2353
  (count t3)
  ;; => 1013
  (count t1)
  ;; => 871
  (count t2)
  ;; => 676  (def corpus
  (->> "dark-corpus"
       io/file
       file-seq
       (remove #(.isDirectory %)))

  (time
   (def example-pos-freqs
     (->> corpus
          (take 100)
          pos-path-freqs
          (reduce
           (fn [a v]
             (nlp/deep-merge-with + a v))
           {}))))

  (time
   (def example-structures
     (->> corpus
          (take 100)
          grammar-tree-frequencies
          (reduce
           (fn [a v]
             (merge-with + a v))
           {}))))

  (let [structure (weighted-selection-from-map example-structures)]
    (repeatedly
     10
     (fn []
       (->> (nlp/generate-from-structure-and-pos-freqs
             structure
             example-pos-freqs)
            nlp/leaf-nodes
            (string/join " ")))))

  ;; => ("then get your life"
  ;;     "sometimes lie my hand"
  ;;     "still become your chapter"
  ;;     "alright fade our surfing"
  ;;     "far care my band"
  ;;     "all fake my fallow"
  ;;     "here gimme our head"
  ;;     "long back my guide"
  ;;     "never stop their seed"
  ;;     "never consume our tomorrow")

  ;; => ("now scarred towards the future"
  ;;     "never gone among the side"
  ;;     "ill removed with the end"
  ;;     "well filled in the life"
  ;;     "again torn towards the world"
  ;;     "desperately matched in the love"
  ;;     "nowadays matched in the ark"
  ;;     "awhile needed through all night"
  ;;     "so torn in the darkness"
  ;;     "first erased on the land")

  ;; => ("pictures of the destiny"
  ;;     "tears on the pain"
  ;;     "lights in the disaster"
  ;;     "corpses on the fire"
  ;;     "castles on the universe"
  ;;     "efforts for the king"
  ;;     "visions of the night"
  ;;     "retreats into the darker"
  ;;     "tales into the attack"
  ;;     "pictures into the play")

  (get-in {:a 1} '())
  (let [zipper (zip/seq-zip '(TOP (S (NP) (VB))))]
    (-> zipper
        zip/down
        zip/right
        zip/node)))
