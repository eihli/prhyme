(ns examples.core
  (:require [clojure.string :as string]
            [clojure.set]
            [clojure.java.io :as io]
            [com.owoga.prhyme.frp :as frp]
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

(defn weight-fn [word target result]
  (let [rimes (frp/consecutive-matching word target :rimes)
        nuclei (frp/consecutive-matching word target :nuclei)
        onsets (frp/consecutive-matching word target :onsets)
        total (apply + (map count [rimes nuclei onsets]))]
    total))

(defn pred-fn [word target result]
  (< 0 (weight-fn word target result)))

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

  (take 10 darklyrics/darklyrics-markov-2)
  (get darklyrics/darklyrics-markov-2 '("memory" "my"))
  (repeatedly
   5
   (fn []
     (let [rhymes (gen/selection-seq
                   dict/prhyme-dict
                   (comp (weighted/adjust-for-tail-rhyme 0.90)
                         #_(weighted/adjust-for-rhymes 0.50)
                         #_(weighted/adjust-for-fn :adj-rimes 0.80 pred-fn weight-fn)
                         (weighted/adjust-for-fn :adj-popular 0.95 pred-popular weight-popular)
                         (weighted/adjust-for-markov darklyrics/darklyrics-markov-2 0.99))
                   (prhyme/phrase->word dict/prhyme-dict "happy birthday taylor my love"))]
       (->> rhymes
            (take 5)
            (map :normalized-word)))))
  )

(defn remove-sentences-with-words-not-in-dictionary [dictionary]
  (let [dictionary (into #{} dictionary)]
    (fn [sentences]
      (->> sentences
           (map #(string/split % #" "))
           (remove #(some (complement dictionary) %))
           (remove #(some string/blank? %))
           (map #(string/join " " %))))))

(defn dark-pos-freqs []
  (let [directory "dark-corpus"]
    (->> (file-seq (io/file directory))
         (remove #(.isDirectory %))
         (take 1000)
         (map slurp)
         (map util/clean-text)
         (filter dict/english?)
         (map #(string/split % #"\n+"))
         (map (remove-sentences-with-words-not-in-dictionary dict/popular))
         (remove empty?)
         (remove #(some empty? %))
         (map nlp/treebank-zipper)
         (map nlp/leaf-pos-path-word-freqs)
         (apply nlp/deep-merge-with +))))

(defn dark-structures []
  (let [directory "dark-corpus"]
    (->> (file-seq (io/file directory))
         (remove #(.isDirectory %))
         (take 500)
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
            (map
             (fn [line]
               (hash-map line 1))
             lines)))
         (map (partial merge-with +))
         flatten
         (apply merge-with +))))

(defn weighted-selection-from-map [m]
  (first (weighted-rand/weighted-selection second (seq m))))

(comment
  (time (def example-pos-freqs (dark-pos-freqs)))

  (time (def example-structures (dark-structures)))

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
        zip/node))
  )
