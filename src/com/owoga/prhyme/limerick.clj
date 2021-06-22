(ns com.owoga.prhyme.limerick
  (:require [com.owoga.prhyme.generation.weighted-selection :as weighted-selection]
            [com.owoga.prhyme.util.math :as math]
            [com.owoga.prhyme.nlp.core :as nlp]
            [clojure.string :as string]
            [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.syllabify :as syllabify]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.trie :as trie]
            [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.tightly-packed-trie.encoding :as encoding]
            [clojure.java.io :as io]))

(defn rhyme-from-scheme
  "scheme of format [[A 9] [A 9] [B 5] [B 5] [A 9]]"
  [words markov scheme]
  (loop [scheme scheme
         rhymes {}
         result []]
    (cond
      (empty? scheme) result
      :else
      (let [[pattern syllable-count] (first scheme)
            banned-words (into #{} (->> result
                                        (map #(string/split % #" "))
                                        (map #(last %))))
            adj (util/comp-rnil
                 (weighted-selection/adjust-for-markov
                  markov
                  0.99)
                 (when (rhymes pattern)
                   (weighted-selection/adjust-for-tail-rhyme 0.99)))
            rhyme (if (nil? (get rhymes pattern))
                    (gen/gen-sentence-with-syllable-count
                     adj
                     syllable-count
                     words)
                    (gen/gen-rhyme-with-syllable-count
                     adj
                     syllable-count
                     (remove #(banned-words (:normalized-word %)) words)
                     (prhyme/phrase->word words (get rhymes pattern))))]
        (recur (rest scheme)
               (assoc rhymes pattern rhyme)
               (conj result rhyme))))))

(comment
  (require '[com.owoga.prhyme.data.dictionary :as dict]
           '[com.owoga.prhyme.data.darklyrics :refer [darklyrics-markov-2]]
           '[clojure.java.io :as io])

  (rhyme-from-scheme dict/prhyme-dict darklyrics-markov-2 '((A 8) (A 8) (B 5) (B 5) (A 8)))

  )

(comment
  ["bishop larch smitten us dwell"
   "solely first week in hell"
   "and take that for three"
   "come wrapped in glory"
   "you ever leave it so well"]
  ["romancing realized too late"
   "my crown revive my withered state"
   "reign is obsolete"
   "i sit in the street"
   "but nobody cares of my fate"]
  ["flesh is hacked to get me sedate"
   "demonstration obsessed with hate"
   "justice will be written in stone"
   "and you will be shown"
   "bedrooms of icons suffocate"]
  ["you will bow to their hungry gods"
   "come will come whatever the odds"
   "now we see the light"
   "you can't put it right"
   "recklessly chopping firing squads"]
  ["untimely they fool their poor life"
   "it wither away with this knife"
   "hate is my virtue"
   "my feelings are well overdue"
   "war we await the afterlife"])


;;;; Generating limericks with a markov model

(defn phrase->flex-rhyme-phones
  "Takes a space-seperated string of words
  and returns the concatenation of the words
  vowel phones.

  Returns them in reversed order so they
  are ready to be used in a lookup of a rhyme trie.
  "
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map (comp syllabify/syllabify first phonetics/get-phones))
       (map (partial reduce into []))
       (map #(filter (partial re-find #"\d") %))
       (flatten)
       (map #(string/replace % #"\d" ""))
       (reverse)))

(defn word->phones [word]
  (or (dict/word->cmu-phones word)
      (util/get-phones-with-stress word)))

(defonce context (atom {}))

(defn decode-fn [db]
  (fn [byte-buffer]
    (let [value (encoding/decode byte-buffer)]
      (if (zero? value)
        nil
        [value (encoding/decode byte-buffer)]))))

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
   :rhyme-trie
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
   :flex-rhyme-trie
   (transduce
    (comp
     (map (fn [[k v]]
            [(string/join " " (map (@context :database) k))
             [k v]]))
     (map (fn [[phrase [k v]]]
            [(phrase->flex-rhyme-phones phrase)
             [k v]])))
    (completing
     (fn [trie [k v]]
       (update trie k (fnil conj [v]) v)))
    (trie/make-trie)
    (->> (trie/children-at-depth (@context :trie) 0 1))))
  nil)


(comment
  (time (initialize))

  (println 2)

  (take 5 (:flex-rhyme-trie @context))

  )

(defn choose-next-word
  "Given an n-gram of [[word1 freq1] [word2 freq2]] chooses
  the next word based on markov data in trie.

  Could be improved by taking into account grammar and/or bidirectional context.

  The n-gram parameter is a list of trie entries
  For trie entries that are word/frequency pairs, it might look something like this.
  `[[sunshine 38] [</s> 509]]`

  But note that nothing in this function uses the frequency count from the passed in n-gram.
  It's just easier for the calling functions to pass them in like that."
  [{:keys [database trie] :as context} n-gram]
  (let [n-gram-ids (->> n-gram (map first) (map database))
        node (trie/lookup trie n-gram-ids)]
    (cond
      (= 0 (count n-gram-ids))
      (let [children (->> (trie/children trie)
                          (map #(get % [])))
            choice (math/weighted-selection second children)]
        [(database (first choice)) (second choice)])
      node
      (let [children (->> (trie/children node)
                          (map #(get % []))
                          (remove (fn [[id f]] (= id (first n-gram-ids)))))]
        (if (seq children)
          (let [children-freqs (into (sorted-map) (frequencies (map second children)))
                n-minus-1-gram-odds (/ (second (first children-freqs))
                                       (+ (second (get node []))
                                          (second (first children-freqs))))
                ;; Good-turing smoothing, take unseen ngram?
                take-n-minus-1-gram? (and (< 1 (count n-gram-ids))
                                          (< (rand) n-minus-1-gram-odds))]
            (if take-n-minus-1-gram?
              (choose-next-word context (butlast n-gram))
              (let [choice (math/weighted-selection second children)]
                [(database (first choice)) (second choice)])))
          (choose-next-word context (butlast n-gram))))
      :else
      (choose-next-word context (butlast n-gram)))))

(defn valid-sentence? [phrase]
  (->> phrase
       (map first)
       (string/join " ")
       (#(string/replace % #"(<s>|</s>)" ""))
       (nlp/valid-sentence?)))

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

(comment
  (take 5 (:database @context))

  (map (:database @context) ["me" "bother"])
  (map (:database @context) ["bother me"])
  (first
   (filter
    valid-sentence?
    (repeatedly
     (fn []
       (generate-sentence-backwards
        @context
        ["bother" "me" "</s>"])))))

  (keys @context)
  (time (initialize))
  )

(defn rhyme-from-scheme-2
  "Generate rhyme without the use of `weighted-selection/adjust-for-markov`."
  [])
