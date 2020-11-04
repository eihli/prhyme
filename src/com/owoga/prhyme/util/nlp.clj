(ns com.owoga.prhyme.util.nlp
  (:require [opennlp.nlp :as nlp]
            [opennlp.treebank :as tb]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [com.owoga.prhyme.nlp.tag-sets.treebank-ii :as tb2])
  (:import (opennlp.tools.postag POSModel POSTaggerME)))

(def tokenize (nlp/make-tokenizer (io/resource "models/en-token.bin")))
(def get-sentences (nlp/make-sentence-detector (io/resource "models/en-sent.bin")))
(def parse (tb/make-treebank-parser (io/resource "models/en-parser-chunking.bin")))
(def pos-tagger (nlp/make-pos-tagger (io/resource "models/en-pos-maxent.bin")))

;;;; The tagger that onennlp.nlp gives us doesn't provide access
;;;; to the probabilities of all tags. It gives us the probability of the
;;;; top tag through some metadata. But to get probs for all tags, we
;;;; need to implement our own tagger.
(defprotocol Tagger
  (tags [this sent])
  (probs [this])
  (top-k-sequences [this sent]))

(defn make-pos-tagger
  [modelfile]
  (let [model (with-open [model-stream (io/input-stream modelfile)]
                (POSModel. model-stream))
        tagger (POSTaggerME. model)]
    (reify Tagger
      (tags [_ tokens]
        (let [token-array (into-array String tokens)]
          (map vector tokens (.tag tagger #^"[Ljava.lang.String;" token-array))))
      (probs [_] (seq (.probs tagger)))
      (top-k-sequences [_ tokens]
        (let [token-array (into-array String tokens)]
          (.topKSequences tagger #^"[Ljava.lang.String;" token-array))))))

(def prhyme-pos-tagger (make-pos-tagger (io/resource "models/en-pos-maxent.bin")))

(comment
  (let [phrase "The feeling hurts."]
    (map (juxt #(.getOutcomes %)
               #(map float (.getProbs %)))
         (top-k-sequences prhyme-pos-tagger (tokenize phrase))))
  ;; => ([["DT" "NN" "VBZ" "."] (0.9758878 0.93964833 0.7375927 0.95285994)]
  ;;     [["DT" "VBG" "VBZ" "."] (0.9758878 0.03690145 0.27251 0.9286113)])
  )

(defn valid-sentence?
  "Tokenizes and parses the phrase using OpenNLP models from
  http://opennlp.sourceforge.net/models-1.5/

  If the parse tree has an clause as the top-level tag, then
  we consider it a valid English sentence."
  [phrase]
  (->> phrase
       tokenize
       (string/join " ")
       vector
       parse
       first
       tb/make-tree
       :chunk
       first
       :tag
       tb2/clauses
       boolean))

(defn unmake-tree
  "Tokenizing and then parsing a sentence returns a string
  representation of the parse tree. This is a helper function
  to make working with the parse tree more convenient. We
  can use `opennlp.treebank/make-tree` to make a clojure map
  representation of the tree, then we can `unmake` the tree
  to turn it into a list representation of the tree that
  we can easily use in a clojure zipper. (read-string almost works,
  but falls apart when reading things like commas)."
  [node]
  (cond
    (string? node) node
    (map? node) (list (:tag node) (unmake-tree (:chunk node)))
    :else (map unmake-tree node)))

(comment
  (let [phrase "Hello, Eric"]
    (->> phrase
         tokenize
         (string/join " ")
         vector
         parse
         (map tb/make-tree)
         unmake-tree))
  ;; => ((TOP ((S ((INTJ ((UH ("Hello")))) (, (",")) (. ("Eric")))))))
  )

(defn treebank-zipper
  "Turns a bit of text into a parse tree into a zipper."
  [text]
  (let [tree (->> text
                  tokenize
                  (string/join " ")
                  vector
                  parse
                  (map tb/make-tree)
                  unmake-tree)]
    (zip/zipper seq? seq (fn [_ c] c) tree)))

(defn node-constituents
  "Given a node of a parse tree, like ('NP (('PRP$ (\"my\" 'NN (\"name\"))))),
  returns a list of the top-level node tag and its first-level child tags.
  "
  [node]
  (list
   (first node)
   (if (every? string? (map first (rest node)))
     nil
     (map first (first (rest node))))))

(defn phrase-constituents
  "Given a bit of text that can be parsed into a treebank tree,
  Get a sequence of the tags and their chunks.
  For example:
    My name is Eric.
  Returns the sequence:
    At the TOP tag, we have a 'S part-of-speech (a clause).
    At the 'S tag, we have a 'NP, 'VP, '. (noun-phrase + verb-phrase + period)
    At the 'NP tag, we have a 'PRP$, 'NN (personal-pronoun + singular-noun)
    ...
  "
  [text]
  (->> (treebank-zipper text)
       (iterate zip/next)
       (take-while (complement zip/end?))
       (filter (complement zip/branch?))
       (map zip/path)
       (map last)
       (map node-constituents)
       (remove #(string? (first %)))))

(comment
  (phrase-constituents "My name is Eric.")
  ;; => ((TOP (S)) (S (NP VP .)) (NP (PRP$ NN)) (VP (VBZ NP)) (NP (NNP)))
  (phrase-constituents "How are you?")
  ;; => ((TOP (SBARQ)) (SBARQ (WHADVP SQ .)) (WHADVP (WRB)) (SQ (VBP NP)) (NP (PRP))) 
  )


(defn pos-constituent-frequencies
  "Frequencies of the parts of speech that make up phrases.
  Example:
    Clauses are made up of:
      NounPhrase + VerbPhrase 2 times
      Clause + CoordinatingConjuction + Clause 1 times
    NounPhrases are made up of:
      ProperNouns 2 times
      PersonalPronoun + SingularNoun 3 times

  Does not include frequencies for leaf words. By that I mean: A SingularNoun might
  appear 5 times all together, 3 times as part of a PersonalPronoun + SingularNoun pair
  and 2 times as part of an Adjective + SingularNoun pair, but the data structure returned
  by this function won't include that 5 anywhere. This is due to the (remove #(nil? (second %)))
  line. This data structure is used as a kind of markov selection process and we don't really
  care how often the leafs are used. We just care about the ratio at which we should pick each
  leaf from a given parent.
  "
  [texts]
  (reduce
   (fn [acc text]
     (let [constituents (->> text
                             phrase-constituents
                             (remove #(nil? (second %))))]
       (reduce
        (fn [acc constituent]
          (let [k1 (first constituent)
                k2 (second constituent)]
            (update-in acc [k1 k2] (fnil inc 0))))
        acc
        constituents)))
   {}
   texts))

(comment
  (pos-constituent-frequencies
   ["My name is Eric."
    "My hat is blue and I like cake."
    "Your name is Taylor."
    "How are you?"])
  ;; => {TOP {(S) 3, (SBARQ) 1},
  ;;     S {(NP VP .) 2, (S CC S .) 1, (NP VP) 2},
  ;;     NP {(PRP$ NN) 3, (NNP) 2, (PRP) 2, (NN) 1},
  ;;     VP {(VBZ NP) 2, (VBZ ADJP) 1, (VBP NP) 1},
  ;;     ADJP {(JJ) 1},
  ;;     SBARQ {(WHADVP SQ .) 1},
  ;;     WHADVP {(WRB) 1},
  ;;     SQ {(VBP NP) 1}}

  (let [phrase "How are you today?"]
    (->> phrase
         tokenize
         (string/join " ")
         vector
         parse
         (map tb/make-tree)))

  (let [phrase "I gave the cake to John at the store."]
    (parse (tokenize phrase)))

  (let [phrase "I've got a good feeling"]
    (pos-tagger (tokenize phrase)))
  )
