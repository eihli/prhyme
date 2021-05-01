(ns com.owoga.prhyme.nlp.core
  (:require [opennlp.nlp :as nlp]
            [opennlp.treebank :as tb]
            [clojure.string :as string]
            [com.owoga.prhyme.data-transform :as df]
            [com.owoga.trie :as trie]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [com.owoga.prhyme.nlp.tag-sets.treebank-ii :as tb2]
            [com.owoga.prhyme.util.weighted-rand :as weighted-rand]
            [clojure.walk :as walk]
            [com.owoga.prhyme.data.dictionary :as dict])
  (:import (opennlp.tools.postag POSModel POSTaggerME)
           (opennlp.tools.parser Parse ParserModel
                                 ParserFactory)
           (opennlp.tools.cmdline.parser ParserTool)))

(comment tb2/phrases)
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
  (let [phrase "I am feeling the heat."]
    (map (juxt #(.getOutcomes %)
               #(map float (.getProbs %)))
         (top-k-sequences prhyme-pos-tagger (tokenize phrase))))
;; => ([["PRP" "VBP" "VBG" "DT" "NN" "."]
;;      (0.9800125 0.9771906 0.9722519 0.9709216 0.9941198 0.98704773)]
;;     [["PRP" "VBP" "NN" "DT" "NN" "."]
;;      (0.9800125 0.9771906 0.01259052 0.76849043 0.99447477 0.98704773)])
  )

;;;; Custom parser to get access to top N parses
(def custom-parser
  (ParserFactory/create
   (ParserModel.
    (io/input-stream (io/resource "models/en-parser-chunking.bin")))
   3
   0.95))

(defn parse-probs [parses]
  (map #(.getProb %) parses))

(defn parse-strs [parses]
  (let [results (StringBuffer.)]
    (run!
     #(do (.show % results)
          (.append results "\n"))
     parses)
    (string/split results #"\n")))

(comment
  (tokenize "Eric's testing.")
  (let [results (StringBuffer.)
        parses (ParserTool/parseLine "Eric 's testing ." custom-parser 3)]
    ((juxt parse-probs parse-strs) parses))

  )

(defn parse-top-n [tokenized n]
  (let [results (StringBuffer.)
        parses (ParserTool/parseLine tokenized custom-parser n)]
    (apply map vector ((juxt parse-strs parse-probs) parses))))

(comment
  (let [phrase "The feeling hurts."]
    (->> phrase
        tokenize
        (string/join " ")
        (#(parse-top-n % 10))))
  (Math/pow Math/E -0.96)
  )

(defn deep-merge-with [f & maps]
  (letfn [(m [& xs]
            (if (some map? xs)
              (apply merge-with m xs)
              (apply f xs)))]
    (reduce m maps)))

(comment
  (let [m1 {:a 1 :b {:b 2}}
        m2 {:c 3 :b {:b 5}}]
    (deep-merge-with + m1 m2))
  ;; => {:a 1, :b {:b 7}, :c 3}
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
  but falls apart when reading things like commas).

  This doesn't map to the string returned by the parser.
  Children here are all nested inside a single list.
  For example: (NP ((DT 'a') (NN ('sample')) (NN 'test')))
  rather than: (NP (DT 'a') (NN 'sample') (NN 'test'))

  If you want a data structure that is in parity with the
  string returned by the parser, look at `parse-to-simple-tree`
  "
  [node]
  (cond
    (string? node) node
    (map? node) (list (:tag node) (unmake-tree (:chunk node)))
    ;; This can fail and throw. It's easier to debug
    ;; if this failure is caught and handled early.
    :else (doall (map unmake-tree node))))

(comment
  (let [text-lines ["This is a sample test."
                    "This is another line."]]
    (->> text-lines
         (map tokenize)
         (map (partial string/join " "))
         parse
         (map tb/make-tree)
         (map unmake-tree)))
  ;; => ((TOP
  ;;      ((S
  ;;        ((NP ((DT ("This"))))
  ;;         (VP ((VBZ ("is")) (NP ((DT ("a")) (NN ("sample")) (NN ("test"))))))
  ;;         (. ("."))))))
  ;;     (TOP
  ;;      ((S
  ;;        ((NP ((DT ("This"))))
  ;;         (VP ((VBZ ("is")) (NP ((DT ("another")) (NN ("line"))))))
  ;;         (. (".")))))))
  )


(defn parse-to-simple-tree
  "Returns a Clojure data structure, a list, in the shape of a tree, that
  matches the structure returned by the parser.

  The implementation takes a roundabout path. It uses `tb/make-tree` to
  get a map/list data structure, it then unmakes the tree info a data
  structure that can be `walk`ed, it then walks the tree making modifications
  to bring it in line with the parse string structure.
  "
  [text-lines]
  (->> text-lines
       (map tokenize)
       (map (partial string/join " "))
       parse
       (map tb/make-tree)
       (map unmake-tree)
       (map
        #(walk/postwalk
          (fn [node]
            (cond
              (and (seq? node)
                   (symbol? (first node))
                   (and (seq? (second node))
                        (seq? (first (second node)))))
              (cons (first node) (second node))

              (and (seq? node)
                   (string? (first node)))
              (first node)

              :else node))
          %))))

(comment
  ;; This is what the parsed string looks like.
  (let [text-lines ["Hello, world!"]]
    (->> text-lines (map tokenize) (map (partial string/join " ")) parse))
  ;; => ["(TOP (FRAG (INTJ (UH Hello)) (, ,) (NP (NN world)) (. !)))"]

  ;; And this is what the simple-tree Clojure data structure looks like.
  (let [text-lines ["Hello, world!"]]
    (parse-to-simple-tree text-lines))
  ;; => ((TOP (FRAG (INTJ (UH "Hello")) (, ",") (NP (NN "world")) (. "!")))) 
  )

(defn treebank-zipper
  "Turns a bit of text into a parse tree into a zipper.

  Porcelain. If you have the simple tree data structure
  returned by `parse-to-simple-tree`, then you can just
  pass that directly to `zip/seq-zip`.

  Returns nil if something can't be parsed. This will be
  the case for empty strings."
  [text]
  (try
    (let [tree (->> text
                    tokenize
                    (string/join " ")
                    vector
                    parse
                    first
                    tb/make-tree
                    unmake-tree)]
      (doall (zip/seq-zip tree)))
    (catch Exception e
      nil)))

(comment
  ;; Here is a demo of zipping through a parse tree and changing
  ;; all adjectives to "thorough".
  (let [text "Eric's test is difficult."]
    (loop [zipper (treebank-zipper text)]
      (cond
        (zip/end? zipper) (zip/root zipper)
        (= 'JJ (zip/node zipper)) (recur (-> zipper
                                             zip/next
                                             (zip/replace '("thorough"))))
        :else (recur (zip/next zipper)))))
  ;; => (TOP
  ;;     ((S
  ;;       ((NP ((NP ((NNP ("Eric")) (POS ("'s")))) (NN ("test"))))
  ;;        (VP ((VBZ ("is")) (ADJP ((JJ ("thorough"))))))
  ;;        (. ("."))))))

  )

(defn iter-zip
  ([zipper]
   (->> zipper
        (iterate zip/next)
        (take-while (complement zip/end?))))
  ([zipper n s]
   (->> zipper
        (iterate n)
        (take-while (complement s)))))

(defn iter-nodes [zipper]
  (->> zipper
       iter-zip
       (map zip/node)))

(defn phrase-level? [node]
  (and (seq? node)
       (symbol? (first node))))

(defn word-level? [node]
  (and (seq? node)
       (symbol? (first node))
       (= 1 (count (second node)))
       (string? (first (second node)))))

(defn leaf-pos-paths
  "Seq of the path down the parse tree to each leaf part-of-speech.

  Useful to aggregate over a corpus information like how often
  different words are used as different parts of speech.

  Benefit of having the entire path down the tree is that you
  can know things like 'A noun phrase as a child of a verb phrase
  never has a determiner.'
  "
  [zipper]
  (->> zipper
       iter-zip
       (filter (complement zip/branch?))
       (map zip/path)
       (map #(map first %))
       (map
        #(filter
          (fn [v] (or (string? v)
                      (symbol? v)))
          %))
       (filter #(string? (last %)))))

(comment
  (let [zipper (treebank-zipper ["Commas, they work."
                                 "Eric's test is difficult."
                                 "Eric's testing the code."])]
    (leaf-pos-paths zipper))
  ;; => ((TOP S NP NNP "Commas")
  ;;     (TOP S , ",")
  ;;     (TOP S NP PRP "they")
  ;;     (TOP S VP VBP "work")
  ;;     (TOP S . ".")
  ;;     (TOP S NP NP NNP "Eric")
  ;;     (TOP S NP NP POS "'s")
  ;;     (TOP S NP NN "test")
  ;;     (TOP S VP VBZ "is")
  ;;     (TOP S VP ADJP JJ "difficult")
  ;;     (TOP S . ".")
  ;;     (TOP S NP NNP "Eric")
  ;;     (TOP S VP VBZ "'s")
  ;;     (TOP S VP VP VBG "testing")
  ;;     (TOP S VP VP NP DT "the")
  ;;     (TOP S VP VP NP NN "code")
  ;;     (TOP S . "."))
  )

(defn leaf-pos-path-word-freqs [zipper]
  (->> zipper
      leaf-pos-paths
      (map #(hash-map (butlast %) {(last %) 1}))
      (apply deep-merge-with +)))

(defn pathed-part-of-speech-word-frequencies
  "I like this name better."
  [zipper]
  (leaf-pos-path-word-freqs zipper))

(comment
  (treebank-zipper ["Eric's test is difficult."
                    "Eric's test is thorough."
                    "Eric's testing."])
  (let [zipper (treebank-zipper ["Eric's test is difficult."
                                 "Eric's test is thorough."
                                 "Eric's testing."])]
    (leaf-pos-path-word-freqs zipper))
  ;; => {(TOP NP .) {"." 1},
  ;;     (TOP NP NP POS) {"'s" 1},
  ;;     (TOP S NP NP NNP) {"Eric" 2},
  ;;     (TOP NP NN) {"testing" 1},
  ;;     (TOP S VP VBZ) {"is" 2},
  ;;     (TOP S .) {"." 2},
  ;;     (TOP S NP NP POS) {"'s" 2},
  ;;     (TOP NP NP NNP) {"Eric" 1},
  ;;     (TOP S NP NN) {"test" 2},
  ;;     (TOP S VP ADJP JJ) {"difficult" 1, "thorough" 1}}
  )

(defn parse-tree-sans-leaf-words
  "Takes a 'simple' parse tree (`parse-to-simple-tree`)
  Removes the leaf words from the tree.

  Useful if you want to work with the structure of something
  without caring about the actual words."
  [tree]
  (walk/postwalk
   (fn [node]
     (if (and (seq? node)
              (string? (second node)))
       (take 1 node)
       node))
   tree))

(comment
  (let [tree (parse-to-simple-tree ["Eric's test is difficult."
                                    "Eric's test is thorough."
                                    "Eric's testing."
                                    "Eric is testing."])]
    (parse-tree-sans-leaf-words tree))
  ;; => ((TOP (S (NP (NP (NNP) (POS)) (NN)) (VP (VBZ) (ADJP (JJ))) (.)))
  ;;     (TOP (S (NP (NP (NNP) (POS)) (NN)) (VP (VBZ) (ADJP (JJ))) (.)))
  ;;     (TOP (NP (NP (NNP) (POS)) (NN) (.)))
  ;;     (TOP (S (NP (NNP)) (VP (VBZ) (VP (VBG))) (.))))
  )

(defn grammar-tree-frequencies
  "Seq of grammar tree frequencies of each document.

  {(TOP (NP (NN)) (VP (VBZ))) 23
   (TOP (NP (DT) (NN)) (VP (VBZ))) 18
   ,,,}

  To reduce, merge with +."
  [document]
  (->> document
       parse-to-simple-tree
       parse-tree-sans-leaf-words
       (map #(hash-map % 1))
       (apply merge-with +)))

(comment
  (let [document ["this is a test"
                  "this is a sample"
                  "that was a test"
                  "you are a test"]]
    (grammar-tree-frequencies
     document))

  (grammar-tree-frequencies ["this is a test."])
  (parse-to-simple-tree ["this is a test."])
  ;; => {(TOP (S (NP (WDT)) (VP (VBD) (NP (DT) (NN))))) 1,
  ;;     (TOP (S (NP (DT)) (VP (VBZ) (NP (DT) (NN))))) 2,
  ;;     (TOP (S (NP (PRP)) (VP (VBP) (NP (DT) (NN))))) 1}
  )

;;;; This is not sufficient
;; You'll end up with a mapping that says a verb phrase can be a
;; third-person-singular verb, like "is" followed by a
;; noun-phrase. But "is" can't be followed by just any noun-phrase.
;; It must be followed by a noun-phrase that starts with a determiner,
;; like "a" or "the".
(defn pos->children-freqs
  "Takes a zipper representation of a parsed text.

  Returns a map of parts-of-speech to a map of their constituent parts-of-speech
  and the number of times each constituent is seen in a corpus.

  For example, in the sentence 'This is a test and that is not a test.',
  there are 4 noun phrases; 'this', 'a test', 'that', 'a test'.

  Twice, the noun phrase is made up of a determiner (namely, 'this' and 'that').
  Twice, the noun phras is made up of a determiner and a noun (namely, 'a test' and 'a test').

  If the constituent part of speech is an actual word, then the key is the word.

  For the sentence 'This is a test and that is not a test.', this function would include a kv pairs of:
  {NP {(DT) 2 (DT NN) 2}
   CC {'and' 1}
   VBZ {'is' 2}
   ,,,}

  This data structure is useful for text generation. If you're generating a sentence and you
  know that you need a coordinating conjunction, you can choose one in from the list weighted
  by their frequencies.
  "
  [zipper]
  (->> zipper
       iter-nodes
       (filter phrase-level?)
       (map
        (fn [[pos xs]]
          (vector
           pos
           (if (string? (first xs))
             (first xs)
             (map first xs)))))
       (map (fn [[k v]]
              {k {v 1}}))
       (apply deep-merge-with +)))

(comment
  (let [zipper (treebank-zipper ["Eric's test is difficult."
                                 "Eric's test is thorough."
                                 "Eric's testing."])
        freqs (->> zipper
                   pos->children-freqs)]
    freqs)
  ;; => {NP {(NP NN) 2, (NNP POS) 3, (NP NN .) 1},
  ;;     ADJP {(JJ) 2},
  ;;     VBZ {"is" 2},
  ;;     S {(NP VP .) 2},
  ;;     NNP {"Eric" 3},
  ;;     JJ {"difficult" 1, "thorough" 1},
  ;;     TOP {(S) 2, (NP) 1},
  ;;     POS {"'s" 3},
  ;;     NN {"test" 2, "testing" 1},
  ;;     VP {(VBZ ADJP) 2},
  ;;     . {"." 3}}
  )

(defn remove-sentences-with-words-not-in-dictionary [dictionary]
  (let [dictionary (into #{} dictionary)]
    (fn [sentences]
      (println sentences)
      (println dictionary)
      (->> sentences
           (map #(string/split % #" "))
           (remove #(some (complement dictionary) %))
           (remove #(some string/blank? %))
           (map #(string/join " " %))))))

(comment
  (let [dictionary ["this" "is" "a" "test"]
        remove-fn (remove-sentences-with-words-not-in-dictionary
                   dictionary)
        sentences ["this is a foobar test"
                   "Eric is a test"
                   "this is a test"
                   "a test this is"]]
    (->> sentences
         remove-fn))
  ;; => ("this is a test" "a test this is")
  )

(defn generate-from-freqs
  "freqs is a map of parts-of-speech to a map of their constituent parts-of-speech
  and the number of times each constituent is seen in a corpus.

  start is the part-of-speech to start generating for.

  Creates a zipper that will initially have just the starting node.
  Iterates through the zipper with zip/next. When we encounter a node
  that exists in the frequencies map, then replace the node we are on
  with a value from the map.

  A problem with this is that we might not always get a valid structure.

  For example, a noun phrase might not start with a determiner 20% of the time,
  but 100% of the time when the noun phrase follows a verb phrase.

  So if we are just selecting based on frequency, we'll end up worse off than
  if we take into account the entire context we're in.
  "
  [freqs start]
  (let [zipper (zip/seq-zip (list start))]
    (loop [zipper zipper]
      (let [cur-freqs (freqs (zip/node zipper))]
        (cond
          (zip/end? zipper) (zip/root zipper)

          (zip/branch? zipper) (recur (zip/next zipper))

          cur-freqs
          (let [selection (first (weighted-rand/weighted-selection second cur-freqs))]
            (recur
             (zip/next
              (zip/next
               (zip/replace
                zipper
                (list (zip/node zipper) selection))))))

          :else (recur (zip/next zipper)))))))

(comment
  (letfn [;; This is just a helper function to get the string leafs of a tree and
          ;; join them into a single string.
          (string-leaf-nodes [tree]
            (->> tree
                 (zip/seq-zip)
                 (iterate zip/next)
                 (take-while (complement zip/end?))
                 (filter (complement zip/branch?))
                 (map zip/node)
                 (filter string?)
                 (string/join " ")))]
    (let [ ;; Create an initial corpus.
          zipper (treebank-zipper ["This is a zipper test in the car."
                                   "And this is another one."
                                   "Here are some sample sentences."
                                   "Let's see what we can generate."
                                   "This is a big adjective."
                                   "That's a short adjective."
                                   "The corpus will be a simple start."])
          ;; Extract parts-of-speech frequencies from the corpus.
          freqs (pos->children-freqs zipper)]
      ;; Let's generate a bunch of noun phrases.
      (repeatedly
       10
       (fn []
         (->> (generate-from-freqs freqs 'NP)
              string-leaf-nodes)))))
  ;; => ("another simple corpus"
  ;;     "we"
  ;;     "a big start"
  ;;     "This start"
  ;;     "'s"
  ;;     "a"
  ;;     "a test sentences"
  ;;     "a sample"
  ;;     "another car"
  ;;     "this car sentences")
  )


(defn pos-freq [pos-path]
  (loop [remaining-path (rest pos-path)
         current-path [(first pos-path)]
         result {}]
    (cond
      (string? (first remaining-path))
      (-> result
          (update-in
           (conj current-path :words)
           #(merge-with + % {(first remaining-path) 1}))
          (update-in
           current-path
           #(merge-with + % {:freq 1})))

      :else
      (recur (rest remaining-path)
             (conj current-path (first remaining-path))
             (update-in
              result
              current-path
              #(merge-with + % {:freq 1}))))))

(comment
  (let [pos-path '(TOP S NP DT "This")]
    (pos-freq pos-path))

  ;; => {TOP {:freq 1, S {:freq 1, NP {:freq 1, DT {:words {"This" 1}, :freq 1}}}}}
  )

(defn pos-freqs [pos-paths]
  (apply deep-merge-with + pos-paths))

(defn structure-freqs
  "Frequencies of entire parse trees."
  [text-lines]
  (let [parse-tree (-> text-lines
                       parse-to-simple-tree
                       parse-tree-sans-leaf-words)
        freqs (->> parse-tree
                   (map (fn [line] {line 1}))
                   (apply merge-with +))]
    freqs))

(comment
  (let [text-lines ["This is a test."
                    "This is a line."
                    "That is a test."
                    "You are a test."
                    "This is not a test."
                    "I am a test."
                    "This is a sample."
                    "Go to the store."
                    "Run the test."
                    "The test is running."]]
    (structure-freqs text-lines))
  ;; => {(TOP (S (NP (DT)) (VP (VBZ) (NP (DT) (NN))) (.))) 4,
  ;;     (TOP (S (NP (PRP)) (VP (VBP) (NP (DT) (NN))) (.))) 2,
  ;;     (TOP (S (NP (DT)) (VP (VBZ) (RB) (NP (DT) (NN))) (.))) 1,
  ;;     (TOP (VP (VB) (PP (TO) (NP (DT) (NN))) (.))) 1,
  ;;     (TOP (VP (VB) (NP (DT) (NN)) (.))) 1,
  ;;     (TOP (S (NP (DT) (NN)) (VP (VBZ) (VP (VBG))) (.))) 1}
  )

(defn generate-from-structure-and-pos-freqs
  "This is an attempt to improve on selecting words for a particular part-of-speech
  based solely on the part of speech.

  By passing a `structure-freqs` map, we can choose from a collection of known-valid
  structures.

  S -> NP | VP | NP VP
  NP -> NN | DT NN | DT
  VP -> VB | VB NN

  If we go just based on frequencies, we might have a grammar that allows something like the above.
  But it might be such that when S is NP VP, then NP is NEVER DT.

  Thak's what supplying the structure frequency map can improve.
  "
  [structure pos-freqs]
  (let [zipper (zip/seq-zip structure)]
    (loop [zipper zipper]
      (let [path (map first (zip/path zipper))
            cur-freqs (pos-freqs path)]
        (cond
          (zip/end? zipper) (zip/root zipper)

          (zip/branch? zipper) (recur (zip/next zipper))

          cur-freqs
          (let [selection (first (weighted-rand/weighted-selection second cur-freqs))]
            (recur
             (zip/next
              (zip/next
               (zip/replace
                zipper
                (list (zip/node zipper) selection))))))

          ;; ???
          :else (recur (zip/next zipper)))))))

(defn leaf-nodes [tree]
  (->> tree
       zip/seq-zip
       (iterate zip/next)
       (take-while (complement zip/end?))
       (map zip/node)
       (filter string?)))

(comment
  (let [corpus ["this is a test"
                "that is a test"
                "this is a sample"
                "that is some code"
                "there is a car"
                "those are some cars"
                "that is a dog"
                "it is a dog"]
        structure '(TOP (S (NP (DT)) (VP (VBZ) (NP (DT) (NN)))))
        pos-freqs (->> corpus
                       treebank-zipper
                       leaf-pos-path-word-freqs)]
    (repeatedly
     10
     (fn []
       (->> (generate-from-structure-and-pos-freqs
             structure
             pos-freqs)
            zip/seq-zip
            (iterate zip/next)
            (take-while (complement zip/end?))
            (filter #(string? (zip/node %)))
            (map zip/node)
            (string/join " ")))))
  ;; => ("this is some dog"
  ;;     "that is a test"
  ;;     "those is a dog"
  ;;     "those is a sample"
  ;;     "this is a test"
  ;;     "that is some test"
  ;;     "that is a car"
  ;;     "that is a car"
  ;;     "that is some dog"
  ;;     "this is a test")
  )

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

(defn node-constituents-with-tokens
  "Given a node of a parse tree, like ('NP (('PRP$ (\"my\" 'NN (\"name\"))))),
  returns a list of the top-level node tag and its first-level child tags.

  Like the above, but instead of converting leaf tokens to nil, leave them
  as-is. This can be used to merge a tree keeping the leaf tokens in a set
  and that tree can be used for a kind of markov generation of new text.
  "
  [node]
  (list
   (first node)
   (if (every? string? (map first (rest node)))
     (first (first (rest node)))
     (map first (first (rest node))))))

(comment
  (let [text "My name is Eric."
        parse-tree (->> (treebank-zipper text)
                        (iterate zip/next)
                        (take-while (complement zip/end?))
                        (filter (complement zip/branch?))
                        (map zip/path)
                        (map last)
                        (map node-constituents-with-tokens))]
    parse-tree)
  )

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
  (phrase-constituents ["My name is Eric."])
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

(defn zipper->freqs [zipper]
  (let [coll ()]))

(defn reduce-to-freqs [tree]
  (reduce
   (fn [acc [pos [children]]]
     (let [ks (map first children)
           vs (map rest children)]
       (-> acc
           (assoc pos (merge-with + {children 1}))
           )))
   {}
   tree))

(defn text->parsed-pos-freqs
  [text]
  (->> text
       (map tokenize)
       (map (partial string/join " "))
       parse
       (map tb/make-tree)
       (map unmake-tree)))

(comment
  (let [text ["My name is Eric and my style is wild."
              "Your name is unknown and where are you going?"]]
    (text->parsed-pos-freqs text))

  )

(defn text->pos
  "'My name is Eric.' -> (['my' 'PRP$'] ['name' 'NN'] ,,,)"
  [text]
  (->> text
       tokenize
       pos-tagger
       (map #(vector (string/lower-case (first %)) (second %)))))

(comment
  (text->pos "My name is Eric.")
  ;; => (["my" "PRP$"] ["name" "NN"] ["is" "VBZ"] ["eric" "NNP"] ["." "."])
  )

(defn text->pos-freqs
  "Turn a line of text into a map of parts of speech to word frequencies.
  Useful if you have a corpus and need to sample a particular part-of-speech."
  [text]
  (let [pos (text->pos text)]
    (into
     {}
     (->> pos
          (map reverse)
          (reduce
           (fn [acc [pos word]]
             (update
              acc
              (symbol pos)
              #(update % word (fnil inc 0))))
           {})))))

(comment
  (text->pos-freqs "My name is Eric and my style is wild.")
  ;; => {PRP$ {"my" 2},
  ;;     NN {"name" 1, "style" 1},
  ;;     VBZ {"is" 2},
  ;;     NNP {"eric" 1},
  ;;     CC {"and" 1},
  ;;     RB {"wild" 1},
  ;;     . {"." 1}}
  )

(defn texts->pos-freqs
  "Convenience method for turning multiple lines of texts into a
  tree with their pos-word-freqs merged."
  [texts]
  (->> texts
       (map text->pos-freqs)
       (apply
        merge-with
        (fn [old new]
          (merge-with + old new)))))

(comment
  (let [texts ["My name is Eric and my style is wildish."
               "Your name is unknown and your style is childish."]]
    (texts->pos-freqs texts))
  ;; => {PRP$ {"my" 2, "your" 2},
  ;;     NN {"name" 2, "style" 2},
  ;;     VBZ {"is" 4},
  ;;     NNP {"eric" 1},
  ;;     CC {"and" 2},
  ;;     JJ {"wildish" 1, "unknown" 1, "childish" 1},
  ;;     . {"." 2}}
  )

(defn pos-freqs->reverse
  "For getting a map of words to their pos frequencies"
  [tree]
  (reduce
   (fn [acc [pos word-freqs]]
     (reduce
      (fn [acc [word freq]]
        (update acc word (fnil conj {}) {pos freq}))
      acc
      word-freqs))
   {}
   tree))

(comment
  (let [pos-freqs {'NN {"name" 2 "style" 2}
                   'VBZ {"is" 4 "name" 1}}]
    (pos-freqs->reverse pos-freqs))
  ;; => {"name" {NN 2, VBZ 1}, "style" {NN 2}, "is" {VBZ 4}}
  )

(defn pos-tree->str [pos-tree]
  (->> pos-tree
       (map (fn [[k v]]
              (cons (str k) (seq v))))
       (map #(string/join " " %))
       (string/join "\n")))

(defn str->pos-tree [s]
  (let [lines (string/split s #"\n")]
    (into
     {}
     (map
      (fn [line]
        (->> line
             (#(string/split % #" " 2))
             ((fn [[k v]]
                (vector (symbol k) (into #{} (string/split v #" ")))))))
      lines))))

(comment
  (text->pos-freqs "My name is Eric.")

  (->> ["My name is Eric."
        "Your name is Taylor."
        "Is your feeling hurt?"
        "How are you feeling?"
        "It is a lovely feeling."
        "Your name is not Eric."
        "Who is your mother and what does she do?"]
       (pos-constituent-frequencies)

       #_#_(apply
        merge-with
        (fn [a b]
          (merge-with + a b)))
       (pos-freqs->reverse))

  )

(defn reverse-children-to-parents
  "We want to work from the reverse, or backwards,
  representation of pos-constituents-frequencies.
  Pos-constituent-frequencies is good for generating text top-down. But we want
  to generate text bottom-up for rhyming."
  [pos-freqs]
  (reduce
   (fn [acc [k v]]
     (reduce
      (fn [acc [children freq]]
        (update acc children assoc k freq))
      acc
      v))
   {}
   pos-freqs))

(defn ways-to-make-a [pos pos-freqs]
  (let [children (pos-freqs pos)]
    (->> children
         ((juxt keys vals))
         (apply map vector)
         (into {}))))

(comment
  (let [pos-freqs (->> ["My name is Eric."
                        "Your name is Taylor."
                        "Is your feeling hurt?"
                        "How are you feeling?"
                        "It is a lovely feeling."
                        "Your name is not Eric."
                        "Who is your mother and what does she do?"]
                       (pos-constituent-frequencies))]
    (->> pos-freqs
         (ways-to-make-a 'S)
         (#(get % 'NN))))

  (let [pos-freqs (->> ["My name is Eric."
                        "Your name is Taylor."
                        "Is your feeling hurt?"
                        "How are you feeling?"
                        "It is a lovely feeling."
                        "Your name is not Eric."
                        "Who is your mother and what does she do?"]
                       (pos-constituent-frequencies))]
    (->> (reverse-children-to-parents pos-freqs)
         (remove (fn [[k v]] (and (= 1 (count k))
                                  (pos-freqs (first k)))))
         (into {})))

  )

(defn most-likely-parts-of-speech
  [phrase]
  (top-k-sequences prhyme-pos-tagger (tokenize phrase)))

(comment
  (let [text "a dog"]
    (first
     (map #(.getOutcomes %)
          (most-likely-parts-of-speech text))))
  ;; => ["PRP" "VBP" "DT" "NN" "."]


  (map (juxt #(.getOutcomes %)
             #(map float (.getProbs %)))
       (top-k-sequences prhyme-pos-tagger (tokenize "")))

  )

(defn loc-path
  [loc]
  (->> loc
       zip/path
       (map first)
       (filter symbol?)))

(defn breadth-first
  [zipper]
  (letfn [(zip-children [loc]
            (try
              (when-let [first-child (zip/down loc)]
                (take-while
                 (complement nil?)
                 (iterate zip/right first-child)))
              (catch Exception e
                (println (zip/root loc))
                (throw e))))]
    (loop [result []
           queue (conj clojure.lang.PersistentQueue/EMPTY zipper)]
      (if (seq queue)
        (let [[zipper children] ((juxt identity zip-children) (peek queue))]
          (recur (conj result zipper) (into (pop queue) children)))
        result))))

(defn loc-children
  [loc]
  (when-let [first-child (zip/down loc)]
    (->> (take-while
          (complement nil?)
          (iterate zip/right first-child))
         (map first))))

(defn part-of-speech-children
  [loc]
  (->> loc
       (iterate zip/next)
       (take-while (complement zip/end?))
       (map (fn [loc]
              (when (symbol? (zip/node loc))
                [(->> (zip/path loc)
                      (map first))
                 (->> (zip/right loc)
                      (zip/node)
                      (map first))])))
       (remove (comp nil? second))))

(defn parts-of-speech-trie-entries
  "Given a zipper of a treebank parse tree, returns a sequence of
  key-value pairs where the key is a sequence of parts-of-speech
  to traverse down the tree and the values are the children
  in the parse tree at that path.

  This can be plugged into a Trie with frequency data to
  give you the following kind of info:

  {'(TOP)
   {'(S)     {:freq    534
              '(NP VB) {:freq 233}
              '(NP ADJP VB {:freq 210})
              ,,,}
    '(SBARQ) {:freq 110}
    '(SQ)    {:freq 23}}}
  "
  [zipper]
  (try
    (->> (breadth-first zipper)
         (filter (comp symbol? zip/node))
         (map zip/prev)
         (filter zip/branch?)
         (mapv (fn [loc]
                 [(->> (zip/next loc)
                       (zip/path)
                       (map first)
                       (filter symbol?))
                  (let [child (zip/next (zip/next loc))]
                    (if (zip/branch? (zip/next child))
                      (map first (zip/node child))
                      (zip/node child)))])))
    (catch Exception e
      (println (zip/node zipper))
      (throw e))))

(comment
  (parts-of-speech-trie-entries
   (zip/seq-zip
    '(TOP
      ((S
        ((NP
          ((NP ((NN ("Everything")))) (PP ((IN ("of")) (NP ((NN ("today"))))))))
         (VP ((VBZ ("is")) (VP ((VBG ("falling"))))))
         (. ("."))))))))
  ;; => ([(TOP) (S)]
  ;;     [(TOP S) (NP VP .)]
  ;;     [(TOP S NP) (NP PP)]
  ;;     [(TOP S VP) (VBZ VP)]
  ;;     [(TOP S .) (".")]
  ;;     [(TOP S NP NP) (NN)]
  ;;     [(TOP S NP PP) (IN NP)]
  ;;     [(TOP S VP VBZ) ("is")]
  ;;     [(TOP S VP VP) (VBG)]
  ;;     [(TOP S NP NP NN) ("Everything")]
  ;;     [(TOP S NP PP IN) ("of")]
  ;;     [(TOP S NP PP NP) (NN)]
  ;;     [(TOP S VP VP VBG) ("falling")]
  ;;     [(TOP S NP PP NP NN) ("today")])

  )


;;;; Grammar Trie
;;
;; Create a trie from treebank parsed grammar trees.

(defn -split-text-into-sentences
  "Splits text on newlines, periods, exclamation and question marks."
  [text]
  (->> text
       (#(string/replace % #"([\.\?\!\n]+)" "$1\n"))
       (string/split-lines)))

(defn -flatten-trie-entry-to-all-subkeys
  "Turns

  [[k1 k2 k3] v]

  into

  [[[k1 k2 k3] v]]
  [[k2 k3] v]]
  [[k3] v]]]

  This is useful for creating a trie from a grammar tree. It's
  nice to know that k3 is a child of both [k1 k2] and [k2] so
  if you need to generate a [k2] in isolation, you have
  acces to [k1 k2] and [k4 k2] and [kn k2] etc... all under the
  top-level key [k2].
  "
  [[k v]]
  (loop [result []
         k k]
    (if (empty? k)
      result
      (recur (conj result [k v])
             (rest k)))))

(defn -normalize-text
  [[k v]]
  (if (string? (first v))
    [k (string/lower-case (first v))]
    [k v]))

(defn english?
  [text]
  (->> text
       (#(string/replace % #"\W" " "))
       (#(string/replace % #" +" " "))
       (#(string/split % #" "))
       (every? #(dict/cmu-with-stress-map (string/lower-case %)))))

(defn text->grammar-trie-map-entry
  "Processes text into key value pairs where
  the keys are parts-of-speech paths and the values
  are the children at that path.

  Ready to be inserted into a trie."
  [text]
  (->> text
       (-split-text-into-sentences)
       (map string/trim)
       (filter english?)
       (remove empty?)
       (mapv treebank-zipper)
       (remove nil?)
       (map parts-of-speech-trie-entries)
       (reduce into [])
       (map -flatten-trie-entry-to-all-subkeys)
       (reduce into [])
       (mapv -normalize-text)
       (mapv (fn [[k v]]
               (clojure.lang.MapEntry. (into (vec k) [v]) v)))))

(comment
  (let [text "Hi my name. Is Eric? \n What is yours? Fooaba brosaet"]
    (text->grammar-trie-map-entry text)
    #_(->> text
         -split-text-into-sentences
         ))

  )
(defn -new-key
  "Associates key with an auto-incrementing ID
  and the ID with the key.

  This 'database' is an atom that maps
  keys to integer ids and integer ids to keys.

  This lets us use integers throughout the trie data structure,
  which ends up being a lot more efficient and prepares the trie
  for being turned into a tightly-packed-trie."
  [database k]
  (let [next-id (@database ::next-id)]
    (swap!
     database
     #(-> %
          (assoc k next-id)
          (assoc next-id k)
          (update ::next-id inc)))
    next-id))

(defn make-database-stateful-xf
  "This 'database' is an atom that maps
  keys to integer ids and integer ids to keys.

  This lets us use integers throughout the trie data structure,
  which ends up being a lot more efficient and prepares the trie
  for being turned into a tightly-packed-trie.

  Takes an atom and returns a function that takes a Trie key/value.
  When the returned function is called, it checks to see
  if the key is in the database and if so it returns the associated id.
  If not, it increments the id (which is stored in the database
  under :next-id) and returns that new id."
  [database]
  (fn [[k v]]
    (let [k' (mapv (fn [kn]
                     (if-let [id (get @database kn)]
                       id
                       (-new-key database kn)))
                   k)]
      [k' 1])))

