(ns com.owoga.prhyme.nlp.core
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
  (let [phrase "Blood falls."]
    (->> phrase
         tokenize
         (top-k-sequences prhyme-pos-tagger)
         (map (juxt #(.getOutcomes %)
                    #(map float (.getProbs %))))))

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
    (zip/seq-zip tree)))

(comment
  (let [zipper (treebank-zipper "This is a zipper test.")]
    zipper)
  ;; => [((TOP
  ;;       ((S
  ;;         ((NP ((DT ("This"))))
  ;;          (VP ((VBZ ("is")) (NP ((DT ("a")) (NN ("zipper")) (NN ("test"))))))
  ;;          (. (".")))))))
  ;;     nil]
  )

(defn treebank-zipper-1
  "Turns a bit of text into a parse tree into a zipper."
  [texts]
  (let [tree (->> texts
                  (map tokenize)
                  (map (partial string/join " "))
                  parse
                  (map tb/make-tree)
                  unmake-tree)]
    (zip/seq-zip tree)))

(defn seq-zip [zipper]
  (->> zipper
       (iterate zip/next)
       (take-while (complement zip/end?))))

(defn seq-nodes [zipper]
  (->> zipper
       seq-zip
       (map zip/node)))

(defn phrase-level? [node]
  (and (seq? node)
       (symbol? (first node))))

(defn word-level? [node]
  (and (seq? node)
       (symbol? (first node))
       (= 1 (count (second node)))
       (string? (first (second node)))))

(defn leaf-pos-paths [zipper]
  (->> zipper
       seq-zip
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
  (let [zipper (treebank-zipper-1 ["This is a zipper test."
                                   "And this is another one."])]
    (leaf-pos-paths zipper))
;; => ((TOP S NP DT "This")
;;     (TOP S VP VBZ "is")
;;     (TOP S VP NP DT "a")
;;     (TOP S VP NP NN "zipper")
;;     (TOP S VP NP NN "test")
;;     (TOP S . ".")
;;     (TOP S CC "And")
;;     (TOP S NP DT "this")
;;     (TOP S VP VBZ "is")
;;     (TOP S VP NP DT "another")
;;     (TOP S VP NP CD "one")
;;     (TOP S . ".")) 
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
       seq-nodes
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
  (let [zipper (treebank-zipper-1 ["This is a test and that is not a test."
                                   "My name is Eric."
                                   "Go to the store."
                                   "Your name is not Eric."
                                   "This is a sample test."])
        freqs (->> zipper
                   pos->children-freqs)]
    freqs)
;; => {NP {(DT) 3, (DT NN) 3, (PRP$ NN) 2, (NNP) 2, (DT NN NN) 1},
;;     VB {"Go" 1},
;;     VBZ {"is" 5},
;;     S {(S CC S .) 1, (NP VP) 2, (NP VP .) 3},
;;     RB {"not" 2},
;;     NNP {"Eric" 2},
;;     TO {"to" 1},
;;     DT {"This" 2, "a" 3, "that" 1, "the" 1},
;;     TOP {(S) 4, (VP) 1},
;;     CC {"and" 1},
;;     NN {"test" 3, "name" 2, "store" 1, "sample" 1},
;;     PP {(TO NP) 1},
;;     VP {(VBZ NP) 3, (VBZ RB NP) 2, (VB PP .) 1},
;;     PRP$ {"My" 1, "Your" 1},
;;     . {"." 5}} 
  )

(defn generate-from-freqs
  "freqs is a map of parts-of-speech to a map of their constituent parts-of-speech
  and the number of times each constituent is seen in a corpus.

  start is the part-of-speech to start generating for.
  "
  [freqs start]
  (let [zipper (zip/seq-zip (list start))]
    (loop [zipper zipper]
      (cond
        (zip/end? zipper) (zip/root zipper)

        (zip/branch? zipper) (recur (zip/next zipper))

        (freqs (zip/node zipper))
        (recur
         (zip/next
          (zip/next
           (zip/replace
            zipper
            (list
             (zip/node zipper)
             (first (rand-nth (seq (freqs (zip/node zipper))))))))))

        :else (recur (zip/next zipper))))))

(comment
  (let [zipper (treebank-zipper-1 ["This is a zipper test in the car."
                                   "And this is another one."
                                   "Here are some sample sentences."
                                   "Let's see what we can generate."
                                   "This is a big adjective."
                                   "That's an ugly sentence."
                                   "The corpus will be simple to start."])
        freqs (->> zipper
                   pos->children-freqs)]
    freqs
    (->> (generate-from-freqs freqs 'TOP))
    (->> (repeatedly
          (fn []
            (->> (generate-from-freqs freqs 'TOP)
                 (zip/seq-zip)
                 (iterate zip/next)
                 (take-while (complement zip/end?))
                 (filter (complement zip/branch?))
                 (map zip/node)
                 (filter string?)
                 (string/join " "))))
         (filter valid-sentence?)
         (take 10)))
  ;; => ("Here be what start big And This test see what be That one start . . That one ."
  ;;     "Here are This ugly zipper ."
  ;;     "And an is The sample ."
  ;;     "generate simple see This corpus test to Let And a Let what an corpus adjective to be And another ugly car can see ugly start what this one to are . . . . . . ."
  ;;     "This sample sentences Let big 's a sample adjective in we ."
  ;;     "will generate what Let"
  ;;     "Here be ugly That sentence generate a big test ."
  ;;     "Here is this corpus sentences in a test this sample sentences ."
  ;;     "And some ugly zipper 's 's ."
  ;;     "Here generate big the adjective 's another corpus an .")
  ;; => ((TOP
  ;;      ((SINV
  ;;        ((ADVP ((RB "Here")))
  ;;         (VP
  ;;          ((TO "to")
  ;;           (VP
  ;;            ((TO "to")
  ;;             (VP
  ;;              ((VB "Let")
  ;;               (SBAR
  ;;                ((WHNP ((WP "what")))
  ;;                 (S
  ;;                  ((NP ((DT "this") (CD "one")))
  ;;                   (VP
  ;;                    ((VBZ "is")
  ;;                     (NP ((DT "The") (NN "adjective") (NN "car")))
  ;;                     (PP
  ;;                      ((IN "in")
  ;;                       (NP
  ;;                        ((DT "The") (NN "test") (NNS "sentences")))))))))))))))))
  ;;         (NP ((DT "another") (CD "one")))
  ;;         (. "."))))))



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

(comment
  (let [zipper (treebank-zipper-1 ["This is a zipper test."
                                   "And this is another one."
                                   "This is not a test."])]
    (->> zipper
         leaf-pos-paths
         (map pos-freq)
         (apply deep-merge-with +)))
;; => {TOP
;;     {:freq 18,
;;      S
;;      {:freq 18,
;;       NP {:freq 3, DT {:words {"This" 2, "this" 1}, :freq 3}},
;;       VP
;;       {:freq 11,
;;        VBZ {:words {"is" 3}, :freq 3},
;;        NP
;;        {:freq 7,
;;         DT {:words {"a" 2, "another" 1}, :freq 3},
;;         NN {:words {"zipper" 1, "test" 2}, :freq 3},
;;         CD {:words {"one" 1}, :freq 1}},
;;        RB {:words {"not" 1}, :freq 1}},
;;       . {:words {"." 3}, :freq 3},
;;       CC {:words {"And" 1}, :freq 1}}}} 
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

(comment
  (let [text "My name is Eric."
        zipper (treebank-zipper text)]
    (map identity (seq-zip zipper)))

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
  "We want to work from the reverse, or backwards, representation of pos-constituents-frequencies.
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
         #_(ways-to-make-a 'S)
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
