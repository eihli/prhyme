(ns com.owoga.prhyme.nlg.core
  (:import (simplenlg.framework NLGFactory)
           (simplenlg.lexicon Lexicon)
           (simplenlg.realiser.english Realiser)
           (simplenlg.features Feature Form)))

(def lexicon (Lexicon/getDefaultLexicon))
(def nlg-factory (NLGFactory. lexicon))
(def realiser (Realiser. lexicon))

(defn realise [element]
  (.realise realiser element))

(let [text "my dog is happy"
      sentence (.createSentence nlg-factory text)]
  (.realiseSentence realiser sentence))

(let [clause (.createClause nlg-factory)
      _ (.setSubject clause "us")
      _ (.setVerb clause "chase")
      _ (.setObject clause "the monkey")]
  (.realiseSentence realiser clause))

(let [NP (.createNounPhrase nlg-factory "zipper")
      _ (.setDeterminer NP "another")
      VP1 (.createVerbPhrase nlg-factory "eat")
      clause (.createSentence nlg-factory [NP VP1])]
  (.realiseSentence realiser clause))

(defn create-noun-phrase
  ([]
   (.createNounPhrase nlg-factory))
  ([noun]
   (.createNounPhrase nlg-factory noun))
  ([specifier noun]
   (.createNounPhrase nlg-factory specifier noun)))

(defn set-determiner [noun-phrase determiner]
  (.setDeterminer noun-phrase determiner)
  noun-phrase)

(defn set-noun [noun-phrase noun]
  (.setNoun noun-phrase noun)
  noun-phrase)

(defn create-adjective-phrase
  ([]
   (.createAdjectivePhrase nlg-factory))
  ([adjective]
   (.createAdjectivePhrase nlg-factory adjective)))

(defn create-adverb-phrase
  ([]
   (.createAdverbPhrase nlg-factory))
  ([adverb]
   (.createAdverbPhrase nlg-factory adverb)))

(defn create-verb-phrase
  ([]
   (.createVerbPhrase nlg-factory))
  ([verb]
   (.createVerbPhrase nlg-factory verb)))

(defn set-feature [element feature-name feature-value]
  (.setFeature element feature-name feature-value)
  element)

(defn set-pre-modifier [element new-pre-modifier]
  (.setPreModifier element new-pre-modifier)
  element)

(ns-unmap *ns* 'create-element)
(defmulti create-element
  (fn [children] (doall (map first children))))

(defmethod create-element '(PRP$)
  [[[_ child]]] (.createNounPhrase nlg-factory child))

(defmethod create-element '(NN)
  [[[_ child]]] (.createNounPhrase nlg-factory child))

(defmethod create-element '(NNP)
  [[[_ child]]]
  (let [noun-phrase (.createNounPhrase nlg-factory child)]
    (.setPlural noun-phrase true)
    noun-phrase))

;; A determiner by itself is usually a subject.
;; i.e. "This is a test." <- "This" is tagged by the treebank parser
;; as a determiner
(defmethod create-element '(DT)
  [[[_ child]]]
  (.createNounPhrase nlg-factory child))

(defmethod create-element '(DT NN NN)
  [[DT NN1 NN2]]
  (let [dt (create-element (list DT))
        nn1 (create-element (list NN1))
        nn2 (create-element (list NN2))
        clause (.createNounPhrase nlg-factory)]
    (.setSpecifier clause dt)
    (.setPreModifier clause nn1)
    (.setNoun clause nn2)
    clause))

(realise (create-element '((DT "a") (NN "sample") (NN "test"))))

(defmethod create-element '(PRP$ NN)
  [[prp$ nn]]
  (-> (.createNounPhrase
       nlg-factory
       (create-element (list nn)))
      (set-pre-modifier
       (-> (create-element (list prp$))
           (set-feature Feature/POSSESSIVE true)))))

(defmethod create-element
  '(NP)
  [[[_ child]]]
  (create-element child))

(realise (create-element '((NP ((PRP$ "Eric") (NN "test"))))))

(realise (create-element '((DT "This"))))
(create-element '((NN "test")))
(realise (create-element '((PRP$ "Eric") (NN "test"))))
(realise (create-element '((NNP "tests"))))

(defmethod create-element '(VB)
  [[[_ child]]]
  (.createVerbPhrase nlg-factory child))

(realise (create-element '((VB "run"))))

(defmethod create-element '(VBZ)
  [[[_ child]]]
  (.createVerbPhrase nlg-factory child))

(defmethod create-element '(VBZ NP)
  [[VBZ NP]]
  (let [np (create-element (list NP))
        vbz (create-element (list VBZ))
        clause (.createClause nlg-factory)]
    (.setObject clause np)
    (.setVerb clause vbz)
    clause))

(comment
  (realise (create-element '((VBZ "is") (NP ((NN "test"))))))
  )

(comment
  (-> (create-noun-phrase)
      (set-determiner "a")
      (set-noun "test"))

  (-> (create-verb-phrase "let")
      (set-feature Feature/FORM Form/INFINITIVE)
      (#(realise %)))


  )
