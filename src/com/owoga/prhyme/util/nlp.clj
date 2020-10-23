(ns com.owoga.prhyme.util.nlp
  (:require [opennlp.nlp :as nlp]
            [opennlp.treebank :as tb]
            [clojure.string :as string]))

(def tokenize (nlp/make-tokenizer "models/en-token.bin"))
(def get-sentences (nlp/make-sentence-detector "models/en-sent.bin"))
(def parse (tb/make-treebank-parser "en-parser-chunking.bin"))

(defn valid-sentence?
  "Tokenizes and parses the phrase using OpenNLP models from
  http://opennlp.sourceforge.net/models-1.5/

  If the parse tree has an 'S as the top-level tag, then
  we consider it a valid English sentence."
  [phrase]
  (->> phrase
       tokenize
       (string/join " ")
       vector
       parse
       first
       tb/make-tree
       (#(= 'S (:tag (first (:chunk %)))))))

