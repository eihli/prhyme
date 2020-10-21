(ns com.owoga.prhyme.util.nlp
  (:require [opennlp.nlp :as nlp]
            [opennlp.treebank :as tb]
            [clojure.string :as string]))

(def tokenize (nlp/make-tokenizer "models/en-token.bin"))
(def get-sentences (nlp/make-sentence-detector "models/en-sent.bin"))
(def parse (tb/make-treebank-parser "en-parser-chunking.bin"))

(defn valid-sentence? [phrase]
  (->> phrase
       tokenize
       (string/join " ")
       vector
       parse
       first
       tb/make-tree
       (#(= 'S (:tag (first (:chunk %)))))))

