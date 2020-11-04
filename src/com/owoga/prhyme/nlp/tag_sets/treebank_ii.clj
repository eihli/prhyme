(ns com.owoga.prhyme.nlp.tag-sets.treebank-ii)

;;;; Penn Treebank II Constituent Tags
;;;; http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html#MD
(def clauses
  {'S "simple declarative clause, i.e. one that is not introduced by a (possible empty) subordinating conjunction or a wh-word and that does not exhibit subject-verb inversion."
   'SBAR "Clause introduced by a (possibly empty) subordinating conjunction."
   'SBARQ "Direct question introduced by a wh-word or a wh-phrase. Indirect questions and relative clauses should be bracketed as SBAR, not SBARQ."
   'SINV "Inverted declarative sentence, i.e. one in which the subject follows the tensed verb or modal."
   'SQ "Inverted yes/no question, or main clause of a wh-question, following the wh-phrase in SBARQ."})

(def phrases
  {'ADJP "Adjective Phrase."
   'ADVP "Adverb Phrase."
   'CONJP "Conjunction Phrase."
   'FRAG "Fragment."
   'INTJ "Interjection. Corresponds approximately to the part-of-speech tag UH."
   'LST "List marker. Includes surrounding punctuation."
   'NAC "Not a Constituent; used to show the scope of certain prenominal modifiers within an NP."
   'NP "Noun Phrase."
   'NX "Used within certain complex NPs to mark the head of the NP. Corresponds very roughly to N-bar level but used quite differently."
   'PP "Prepositional Phrase."
   'PRN "Parenthetical."
   'PRT "Particle. Category for words that should be tagged RP."
   'QP "Quantifier Phrase (i.e. complex measure/amount phrase); used within NP."
   'RRC "Reduced Relative Clause."
   'UCP "Unlike Coordinated Phrase."
   'VP "Vereb Phrase."
   'WHADJP "Wh-adjective Phrase. Adjectival phrase containing a wh-adverb, as in how hot."
   'WHAVP "Wh-adverb Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing a wh-adverb such as how or why."
   'WHNP "Wh-noun Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing some wh-word, e.g. who, which book, whose daughter, none of which, or how many leopards."
   'WHPP "Wh-prepositional Phrase. Prepositional phrase containing a wh-noun phrase (such as of which or by whose authority) that either introduces a PP gap or is contained by a WHNP."
   'X "Unknown, uncertain, or unbracketable. X is often used for bracketing typos and in bracketing the...the-constructions."})

(def words
  {'CC "Coordinating conjunction"
   'CD "Cardinal number"
   'DT "Determiner"
   'EX "Existential there"
   'FW "Foreign word"
   'IN "Preposition or subordinating conjunction"
   'JJ "Adjective"
   'JJR "Adjective, comparative"
   'JJS "Adjective, superlative"
   'LS "List item marker"
   'MD "Modal"
   'NN "Noun, singular or mass"
   'NNS "Noun, plural"
   'NNP "Proper noun, singular"
   'NNPS "Proper noun, plural"
   'PDT "Predeterminer"
   'POS "Possessive ending"
   'PRP "Personal pronoun"
   'PRP$ "Possessive pronoun (prolog version PRP-S)"
   'RB "Adverb"
   'RBR "Adverb, comparative"
   'RBS "Adverb, superlative"
   'RP "Particle"
   'SYM "Symbol"
   'TO "to"
   'UH "Interjection"
   'VB "Verb, base form"
   'VBD "Verb, past tense"
   'VBG "Verb, gerund or present participle"
   'VBN "Verb, past participle"
   'VBP "Verb, non-3rd person singular present"
   'VBZ "Verb, 3rd person singular present"
   'WDT "Wh-determiner"
   'WP "Wh-pronoun"
   'WP$ "Possessive wh-pronoun (prolog version WP-S)"
   'WRB "Wh-adverb"})

(def form-function-discrepencies
  {'-ADV "(adverbial) - marks a constituent
  other than ADVP or PP when it is used adverbially (e.g. NPs or
  free (\"headless\" relatives). However, constituents that themselves are
  modifying an ADVP generally do not get -ADV. If a more specific tag is
  available (for example, -TMP) then it is used alone and -ADV is implied. See
  the Adverbials section."
   '-NOM "(nominal) - marks free (\"headless\") relatives
  and gerunds when they act nominally."})
