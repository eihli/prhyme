(ns com.owoga.prhyme.grammar)

(def root-states
  [{::tk/name :failed
    ::tk/transitions [{::tk/on tk/_ ::tk/to :failed}]}
   {::tk/name :object
    ::tk/transitions [{::tk/on :adjectives ::tk/to :obj-adj}
                      {::tk/on :nouns ::tk/to :obj-noun}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :obj-adj
    ::tk/transitions [{::tk/on :nouns ::tk/to :obj-noun}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :obj-noun
    ::tk/transitions [{::tk/on :verbs ::tk/to :verbs}
                      {::tk/on :adverbs ::tk/to :adverbs}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :verbs
    ::tk/transitions [{::tk/on :nouns ::tk/to :subj-noun}
                      {::tk/on :adjectives ::tk/to :subj-adj}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :adverbs
    ::tk/transitions [{::tk/on :verbs ::tk/to :verbs}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :subj-noun
    ::tk/transitions [{::tk/on :nouns ::tk/to :obj-noun}
                      {::tk/on :adjectives ::tk/to :obj-adj}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}
   {::tk/name :subj-adj
    ::tk/transitions [{::tk/on :nouns ::tk/to :subj-noun}
                      {::tk/on tk/_ ::tk/to :object ::tk/actions [:failed]}]}])

(def root-fsm
  {::tk/states root-states
   ::tk/action! (fn [{::tk/keys [signal action] :as fsm}]
                  (case signal
                    :failed (println "Failed! " signal " " action))
                  fsm)
   ::tk/state :object})
