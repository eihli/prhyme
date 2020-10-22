(ns com.owoga.prhyme.util.lovecraft
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [com.owoga.prhyme.util.weighted-rand :as wr]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util.nlp :as nlp]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [com.owoga.prhyme.frp :as frp]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(tufte/add-basic-println-handler! {})

(def ^:dynamic *base-url* "https://www.hplovecraft.com/writings/texts/")

(def words-map
  (into {} (map #(vector (string/lower-case (:word %)) %) frp/words)))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(comment
  (fetch-url *base-url*))

(defn links []
  (map
   #(str *base-url* (first (html/attr-values % :href)))
   (html/select
    (fetch-url *base-url*)
    [:li :> [:a (html/attr? :href)]])))

(defn contentful-sections [nodes]
  (->> nodes
       (map html/text)
       (filter #(> (count %) 100))))

(defn text-from-link [link]
  (->> (html/select
        (fetch-url link)
        [:body])
       (first)
       (html/text)
       ((fn [s] (string/replace s #"[\s\u00A0]+" " ")))))

(defn cleanup [content]
  (-> content
      (string/replace #"Return to.*$" "")
      (string/replace #"Home.*?This Site" "")
      (string/replace #"[^a-zA-Z -]+" "")))

(defn tokens [content]
  (string/split content #"\s+"))

(defn append-to-file [filepath text]
  (with-open [w (io/writer filepath :append true)]
    (.write w text)))

(defn scrape []
  (run!
   (fn [link]
     (->> (text-from-link link)
          (cleanup)
          (#(str % "\n"))
          (append-to-file "lovecraft.txt")))
   (take 10 (links))))

(defn tokens-from-file [file]
  (with-open [r (io/reader file)]
    (tokens (slurp r))))

(defn window [n]
  (fn [coll]
    (cond
      (empty? coll) []
      (< (count coll) n) []
      :else (cons (take n coll)
                  (lazy-seq ((window n) (drop n coll)))))))

(defnp markov [tokens]
  (->> tokens
       (map
        (fn [token]
          (let [k (butlast token)
                v (last token)]
            [k v])))
       (reduce
        (fn [a [k v]]
          (update-in a [k v] (fnil inc 0)))
        {})))

(defnp running-total
  ([coll]
   (running-total coll 0))
  ([coll last-val]
   (cond
     (empty? coll) nil
     :else (cons (+ last-val (first coll))
                 (lazy-seq
                  (running-total
                   (rest coll)
                   (+ last-val (first coll))))))))

(defnp weighted-rand [weights]
  (let [running-weights (running-total weights)
        rand-val (rand (last running-weights))]
    (loop [i 0]
      (if (> (nth running-weights i) rand-val)
        i
        (recur (inc i))))))

(defnp choose-from-markov-possibilities [possibilities]
  (if (empty? possibilities)
    nil
    (let [weights (vals possibilities)
          rng (wr/from-weights weights)
          index (wr/nextr rng nil)]
      (nth (keys possibilities) index))))

(def word-set (into #{} (->> prhyme/words
                             (map first)
                             (map string/lower-case)
                             (map #(string/replace % #"\(\d+\)" "")))))

(defn normalize-tokens [tokens]
  (->> tokens
       (map string/lower-case)
       (filter word-set)))

(defn main []
  (->> (tokens-from-file "lovecraft.txt")
       (reverse)
       (normalize-tokens)
       ((window 2))
       (markov)
       (into {})))

(defn make-markov-picker [markov-data]
  (fn [k]
    (choose-from-markov-possibilities
     (get markov-data k {}))))

(defn synonym?
  "Given a possibility, like [\"foo\" 3]
  which says that foo follows a particular key with
  a weight of 3, a word is a synonym of that possibility
  if the word is a synonym ."
  [p synonyms]
  (synonyms p))

(defnp adjust-for-synonyms
  "If a word is in a set of synonyms, adjust its weight upwards."
  [synonyms]
  (fn [possibilities]
    (reduce
     (fn [p s]
       (if (s p)
         (update p s #(* 5 %))
         p))
     possibilities
     synonyms)))

(defnp adjust-for-rimes
  [target-rime dictionary]
  (fn [possibilities]
    (into
     {}
     (map
      (fn [[p v]]
        (let [possibility (get dictionary p)
              factor (count
                      (frp/consecutive-matching
                       target-rime
                       possibility
                       :rimes))]
          [p (* v (max 1 (* factor 4)))]))
      possibilities))))

(comment
  ((adjust-for-synonyms #{"war" "famine"})
   {"war" 1
    "disease" 3})
  ;; => {"war" 5, "disease" 3}
  ((adjust-for-rimes
    (frp/make-word ["magic" "M" "AE" "JH" "IH" "K"])
    words-map)
   {"tragic" 3
    "trick" 2
    "foo" 1})
  ;; => {"tragic" 24, "trick" 8, "foo" 1} 
  )

(defonce lovecraft-markov (read-string (slurp "lovecraft.edn")))
(defonce markover (make-markov-picker lovecraft-markov))

(defn markov-key [key-fn]
  (fn [text]
    (key-fn text)))

(defn gen-from [m p initial]
  (loop [r (list initial)]
    (cond
      (p r) (recur (cons (m (list (first r))) r))
      :else r)))


(defn rhyming-words
  "List of rhyming words sorted by quality of rhyme."
  [target]
  (let [target-phrase (->> target
                           (frp/phrase->word frp/words)
                           (#(assoc % :rimes? true)))]
    (->> target-phrase
         (#(assoc % :rimes? true))
         (frp/prhyme frp/words)
         (sort-by
          #(- (count
               (frp/consecutive-matching
                %
                target-phrase
                :rimes)))))))

(defn markov-rhymes [markov-data rhyming-words]
  (->> (map
        (fn [word]
          (->> word
               :word
               string/lower-case
               (#(string/replace % #"\(\d+\)" ""))
               (#(vector % (get markov-data (list %))))))
        rhyming-words)
       (into #{})
       (remove
        (fn [[w p]]
          (nil? p)))))

(defn markov-gen [markov-data initial]
  (let [m (make-markov-picker markov-data)]
    (loop [r initial]
      (if (> (count r) 5)
        r
        (recur (cons (m (list (first r)))
                     r))))))

(defn make-rhymes [markov-data target]
  (let [target-word (frp/phrase->word frp/words target)
        rhyming-words (rhyming-words target)
        markov--rhymes (markov-rhymes markov-data rhyming-words)
        rime-adjuster (adjust-for-rimes target-word words-map)
        modified-markov-data
        (merge
         markov-data
         (into {}
               (map (fn [[word weights]]
                      [word (rime-adjuster weights)])
                    markov--rhymes)))]
    (->> rhyming-words
         (markov-rhymes modified-markov-data)
         (map
          (fn [[k v]]
            (markov-gen modified-markov-data (list k))))
         (map #(remove nil? %)))))

(defn adjust-for-over-syllables
  "Adjust weights to prefer not going over the number
  of syllables of the target word."
  [target]
  (fn [words]
    (p :adjust-for-syllables
       (map
        (fn [word]
          (if (or (nil? (:syllable-count word))
                  (nil? (:syllables target)))
            (println word target))
          (cond
            (= (:syllable-count word) (count (:syllables target)))
            (as-> word word
              (assoc word :weight (* 3 (:weight word)))
              (assoc word :adjusted-for-syllables-factor 3))

            (< (:syllable-count word) (count (:syllables target)))
            (as-> word word
              (assoc word :weight (* 2 (:weight word)))
              (assoc word :adjusted-for-syllables-factor 2))

            :else
            (as-> word word
              (assoc word :weight (* 1 (:weight word)))
              (assoc word :adjusted-for-syllables-factor 1))))
        words))))

(comment
  (let [words (->> ["distort" "kiss" "sport"]
                   (map #(frp/phrase->word frp/words %))
                   (map #(assoc % :weight 1)))
        target (->> "report"
                    (frp/phrase->word frp/words)
                    (#(assoc % :syllables (:syllables %))))
        adjuster (adjust-for-over-syllables target)]
    (adjuster words)))

(defn adjust-for-rhymes
  "Adjust weights to prefer words that rhyme"
  [target]
  (fn [words]
    (p :adjust-for-rhymes
       (map
        (fn [word]
          (let [factor (max 0.001 (count (frp/consecutive-matching word target :rimes)))]
            (as-> word word
              (assoc word :weight (* factor (:weight word)))
              (assoc word :adjust-for-rhyme-factor factor))))
        words))))

(defn adjust-for-rhymes-1
  "Adjust weights to prefer words that rhyme"
  [target percent]
  (fn [words]
    (let [ratio (/ percent (- 1 percent))
          [rhymes non-rhymes]
          ((juxt filter remove)
           (fn [word]
             (< 0 (count (frp/consecutive-matching word target :rimes))))
           words)
          weight-non-rhymes (apply + (map :weight non-rhymes))
          target-weight-rhymes (* ratio weight-non-rhymes)
          count-rhymes (max 1 (count rhymes))
          adjustment-rhyme (/ target-weight-rhymes count-rhymes)]
      (concat
       non-rhymes
       (map
        (fn [rhyme]
          (as-> rhyme rhyme
            (assoc rhyme :weight (* adjustment-rhyme (:weight rhyme)))
            (assoc rhyme :adjust-for-rhyme-factor adjustment-rhyme)))
        rhymes)))))

(defn adjust-for-membership-1
  [set_ percent]
  (let [ratio (- 1 percent)]
    (fn [words]
      (let [[members non-members]
            ((juxt filter remove)
             #(set_ (:norm-word %))
             words)
            weight-non-members (apply + (map :weight non-members))
            target-weight-members (* ratio weight-non-members)
            count-members (count members)
            adjustment-members (/ target-weight-members count-members)]
        (concat
         (map
          (fn [member]
            (as-> member member
              (assoc member :weight (* adjustment-members (:weight member)))
              (assoc member :adjustment-for-membership adjustment-members)))
          members)
         non-members)))))

(comment
  (let [words (->> ["distort" "kiss" "sport"]
                   (map #(frp/phrase->word frp/words %))
                   (map #(assoc % :weight 1)))
        target (->> "report"
                    (frp/phrase->word frp/words)
                    (#(assoc % :remaining-syllables (:syllables %))))
        rhyme-adjuster (adjust-for-rhymes target)
        syllable-count-adjuster (adjust-for-over-syllables target)]
    (syllable-count-adjuster (rhyme-adjuster words))))

(defn adjust-for-membership [set_]
  (fn [words]
    (map
     (fn [word]
       (if (set_ (:norm-word word))
         (as-> word word
           (assoc word :weight (* 2 (:weight word)))
           (assoc word :adjust-for-membership-factor 2))
         (assoc word :adjust-for-membership-factor 1)))
     words)))

(defn filter-for-membership [set_]
  (fn [words]
    (map
     (fn [word]
       (if-not (set_ (:norm-word word))
         (as-> word word
           (assoc word :weight (* 0.01 (:weight word)))
           (assoc word :filter-for-membership-factor 0.01))
         word))
     words)))

(defn adjust-for-markov [markov-options]
  (let [markov-set (into #{} (map first (keys markov-options)))]
    (fn [words]
      (let [result (map
                    (fn [word]
                      (if (markov-set (:norm-word word))
                        (as-> word word
                          (assoc word :weight (* 100 (:weight word)))
                          (assoc word :adjust-for-markov-factor 100))
                        (assoc word :adjust-for-markov-factor 1)))
                    words)]
        result))))

(comment
  (let [markov-adjuster (adjust-for-markov (lovecraft-markov '("help")))]
    (take 5 (markov-adjuster frp/words))))
(defn adjust-for-membership-1
  [set_ percent]
  (let [ratio (- 1 percent)]
    (fn [words]
      (let [[members non-members]
            ((juxt filter remove)
             #(set_ (:norm-word %))
             words)
            weight-non-members (apply + (map :weight non-members))
            target-weight-members (* ratio weight-non-members)
            count-members (count members)
            adjustment-members (/ target-weight-members count-members)]
        (concat
         (map
          (fn [member]
            (as-> member member
              (assoc member :weight (* adjustment-members (:weight member)))
              (assoc member :adjustment-for-membership adjustment-members)))
          members)
         non-members)))))

(defn adjust-for-markov-1
  [markov-options percent]
  (let [ratio (- 1 percent)]
    (fn [words]
      (if (nil? markov-options)
        words
        (let [[markovs non-markovs]
              ((juxt filter remove)
               #(markov-options (:norm-word %))
               words)
              weight-non-markovs (apply + (map :weight non-markovs))
              target-weight-markovs (* ratio weight-non-markovs)
              count-markovs (count markovs)
              adjustment-markovs (if (= 0 count-markovs) 1 (/ target-weight-markovs count-markovs))]
          (concat
           (map
            (fn [markov]
              (as-> markov markov
                (assoc markov :weight (* adjustment-markovs (:weight markov)))
                (assoc markov :adjustment-for-markov adjustment-markovs)))
            markovs)
           non-markovs))))))

(defn e-prhyme
  "2020-10-21 iteration"
  [words markov target stop?]
  (let [target (assoc target :original-syllables (:syllables target))
        words (map #(assoc % :weight 1) words)]
    (loop [target target
           result '()
           sentinel 0]
      (if (or (stop? target result)
              (> sentinel 5))
        result
        (let [markov-options (markov (list (:norm-word (first result))))
              markov-adjuster (adjust-for-markov-1 markov-options 0.9)
              syllable-count-adjuster (adjust-for-over-syllables target)
              rhyme-adjuster (adjust-for-rhymes-1 target 0.9)
              lovecraft-set (into #{} (map (comp first first) lovecraft-markov))
              lovecraft-filter (adjust-for-membership-1 lovecraft-set 0.9)
              adjust (comp rhyme-adjuster
                           syllable-count-adjuster
                           markov-adjuster
                           lovecraft-filter)
              weighted-words (p :adjust
                                (->> (adjust words)
                                     (remove #(= 0 (:weight %)))))
              rng (p :from-weights (wr/from-weights (map :weight weighted-words)))
              index (p :nextr (wr/nextr rng nil))
              selection (nth weighted-words index)
              new-target (->> target
                              (#(assoc % :syllables (drop-last
                                                     (:syllable-count
                                                      selection)
                                                     (:syllables
                                                      target))))
                              (#(assoc % :rimes (prhyme/rimes (:syllables %))))
                              (#(assoc % :onsets (prhyme/onset+nucleus (:syllables %))))
                              (#(assoc % :nuclei (prhyme/nucleus (:syllables %)))))
              result (cons selection result)]
          (recur new-target result (inc sentinel)))))))

(def words (map #(assoc % :weight 1) frp/words))

(defn main [poem-lines]
  (map
   (fn [line]
     (let [orig-target (frp/phrase->word frp/words line)]
       (e-prhyme
        frp/popular
        lovecraft-markov
        orig-target
        (fn [target result]
          (<= (count (:syllables orig-target))
              (apply + (map :syllable-count result)))))))
   poem-lines))

(defn rhymer [words markov target stop]
  (cons (e-prhyme
         words
         markov
         target
         stop)
        (lazy-seq (rhymer words markov target stop))))

(defn stop [target]
  (fn [inner-target result]
    (<= (count (:syllables target))
        (apply + (map :syllable-count result)))))

(defn sentence-stop [target]
  (fn [inner-target result]
    (let [result-sentence (string/join " " (map :norm-word result))]
      (when-not (empty? result)
        (or (nlp/valid-sentence? result-sentence)
            (< (:syllable-count target)
               (apply + (map :syllable-count result)))
            (< 5 (count result)))))))

(comment
  (let [phrase (frp/phrase->word frp/words "i solemnly swear i am up to no good")
        r (rhymer
           frp/popular
           lovecraft-markov
           phrase
           (sentence-stop phrase))]
    (take 5 (map #(string/join " " (map :norm-word %))
                 (filter #(nlp/valid-sentence? (string/join " " (map :norm-word %))) r))))

  (let [poem-lines ["mister sandman"
                    "give me a dream"
                    "make him the cutest"
                    "that i've ever seen"
                    "give him two lips"
                    "like roses in clover"
                    "please tell me that"
                    "these lonesome nights are over"]]
    (map (fn [line] (map :norm-word line)) (main poem-lines)))

  (let [orig-target (frp/phrase->word frp/words "mister sandman give me a dream")]
    (repeatedly
     10
     (fn []
       (e-prhyme
        frp/popular
        lovecraft-markov
        orig-target
        (fn [target result]
          (<= (count (:syllables orig-target))
              (apply + (map :syllable-count result)))))))))

(comment
  (frp/phrase->word frp/words "distort bad man")
  (repeatedly 10 #(make-rhymes lovecraft-markov "bad man"))

  (rhyming-words "magic beam")
  ((make-markov-picker lovecraft-markov) '("no"))
  (markov-gen lovecraft-markov '("world"))
  (interleave
   (->> "your eyes"
        (make-rhymes lovecraft-markov)
        (map
         (fn [[k v]]
           (markov-gen lovecraft-markov (list k)))))
   (->> "pretty"
        (make-rhymes lovecraft-markov)
        (map
         (fn [[k v]]
           (markov-gen lovecraft-markov (list k))))
        (remove nil?)))

  (frp/phrase->word frp/words "well-off")
  (frp/prhyme frp/words (assoc (words-map "well") :rimes? true))
  )
(defn ghost
  "Rhyme a phrase with markov"
  [words word]
  (let [rhymes (frp/prhyme words word)
        norm-rhyme-words (->> rhymes
                              (map :word)
                              (map string/lower-case)
                              (map #(string/replace % #"\(\d+\)" ""))
                              (into #{})
                              (filter #(get lovecraft-markov (list %))))
        keyer (markov-key #(list (first (string/split % #"\s"))))]
    (->> norm-rhyme-words
         (map (fn [w]
                (gen-from markover #(< (count %) 5) w))))))

(comment
  (take 10 lovecraft-markov)
  (ghost frp/words (assoc (frp/make-word ["dream" "D" "R" "IY" "M"])
                          :rimes?
                          true)))

(comment
  (->> (frp/make-word ["dream" "D" "R" "IY" "M"])
       (#(assoc % :rimes? true))
       (frp/prhyme frp/words)
       (take 10))


  (->> (main)
       (#(spit "lovecraft.edn" (pr-str %))))

  (let [t (read-string (slurp "lovecraft.edn"))]
    (take 20 t))
  )

(comment
  (->> (tokens-from-file "lovecraft.txt")
       (reverse)
       (normalize-tokens)
       ((window 2))
       (markov)
       (take 10)
       (into {})
       (#(get % '("away")))
       (choose-from-markov-possibilities))
 
  (markov [["boy" "good"] ["the" "over"]
           ["ran" "he"] ["walked" "he"]
           ["walked" "he"] ["walked" "she"]])
  (tokens-from-file "lovecraft.txt")
  (scrape)
  (def test-links (take 3 (links)))
  (->> (text-from-link (first test-links))
       (cleanup))
  (->> (text-from-link (first test-links))
       (append-to-file "test.txt" "hi"))
  (take 3 (html/select (fetch-url (first test-links)) [:body]))
  )
