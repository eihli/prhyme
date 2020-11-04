(ns com.owoga.corpus.lovecraft
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [com.owoga.prhyme.util.weighted-rand :as wr]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.util.nlp :as nlp]
            [com.owoga.prhyme.gen :as gen]
            [com.owoga.prhyme.generation.weighted-selection :as weighted-selection]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [com.owoga.prhyme.frp :as frp]
            [clojure.java.io :as io]))

(tufte/add-basic-println-handler! {})

(def ^:dynamic *base-url* "https://www.hplovecraft.com/writings/texts/")

(def words-map
  (into {} (map #(vector (string/lower-case (:word %)) %) frp/words)))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

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

(defn markov [tokens]
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

(defn running-total
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

(defn weighted-rand [weights]
  (let [running-weights (running-total weights)
        rand-val (rand (last running-weights))]
    (loop [i 0]
      (if (> (nth running-weights i) rand-val)
        i
        (recur (inc i))))))

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

(defn synonym?
  "Given a possibility, like [\"foo\" 3]
  which says that foo follows a particular key with
  a weight of 3, a word is a synonym of that possibility
  if the word is a synonym ."
  [p synonyms]
  (synonyms p))

(defn adjust-for-synonyms
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

(defn adjust-for-rimes
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


(defonce lovecraft-markov (read-string (slurp "lovecraft.edn")))

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
                           (prhyme/phrase->word frp/words)
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

(defn adjust-for-membership-1
  [set_ percent]
  (let [ratio (- 1 percent)]
    (fn [words]
      (let [[members non-members]
            ((juxt filter remove)
             #(set_ (:normalized-word %))
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

(defn adjust-for-membership [set_]
  (fn [words]
    (map
     (fn [word]
       (if (set_ (:normalized-word word))
         (as-> word word
           (assoc word :weight (* 2 (:weight word)))
           (assoc word :adjust-for-membership-factor 2))
         (assoc word :adjust-for-membership-factor 1)))
     words)))

(defn filter-for-membership [set_]
  (fn [words]
    (map
     (fn [word]
       (if-not (set_ (:normalized-word word))
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
                      (if (markov-set (:normalized-word word))
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
             #(set_ (:normalized-word %))
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
               #(markov-options (:normalized-word %))
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
