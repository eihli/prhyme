;; TODO: Filter out non-English lyrics.

(ns com.owoga.corpus.darklyrics
  (:require [net.cgrand.enlive-html :as html]
            [com.owoga.prhyme.util :as util]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def root-url "http://www.darklyrics.com")
(def base-url "http://www.darklyrics.com/a.html")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn parse-letters-urls [index]
  (-> index
      (html/select [:div.listrow])
      (first)
      (html/select [:a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url %)))))

(defn parse-artists-urls [page]
  (-> page
      (html/select [:div.artists :a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url "/" %)))))

(defn parse-artists-albums [page]
  (-> page
      (html/select [:div.album])
      ((partial
        map
        (fn [album]
          (first
           (map
            #(str root-url (string/replace (get-in % [:attrs :href]) #"\.\." ""))
            (html/select album [:a]))))))))

(defn parse-album-lyrics [page]
  (-> page
      (html/select [:div.lyrics])
      first
      :content
      ((partial partition-by #(and (map? %) (= :h3 (:tag %)))))
      flatten
      ((partial filter string?))
      ((partial apply str))
      (string/replace #"\s+" " ")))

(defn english? [text]
  (let [words (string/split text #"\s+")
        english-words
        (->> words (filter #(util/words-map (string/lower-case %))))]
    (< 0.5 (/ (count english-words) (count words)))))

(defn scrape
  ([base-url]
   (scrape (parse-letters-urls (fetch-url base-url)) '() '()))
  ([letters-urls artists-urls albums-urls]
   (cond
     (not-empty albums-urls)
     (cons (parse-album-lyrics (fetch-url (first albums-urls)))
           (lazy-seq (scrape letters-urls artists-urls (rest albums-urls))))

     (not-empty artists-urls)
     (scrape letters-urls (rest artists-urls) (parse-artists-albums (fetch-url (first artists-urls))))

     (not-empty letters-urls)
     (scrape (rest letters-urls) (parse-artists-urls (fetch-url (first letters-urls))) albums-urls)

     :else
     nil)))

(defn clean-text [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]" "")))

(defn make-markov [text n]
  (let [tokens (reverse (string/split (clean-text text) #"\s+"))]
    (reduce
     (fn [a w]
       (let [k (butlast w)
             v (last w)]
         (update-in a [k v] (fnil inc 0))))
     {}
     ((util/window (inc n)) tokens))))

(def darkov-2
    (into
     {}
     (map (fn [[k v]] (vector (list k) v))
          (make-markov (slurp "darklyrics.txt") 2))))

(comment
  (def darkov
    (into
     {}
     (map (fn [[k v]] (vector (list k) v))
          (make-markov (slurp "darklyrics.txt")))))
  (take 100 darkov)
  (util/write-markov "darklyrics.edn" darkov)
  (spit "test.txt" (pr-str {:foo "1"}))
  (def lyrics (scrape base-url))
  (with-open [writer (io/writer "darklyrics.txt")]
    (run!
     #(.write writer %)
     (take 200 (filter english? lyrics)))))
