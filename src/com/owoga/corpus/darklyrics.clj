;; TODO: Filter out non-English lyrics.

(ns com.owoga.corpus.darklyrics
  (:require [net.cgrand.enlive-html :as html]
            [com.owoga.prhyme.util :as util]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def root-url "http://www.darklyrics.com")
(def base-url "http://www.darklyrics.com/a.html")
(def data-dir "dark-corpus")

(defn fix-url [url]
  (string/replace url #".*(http://.*(?!http://).*$)" "$1"))

(defn fetch-url- [url]
  (let [url (fix-url url)]
    (try
      (html/html-resource (java.net.URL. url))
      (catch Exception e
        (prn "Exception during fetch " e)
        {}))))

(def fetch-url (memoize fetch-url-))

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

(defn parse-album-songs [album-html]
  (->> album-html
       (#(html/select % [:div.lyrics]))
       first
       :content
       (util/take-between #(= :h3 (:tag %)))
       (map
        (fn [[title & lyrics]]
          [(html/text (first (html/select title [:a])))
           (->> lyrics
                (filter string?)
                (map string/trim)
                (string/join "\n")
                (string/trim))]))))

(defn english? [text]
  (let [words (string/split text #"\s+")
        english-words
        (->> words (filter #(util/words-map (string/lower-case %))))]
    (< 0.7 (/ (count english-words) (count words)))))

(defn scrape
  ([base-url]
   (scrape (drop 3 (parse-letters-urls (fetch-url base-url))) '() '()))
  ([letters-urls artists-urls [artist-name albums-urls]]
   (cond
     (not-empty albums-urls)
     (try
       (let [album-html (fetch-url (first albums-urls))
             album-name (->> (html/select album-html [:div.albumlyrics :h2])
                             (map html/text)
                             first
                             (#(string/replace % #"(album: |\s+)" " "))
                             (string/trim))]
         (cons [artist-name album-name (parse-album-songs album-html)]
               (lazy-seq (scrape letters-urls artists-urls [artist-name (rest albums-urls)]))))
       (catch Exception e
         (prn "album exception" e)
         (scrape letters-urls artists-urls [artist-name (rest albums-urls)])))

     (not-empty artists-urls)
     (try
       (let [artist-html (fetch-url (first artists-urls))
             artist-name (->> (html/select artist-html [:h1])
                              (map html/text)
                              first
                              (#(string/replace % #" LYRICS" "")))]
         (scrape
          letters-urls
          (rest artists-urls)
          [artist-name (parse-artists-albums artist-html)]))
       (catch Exception e
         (prn "artist exception" e)
         (scrape
          letters-urls
          (rest artists-urls)
          ["unknown" '()])))

     (not-empty letters-urls)
     (scrape
      (rest letters-urls)
      (parse-artists-urls (fetch-url (first letters-urls)))
      [nil nil])

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

(def darkov-2 (util/read-markov "dark-corpus-2.edn"))


(defn norm-filepath [text]
  (-> text
      string/lower-case
      (string/replace #"\s+" "-")
      (string/replace #"[\(\)\"',\.]" "")))

(defn write-scrape [[artist album songs]]
  (run!
   (fn [[song lyrics]]
     (let [file (io/file (string/join "/" (map norm-filepath [data-dir artist album song])))]
       (io/make-parents file)
       (spit file lyrics)))
   songs))

(defn -main []
  (let [artist-album-texts (scrape base-url)]
    (run!
     (fn [x]
       (try
         (println (str "Writing songs for " (second x)))
         (write-scrape x)
         (catch Exception e
           (prn "Exception: " e))))
     artist-album-texts)))

(comment

  (def darkov-2 (util/read-markov "dark-corpus-2.edn"))
  (get darkov-2 '(nil nil))
  (take 3 (scrape base-url))
  (-main)
  (def letters-urls (parse-letters-urls (fetch-url base-url)))
  (def artists-urls (parse-artists-urls (fetch-url (first letters-urls))))
  (def artist-html (fetch-url (first artists-urls)))
  (def album-urls (parse-artists-albums artist-html))
  (def album-html (fetch-url (first album-urls)))
  (->> album-html
       (#(html/select % [:div.lyrics]))
       first
       :content
       (util/take-between #(= :h3 (:tag %)))
       (map
        (fn [[title & lyrics]]
          [(html/text (first (html/select title [:a])))
           (->> lyrics
                (filter string?)
                (map string/trim)
                (string/join "\n")
                (string/trim))])))

  (->> (html/select artist-html [:h1])
       (map html/text)
       (first ))
  (def darkov
    (into
     {}
     (map (fn [[k v]] (vector (list k) v))
          (make-markov (slurp "darklyrics.txt") 1))))
  (run! write-scrape (take 4 (scrape base-url)))

  (take 100 darkov)
  (util/write-markov "darklyrics.edn" darkov)
  (spit "test.txt" (pr-str {:foo "1"}))
  (def lyrics (scrape base-url))
  (with-open [writer (io/writer "darklyrics.txt")]
    (run!
     #(.write writer %)
     (take 200 (filter english? lyrics)))))
