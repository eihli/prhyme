(ns com.owoga.corpus.darklyrics
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def root-url "http://www.darklyrics.com")
(def base-url "http://www.darklyrics.com/a.html")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn pages-urls [index]
  (-> index
      (html/select [:div.listrow])
      (first)
      (html/select [:a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url %)))))

(defn parse-letters-urls [index]
  (-> index
      (html/select [:div.listrow])
      (first)
      (html/select [:a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url %)))))

(defn artists-urls [page]
  (-> page
      (html/select [:div.artists :a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url "/" %)))))

(defn parse-artists-urls [page]
  (-> page
      (html/select [:div.artists :a])
      ((partial map #(get-in % [:attrs :href])))
      ((partial map #(apply str root-url "/" %)))))

(defn artists-names [page]
  (-> page
      (html/select [:div.artists :a])
      ((partial map #(get-in % [:content])))))

(defn artists-albums [page]
  (-> page
      (html/select [:div.album])
      ((partial
        map
        (fn [album]
          (cons
           (first (map html/text (html/select album [:h2 :strong])))
           (list
            (map
             #(str root-url (string/replace (get-in % [:attrs :href]) #"\.\." ""))
             (html/select album [:a])))))))))

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

(defn album-lyrics [page]
  (-> page
      (html/select [:div.lyrics])
      first
      :content
      ((partial partition-by #(and (map? %) (= :h3 (:tag %)))))
      flatten
      ((partial filter string?))
      ((partial apply str))
      (string/replace #"\s+" " ")))

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

(defn lazy-artists
  ([urls]
   (lazy-artists urls '()))
  ([urls artists]
   (cond
     (empty? urls)
     nil

     (empty? artists)
     (lazy-artists (rest urls)
                   (artists-urls (fetch-url (first urls))))

     :else
     (cons (fetch-url (first artists))
           (lazy-seq (lazy-artists urls (rest artists)))))))

(defn lazy-lyrics
  ([page]
   (let [album-urls (->> (artists-albums page)
                         (map #(vector (first %) (first (second %)))))]
     (lazy-lyrics page album-urls)))
  ([page albums]
   (cond
     (empty? albums) nil
     :else
     (cons (album-lyrics (fetch-url (second (first albums))))
           (lazy-seq (lazy-lyrics page (rest albums)))))))

(defn lazy-scrape
  ([base-url]
   (let [response (fetch-url base-url)
         alphabetical (pages-urls response)
         artists (lazy-artists alphabetical)]))
  ([response artists albums]
   (cond
     (empty? artists) nil
     )))

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

(comment
  (parse-letters-urls (fetch-url base-url))
  (def lyrics (scrape base-url))
  (with-open [writer (io/writer "darklyrics.txt")]
    (run!
     #(.write writer %)
     (take 20 lyrics)))
  (def response (fetch-url base-url))
  (def a (fetch-url (first (pages-urls response))))
  (artists-urls (fetch-url (second (pages-urls response))))
  (def la (lazy-artists (pages-urls response)))
  (first la)
  (def first-artists-page (first la))
  (def first-artists-album-url (first (second (first (artists-albums first-artists-page)))))
  (album-lyrics (fetch-url first-artists-album-url))
  (first (lazy-albums (first la)))
  (def artist-1 (first (artists-urls a)))
  (def artist-1-page (fetch-url artist-1))
  (-> artist-1-page
      (html/select [:div.album]))
  (def artists-albums-1 (artists-albums artist-1-page))
  (def artist-album (first (second (first artists-albums-1))))
  artist-album
  (def album (fetch-url artist-album))
  )
