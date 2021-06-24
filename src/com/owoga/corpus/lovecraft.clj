(ns com.owoga.corpus.lovecraft
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as string]
            [com.owoga.prhyme.util.weighted-rand :as wr]
            [com.owoga.prhyme.data.dictionary :as dict]
            [com.owoga.prhyme.util :as util]
            [com.owoga.prhyme.core :as prhyme]
            [com.owoga.prhyme.generation.weighted-selection :as weighted-selection]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [clojure.java.io :as io]))

(tufte/add-basic-println-handler! {})

(def ^:dynamic *base-url* "https://www.hplovecraft.com/writings/texts/")

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
