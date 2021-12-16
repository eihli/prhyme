(ns com.owoga.corpus.util
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as html]))

(set! *warn-on-reflection* true)
(tufte/add-basic-println-handler! {})

(defn fix-url
  "This is specific to some non-conformity on DarkLyrics.com.
  Some hrefs are relative and some are absolute."
  [url]
  (string/replace url #".*(http://.*(?!http://).*$)" "$1"))

(defn fetch-url-
  "Memoized for faster iterations in development."
  [url]
  (let [url (fix-url url)]
    (try
      (html/html-resource (java.net.URL. url))
      (catch Exception e
        (prn "Exception during fetch " e)
        {}))))

(def fetch-url (memoize fetch-url-))

(defn clean-text
  "Removes all non-alphabetical characters and lowercases everything.
  Very spartan way of cleaning."
  [text]
  (string/lower-case (string/replace text #"[^a-zA-Z'\-\s]+" "")))

(defn xf-file-seq [start end]
  (comp (remove #(.isDirectory %))
        (drop start)
        (take end)))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn pad-tokens
  "Pads the beginning with n - 1 <s> tokens and
  the end with 1 </s> token."
  [beginning-pad
   number-of-beginning-pad
   ending-pad
   number-of-ending-pad
   tokens]
  (vec
   (concat
    (vec (repeat number-of-beginning-pad beginning-pad))
    tokens
    (vec (repeat number-of-ending-pad ending-pad)))))

(defn padder
  [beg beg-n end end-n]
  (partial pad-tokens beg beg-n end end-n))

(comment
  (let [p (padder "<s>" 1 "</s>" 3)]
    (p [1 2 3]));; => ["<s>" 1 2 3 "</s>" "</s>" "</s>"]
  )

(defn tokenize-line
  [line]
  (->> line
       (string/trim)
       (re-seq re-word)
       (mapv second)
       (mapv string/lower-case)))

(comment
  (tokenize-line "The lazy fox jumps over the moon.")
  ;; => ["the" "lazy" "fox" "jumps" "over" "the" "moon" "."]
  )

(defn text->ngrams
  "Takes text from a file, including newlines."
  [text n]
  (->> text
       clean-text
       (#(string/split % #"\n+"))
       (remove empty?)
       (mapv tokenize-line)
       (mapv #(partition n 1 %))
       (mapv #(mapv vec %))
       (reduce #(into %1 %2) [])))

(comment
  (text->ngrams "The lazy fox jumps.\nOver the 5th full moon." 3)
  ;; => [["the" "lazy" "fox"]
  ;;     ["lazy" "fox" "jumps"]
  ;;     ["over" "the" "th"]
  ;;     ["the" "th" "full"]
  ;;     ["th" "full" "moon"]]

  )
