(ns com.owoga.prhyme.data.darklyrics
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.nippy :as nippy]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [com.owoga.prhyme.data.dictionary :as dict])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream
                    DataOutputStream DataInputStream)))

(defn thaw-from-file
  "Convenience util: like `thaw`, but reads from `(clojure.java.io/file <file>)`.

  To thaw from a resource on classpath (e.g in Leiningen `resources` dir):
    (thaw-from-file (clojure.java.io/resource \"my-resource-name.npy\"))

  See also `freeze-to-file`."
  ([file          ] (thaw-from-file file nil))
  ([file thaw-opts]
   (let [xin (io/input-stream file)
         xout (ByteArrayOutputStream.)]
     (io/copy xin xout)
     (nippy/thaw (.toByteArray xout) thaw-opts))))

(def darklyrics-markov-2
  (thaw-from-file (io/resource "dark-corpus-2.bin")))

(comment
  (def words (map #(vector (hash %) %)
                  (map :normalized-word dict/prhyme-dict)))
  (count words)
  (count dict/prhyme-dict)
  (count (into #{} (map first words)))
  (take 5 words)

  (def ds "jdbc:sqlite:resources/darklyrics.db")

  (def hashes
    (into
     {}
     (map
      (fn [[k v]]
        [(hash k) k])
      darklyrics-markov-2)))

  (nippy/freeze-to-file
   "resources/dark-corpus-hashes.nip"
   hashes)

  (run!
   (fn [c]
     (sql/insert-multi!
      ds
      :markov
      [:hash :words]
      c))
   (partition (int 1e5) hashes))

  (run!
   (fn [c]
     (sql/insert-multi!
      ds
      :dict
      [:hash :word]
      c))
   (partition (int 1e5) words))

  (println (+ 2 2))

  (keyword "won't")
  (get darklyrics-markov-2 '("hiding" "our"))
  (count darklyrics-markov-2)
  )
