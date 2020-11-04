(ns com.owoga.prhyme.data.darklyrics
  (:require [clojure.java.io :as io]
            [taoensso.nippy :as nippy])
  (:import [java.io DataInputStream ByteArrayOutputStream]))

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

;; (thaw-from-file (io/resource "test.bin"))
;; (bytes (byte-array (take 20 (range))))

;; (byte-array (map (comp byte int) "ascii"))
;; ;; => [97, 115, 99, 105, 105]
;; (bytes (byte-array (map (comp byte int) "ascii")))
;; ;; => [97, 115, 99, 105, 105]

;; (let [xin (io/input-stream (io/resource "test.bin"))
;;       xout (ByteArrayOutputStream.)]
;;   (io/copy xin xout)
;;   (nippy/thaw (.toByteArray xout)))

;; (.fullyRead (io/input-stream (io/resource "test.bin")))

;; (.getSize (io/input-stream (io/resource "dark-corpus-2.bin")))
;; (def data (into {} (map vec (partition 2 (range 20)))))
;; (nippy/freeze-to-file "resources/test.bin" data)
(def darklyrics-markov-2
  (thaw-from-file (io/resource "dark-corpus-2.bin")))
