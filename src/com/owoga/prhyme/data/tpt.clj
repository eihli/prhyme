(ns com.owoga.prhyme.data.tpt
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)
           (java.lang.reflect Array)))

(defn ubyte [b]
  (bit-and 0xff b))

(defn sbyte [b]
  (bit-or -0x100 b))

;; I've got a dictionary of ~120,000 words.
;; I need to reference them in a compact way.
;; It will be a little bit of a waste, but we can use a 32 bit index.

(/ (Math/log 1.2e5) (Math/log 2))

(defn as-binary-string [b]
  (string/replace
   (format "%8s" (Integer/toBinaryString b))
   #" "
   "0"))

(defn vb-encode [n]
  (loop [n n
         b '()]
    (cond
      (< n 128) (let [ba (byte-array (cons n b))
                      i (dec (count ba))]
                  (aset-byte ba i (sbyte (+ 128 (aget ba i))))
                  ba)
      :else (recur (quot n 128) (cons (mod n 128) b)))))

(defn vb-decode-1 [ba]
  (loop [n 0
         i 0]
    (let [b (aget ba i)]
      (cond
        (> (bit-and b 0x80) 0)
        [(bit-or (bit-and 0x7f b)
                 (bit-shift-left n 7))
         (inc i)]
        :else
        (recur (bit-or (bit-and 0x7f b)
                       (bit-shift-left n 7))
               (inc i))))))

(def dictionary ["hi" "my" "name" "is" "what"])

(defn slurp-bytes [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (io/copy (io/input-stream x) out)
    (.toByteArray out)))

(def markov
  {"hello" {:freq 5
            :children {"world" {:freq 2}
                       "eric" {:freq 1}}}
   "goodbye" {:freq 2
              :children {"cruel" {:freq 1
                                  :children {"world" {:freq 1}}}}}
   "world" {:freq 3}
   "eric" {:freq 1}
   "cruel" {:freq 1
            :children {"world" {:freq 1}}}})

(def markov
  {"world"
   {:freq 3
    :children {"hello" {:freq 2}
               "goodbye" {:freq 2}
               "cruel" {:freq 1
                        :children {"goodbye" {:freq 1}}}}}
   "hello"
   {:freq 2}
   "goodbye"
   {:freq 3}
   "cruel"
   {:freq 1
    :children {"goodbye" {:freq 1}}}})

(comment
  (with-open [o (io/output-stream "test.bin")]
    (.write o (byte-array 8) 0 8))

  (let [ba (slurp-bytes "test.bin")
        len (count ba)]
    )

  (with-open [o (io/output-stream "test.bin")]
    (.write o (count dictionary))
    (run!
     (fn [[i w]]
       (let [b (vb-encode i)]
         (.write o b 0 (count b)))
       (let [b (.getBytes w)]
         (.write o b 0 (count b))))
     (map vector (range) dictionary))
    )

  )
