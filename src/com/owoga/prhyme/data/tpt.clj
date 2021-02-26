(ns com.owoga.prhyme.data.tpt
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)
           (java.lang.reflect Array)))

(defn ubyte [b]
  (bit-and 0xff b))

(defn sbyte [b]
  (bit-or -0x100 b))

(defn nth-bit [n b]
  (let [mask (bit-shift-left 1 n)]
    (bit-and
     1
     (bit-shift-right
      (bit-and mask b)
      n))))

(defn as-binary-string [b]
  (string/replace
   (format "%8s" (Integer/toBinaryString b))
   #" "
   "0"))

(defn bits [binary-string]
  (Integer/parseInt binary-string 2))

(defn bit-on [i b]
  (bit-or b (int (Math/pow 2 i))))

;; I've got a dictionary of ~120,000 words.
;; I need to reference them in a compact way.
;; It will be a little bit of a waste, but we can use a 32 bit index.

(/ (Math/log 1.2e5) (Math/log 2))



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

(defn vb-decode
  ([ba]
   (vb-decode ba 0))
  ([ba i]
   (if (>= i (count ba))
     (cons (first (vb-decode-1 ba))
           nil)
     (let [[value byte-count] (vb-decode-1 ba)]
       (lazy-seq
        (cons
         value
         (vb-decode (byte-array (drop byte-count ba))
                    (+ i byte-count))))))))

(comment
  (let [n1 0
        n2 1
        n3 127
        n4 128
        n5 257
        n6 9876543210
        baos (java.io.ByteArrayOutputStream.)]
    (->> [n1 n2 n3 n4 n5 n6]
         (map vb-encode)
         (run! #(.writeBytes baos %)))
    (let [ba (.toByteArray baos)]
      (vb-decode ba)))
  ;; => ([0 1] [1 1] [127 1] [128 2] [257 2] [9876543210 5])
  )

(defn byte-buffer-variable-length-decode
  [bb]
  (let [combine (fn [n b]
                  (bit-or (bit-and 0x7f b)
                          (bit-shift-left n 7)))]
    (loop [n 0 i 0]
      (let [b (.get bb)]
        (if (zero? (bit-and b 0x80))
          (recur
           (combine n b)
           (inc i))
          (combine n b))))))

(comment
  (let [bb (java.nio.ByteBuffer/wrap (vb-encode 9876543210))]
    (println (.limit bb))
    (byte-buffer-variable-length-decode bb))

  )

(defn encode-with-flag-bits
  "Flag is a binary string.
  Returns byte array."
  [flag n]
  (let [flag-len (count flag)
        data-len (- 8 flag-len)
        max-data-val (int (Math/pow 2 data-len))
        flag-val (bit-shift-left (bits flag) data-len)]
    (loop [n n r '()]
      (if (< n max-data-val)
        (byte-array (cons (bit-or flag-val n) r))
        (recur
         (quot n max-data-val)
         (cons
          (bit-or flag-val (mod n max-data-val))
          r))))))

(comment
  (let [ns [0 1 127 128 257 9876543210]
        encoded (map
                 (partial encode-with-flag-bits "101")
                 ns)
        as-binary (->> encoded
                       (map (partial map as-binary-string))
                       (map (partial map (partial take-last 8)))
                       (map (partial map (partial apply str))))]
    as-binary)
  ;; => (("10100000")
  ;;     ("10100001")
  ;;     ("10100011" "10111111")
  ;;     ("10100100" "10100000")
  ;;     ("10101000" "10100001")
  ;;     ("10101001" "10100110" "10101011" "10100000" "10100101" "10110111" "10101010"))
  )

(defn decode-with-flag-bits
  "Flag is only used for length. It could be refactored to take an int.
  Or it could be refactored to take a function that does work based on the flags.
  For now though, flag bits must be checked external to this. This is just parity
  decoding of the encoding above.
  Returns [decoded-number number-of-bytes-read]."
  [flag ba]
  (let [flag-len (count flag)
        data-len (- 8 flag-len)
        mask-val (int (dec (Math/pow 2 data-len)))]
    (loop [ba ba r 0 i 0]
      (if (empty? ba)
        [r i]
        (let [data-val (bit-and mask-val (first ba))]
          (recur
           (rest ba)
           (bit-or
            (bit-shift-left r data-len)
            data-val)
           (inc i)))))))

(comment
  (let [ns [0 1 127 128 257 9876543210]
        encoded (map
                 (partial encode-with-flag-bits "101")
                 ns)
        decoded (->> encoded
                     (map (partial decode-with-flag-bits "101")))]
    decoded)
  ;; => ([0 1] [1 1] [127 2] [128 2] [257 2] [9876543210 7])
  (let [ns [0 1 127 128 257 9876543210]
        encoded (map
                 (partial encode-with-flag-bits "001")
                 ns)
        decoded (->> encoded
                     (map (partial decode-with-flag-bits "101")))]
    decoded)
  ;; => ([0 1] [1 1] [127 2] [128 2] [257 2] [9876543210 7])
  )

(defn encode-byte-with-n-flag-bits [n b]
  (let [max-size-with-flag (int (Math/pow 2 (- 8 n)))]
    (loop [b b r '()]
      (if (< b max-size-with-flag)
        (byte-array (cons b r))
        (recur (quot b max-size-with-flag)
               (cons (mod b max-size-with-flag) r))))))

(defn decode-byte-with-n-flag-bits [n ba]
  (let [max-size-with-flag (int (Math/pow 2 (- 8 n)))]
    (loop [ba ba r 0]
      (if (nil? ba)
        r
        (recur (rest ba)
               (+ r ))))))

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
