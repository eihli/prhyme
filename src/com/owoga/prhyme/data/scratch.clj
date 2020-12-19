(ns com.owoga.prhyme.data.scratch
  (:require [clojure.java.io :as io]
            [cljol.dig9 :as d]
            [com.owoga.prhyme.data.dictionary :as dict])
  (:import (java.nio ByteBuffer)
           (java.io FileInputStream
                    FileOutputStream
                    DataOutputStream
                    ByteArrayOutputStream)))

(def symbols
  (-> (io/reader (io/resource "cmudict-0.7b.symbols"))
      line-seq))

(count dict/prhyme-dict)
(type dict/prhyme-dict)
(def foo (take 5 dict/prhyme-dict))

(count foo)

(def g (d/sum foo))

(def g (d/sum (into {} (take 13000 dict/prhyme-dict))))
(let [buf (ByteBuffer/allocate 1024)]
  (with-open [in (FileInputStream. "src/com/owoga/prhyme/data/random.txt")
              out (FileOutputStream. "src/com/owoga/prhyme/data/random.out")]
    (loop [len (.. in (getChannel) (read buf))]
      (cond
        (= len -1) (println "done")
        :else
        (do
          (println (format "read %d" len))
          (.clear buf)
          (recur (.. in (getChannel) (read buf))))))))

(defn ow-output-stream []
  (let [baos (ByteArrayOutputStream.)
        dos (DataOutputStream. baos)]
   dos))

(defn symbol->byte-array [s]
  (let [b (.getBytes s)
        r (cons (count b) b)]
    (byte-array r)))

(defn symbols->byte-array [s]
  (let [baos (ByteArrayOutputStream.)
        dos (DataOutputStream. baos)]
    (run!
     (fn [[i sym]]
       (let [b (.getBytes sym)]
         (.write dos (byte 0))
         (.write dos (byte (inc (count b))))
         (.write dos (byte i))
         (.writeBytes dos sym)))
     (map vector (range) s))
    (.flush dos)
    (.toByteArray baos)))

(def sym-array (symbols->byte-array symbols))

(defn sym-array-get [sym-array n]
  (loop [start 0
         end (count sym-array)]
    (let [mid (loop [mid (+ start (quot (- end start) 2))]
                (let [cur (aget sym-array mid)]
                  (cond
                    (= cur 0) (inc mid)
                    :else (recur (dec mid)))))
          id (aget sym-array (inc mid))]
      (cond
        (or (= start (dec (count sym-array)))
            (= end 1))
        -1
        (= id n) (let [l (dec (aget sym-array mid))
                       ba (byte-array l)]
                   (run!
                    #(aset ba % (aget sym-array (+ (+ 2 mid) %)))
                    (range l))
                   (apply str (map char ba)))
        (= mid start) (recur (+ start (aget sym-array mid)) end)
        (> id n) (recur start mid)
        (< id n) (recur mid end)))))


(sym-array-get sym-array 1)

(map identity (.getBytes (last symbols)))

(map identity sym-array)
(let [baos (ByteArrayOutputStream.)
      dos  (DataOutputStream. baos)]
  (.writeInt dos 0)
  (run!
   #(.write dos (symbol->byte-array %))
   symbols)
  (.flush dos)
  (with-open [out (io/output-stream "src/com/owoga/prhyme/data/scratch.out")]
    (let [ba (.toByteArray baos)]
      (.write out ba 0 (count ba)))))
