(ns com.owoga.prhyme.data.tpt
  (:require [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.trie :as trie]
            [com.owoga.prhyme.util :as util]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)
           (java.lang.reflect Array)))

(comment
  (util/get-phones-with-stress "It's phil's coffee")

  )
