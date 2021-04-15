(ns com.owoga.prhyme.data.tpt
  (:require [com.owoga.tightly-packed-trie :as tpt]
            [com.owoga.trie :as trie]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)
           (java.lang.reflect Array)))
