(ns com.owoga.frp.infrastructure
  (:require [clojure.set :as set]))
(defprotocol PRelations
  (load! [this relations])
  (insert!
    [this relation]
    [this & relations])
  (delete! [this & relations])
  (update! [this old-relation new-relation])
  (clear! [this]))

(defprotocol PRelVar
  (restrict [this criteria])
  (project [this attributes])
  (product [this relvar])
  (union [this relvar])
  (intersection [this relvar])
  (difference [this relvar])
  (join [this relvar])
  (divide [this relvar])
  (rename [this renames]))
(declare project-)
(declare restrict-)

(deftype RelVar [relvar xf]
  PRelVar
  (project
   [this attributes]
   (project- this (map #(select-keys % attributes))))
  (restrict
   [this criteria]
   (restrict- this (filter criteria)))

  clojure.lang.IDeref
  (deref [_] (into #{} xf @relvar)))

(deftype BaseRelVar [relvar-name store constraints]
  PRelVar
  (project
   [this attributes]
   (project- this (map #(select-keys % attributes))))
  (restrict
   [this criteria]
   (restrict- this (filter criteria)))

  PRelations
  (load! [this relations] (reset! store relations))
  (insert!
   [this relation]
   (run!
    (fn [constraint]
      (when (constraint @this)
        (throw (ex-info "Constraint Exception" {}))))
    constraints)
   (swap! store conj relation))
  (insert!
   [this & relations]
   (run!
    (fn [constraint]
      (when (constraint @this)
        (throw (ex-info "Constraint Exception" {}))))
    constraints)
   (swap! store set/union (into #{} relations)))

  clojure.lang.IDeref
  (deref [_] @store))

(defn project- [relvar xf]
  (->RelVar relvar xf))

(defn restrict- [relvar xf]
  (->RelVar relvar xf))

(def *constraints* (atom {}))

(defmacro defrelvar
  [relvar-name & constraints]
  (swap! *constraints* assoc-in [relvar-name :constraints] constraints)
  `(->BaseRelVar '~relvar-name (atom #{}) [~@constraints]))
