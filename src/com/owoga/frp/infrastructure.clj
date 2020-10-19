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
  (restrict [this criteria & constraints])
  (project [this attributes])
  (project [this attributes & constraints])
  (product [this relvar])
  (product [this relvar & constraints])
  (union [this relvar])
  (union [this relvar & constraints])
  (intersection [this relvar])
  (intersection [this relvar & contstraints])
  (difference [this relvar])
  (difference [this relvar & constraints])
  (join [this relvar])
  (join [this relvar & constraints])
  (divide [this relvar])
  (divide [this relvar & constraints])
  (rename [this renames])
  (rename [this renames & constraints]))
(declare project-)
(declare restrict-)

(deftype RelVar [relvar xf constraints]
  PRelVar
  (project
   [this attributes constraints]
   (project- this (map #(select-keys % attributes)) constraints))
  (restrict
   [this criteria constraints]
   (restrict- this (filter criteria) constraints))

  clojure.lang.IDeref
  (deref [_] (into #{} xf @relvar)))

(deftype BaseRelVar [relvar-name store constraints]
  PRelVar
  (project
   [this attributes constraints]
   (project- this (map #(select-keys % attributes)) constraints))
  (restrict
   [this criteria constraints]
   (restrict- this (filter criteria) constraints))

  PRelations
  (load! [this relations] (reset! store relations))
  (insert!
   [this relation]
   (let [new-relation (conj @store relation)]
     (run!
      (fn [constraint]
        (when (not (every? true? (constraint new-relation)))
          (throw (ex-info "Constraint Exception" {}))))
      constraints)
     (reset! store new-relation)))
  (insert!
   [this & relations]
   (let [new-relation (set/union @store (into #{} relations))]
     (run!
      (fn [constraint]
        (when (not (every? true? (constraint new-relation)))
          (throw (ex-info "Constraint Exception" {}))))
      constraints)
     (reset! store new-relation)))

  clojure.lang.IDeref
  (deref [_] @store))

(defn project- [relvar xf constraints]
  (->RelVar relvar xf constraints))

(defn restrict- [relvar xf constraints]
  (->RelVar relvar xf constraints))

(def *constraints* (atom {}))

(defmacro defrelvar
  [relvar-name & constraints]
  (swap! *constraints* assoc-in [relvar-name :constraints] constraints)
  `(->BaseRelVar '~relvar-name (atom #{}) [~@constraints]))
