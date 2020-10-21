(ns com.owoga.frp.infrastructure
  (:require [clojure.set :as set])
  (:refer-clojure :exclude [extend]))

(defprotocol PRelVar
  (extend [this extensions & constraints])
  (restrict [this criteria & constraints])
  (project [this attributes & constraints])
  (product [this relvar & constraints])
  (union [this relvar & constraints])
  (intersection [this relvar & contstraints])
  (difference [this relvar & constraints])
  (join [this relvar & constraints])
  (divide [this relvar & constraints])
  (rename [this renames & constraints]))

(defprotocol PRelations
  (load! [this relations])
  (insert!
    [this relation]
    [this & relations])
  (delete! [this & relations])
  (update! [this old-relation new-relation])
  (clear! [this]))
(declare extend-)
(declare project-)
(declare restrict-)

(deftype RelVar [relvar xf constraints]
  PRelVar
  (extend
   [this extensions & constraints]
   (extend- this extensions constraints))
  (project
   [this attributes & constraints]
   (project- this attributes constraints))
  (restrict
   [this criteria & constraints]
   (restrict- this criteria constraints))

  clojure.lang.IDeref
  (deref [_] (into #{} xf @relvar)))

(deftype BaseRelVar [relvar-name store constraints]
  PRelVar
  (extend
   [this extensions & constraints]
   (extend- this extensions constraints))
  (project
   [this attributes & constraints]
   (project- this attributes constraints))
  (restrict
   [this criteria & constraints]
   (restrict- this criteria constraints))

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

(defn extend- [relvar extensions constraints]
  (let [xf (map (fn [element]
                  (map (fn [[k f]]
                         (assoc element k (f element)))
                       extensions)))]
    (->RelVar relvar xf constraints)))

(defn project- [relvar attributes constraints]
  (->RelVar relvar (map #(select-keys % attributes)) constraints))

(defn restrict- [relvar criteria constraints]
  (->RelVar relvar (filter criteria) constraints))

(def *constraints* (atom {}))

(defmacro defrelvar
  [relvar-name & constraints]
  (swap! *constraints* assoc-in [relvar-name :constraints] constraints)
  `(->BaseRelVar '~relvar-name (atom #{}) [~@constraints]))
