(ns com.owoga.frp.infrastructure)
(defprotocol PRelations
  (load! [this relations])
  (insert! [this relation])
  (delete! [this relation])
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

(deftype RelVar [relvar xf]
  PRelVar
  (project
   [this attributes]
   (project- this (map #(select-keys % attributes))))

  clojure.lang.IDeref
  (deref [_] (into #{} xf @relvar)))

(deftype BaseRelVar [relvar-name spec store]
  PRelVar
  (project
   [this attributes]
   (project- this (map #(select-keys % attributes))))

  PRelations
  (load! [this relations] (reset! store relations))

  clojure.lang.IDeref
  (deref [_] @store))

(defn project- [relvar xf]
  (->RelVar relvar xf))

(defmacro defrelvar
  [relvar-name & specs])
