(ns com.owoga.prhyme.util.math-test
  (:require [com.owoga.prhyme.util.math :as math]
            [clojure.test :as t]))

(defn approx=
  [a b e]
  (<= (Math/abs (- a b)) e))

;; Following the work in Church and Gale [1991], we averagewith each
;; non-zeroNrthe zeroNr’s that surround it: order the non-zeroNrbyr, and letq,r,
;; andtbesuccessive indices of non-zero values. We replaceNrbyZr=Nr/0. 5 (t−q).
;; In other words we estimatethe expectedNrby the density ofNrfor larger. For
;; smallr, there is no difference, because the length ofthe intervals is unity.
;; For larger, the change can make a difference of several orders of magnitude.
(t/deftest averaging-consecutives
  (t/testing "averaging consecutives"
    (let [r-coll  [1  2  3 5 10]
          nr-coll [20 10 5 1 2]
          zr-coll (math/average-consecutives r-coll nr-coll)]
      (t/is (approx= (nth zr-coll 1)
                     (/ 10 (* 0.5 (- 3 1)))
                     1e-4))
      (t/is (approx= (nth zr-coll 2)
                     (/ 5 (* 0.5 (- 5 2)))
                     1e-4))
      (t/is (approx= (nth zr-coll 3)
                     (/ 1 (* 0.5 (- 10 3)))
                     1e-4))
      (t/is (approx= (nth zr-coll 4)
                     (/ 2 (* 0.5 (- 15 5)))
                     1e-4)))))

;;;; Values from LibreOffice data -> statistics -> regression
;; 18.1311501368169
;; 7.85117996785167
;; 4.81179001426509
;; 2.59672962119497
;; 1.12444006199334

(t/deftest linear-regression
  (t/testing "The results of the linear regression model are accurate"
    (let [r-coll  [1  2  3 5 10]
          zr-coll [20 10 5 1 2] ;; not really smoothed, but smoothing isn't under test
          linear-model (math/least-squares-linear-regression r-coll zr-coll)
          linear-results (map linear-model r-coll)]
      (t/is (every?
             (fn [[expected predicted]]
               (approx= expected predicted 0.01))
             (map
              vector
              '(18.13
                7.85
                4.81
                2.59
                1.12)
              linear-results))))))

;; The below passes a sanity check in that each r* is slightly less than r.
#_(t/deftest turing-estimation
  (t/testing "turing estimation - r*"
    (let [r-coll  [1  2  3 5 10]
          nr-coll [20 10 5 1 2]
          zr-coll (math/average-consecutives r-coll nr-coll)
          log-r (map #(Math/log %) r-coll)
          log-zr (map #(Math/log %) zr-coll)
          linear-model (math/least-squares-linear-regression log-r log-zr)]
      (t/is (= [] (map
                   (partial math/turing-estimate linear-model)
                   r-coll))))))

(t/deftest simple-good-turing-estimator
  (t/testing "The simple good turing estimator switches between linear and turing"
    (let [r-coll  [1  2  3 5 10]
          zr-coll [20 10 5 1 2] ;; not smoothed, but smoothing isn't under test
          log-r (map #(Math/log %) r-coll)
          log-zr (map #(Math/log %) zr-coll)
          linear-model (math/least-squares-linear-regression log-r log-zr)
          sgt-estimator (math/estimator linear-model r-coll zr-coll)
          sgt-estimates (:r*
                         (reduce
                          (fn [{:keys [lgt? r*] :as acc} x]
                            (let [[y lgt?] (sgt-estimator x lgt?)]
                              {:lgt? lgt?
                               :r* (conj r* y)}))
                          {:lgt? false
                           :r* []}
                          r-coll))]
      (println zr-coll)
      (println (map linear-model r-coll))
      (println sgt-estimates)
      (t/is (every?
             (fn [[expected predicted]]
               (approx= expected predicted 0.01))
             (map
              vector
              '(18.13
                7.85
                4.81
                2.59
                1.12)
              sgt-estimates))))))
