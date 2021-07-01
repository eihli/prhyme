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

;; Silly test, turing estimation just returns Nr unchanged.
(t/deftest turing-estimation
  (t/testing "turing estimation"
    (let [r-coll  [1  2  3 5 10]
          nr-coll [20 10 5 1 2]
          turing-estimator (math/turing-estimator r-coll nr-coll)]
      (t/is (= nr-coll
               (map
                turing-estimator
                r-coll))))))

;; Hand check looks reasonable but precise assertion would come from careful hand calculation
;; or comparison to a known-good implementation.
#_(t/deftest lgt-estimation
  (t/testing "linear good-turing estimation"
    (let [r-coll  [1  2  3 5 10]
          nr-coll [20 10 5 1 2]
          zr-coll (math/average-consecutives r-coll nr-coll)
          _ (println zr-coll)
          linear-model (math/least-squares-linear-regression r-coll zr-coll)
          lgt-estimator (math/lgt-estimator linear-model)]
      (t/is (= [] (map
                   lgt-estimator
                   r-coll))))))

(comment
  (let [r-coll  [1  2  3 5 10]
        nr-coll [20 10 5 1 2]
        zr-coll (math/average-consecutives r-coll nr-coll)
        linear-model (math/least-squares-linear-regression r-coll zr-coll)
        lgt-estimator (math/lgt-estimator linear-model)]
    (map
     lgt-estimator
     r-coll))
  ;; => (23.33291663880418
  ;;     5.8271897728476425
  ;;     2.5882932698055106
  ;;     0.931074517265868
  ;;     0.23252763418987563)
  )

;; Hardcoded the expectation received by runing ~sgt/sgt.h~
;; // Simple Good-Turing estimation
;; //
;; // Copyright (c) David Elworthy 2004.
(t/deftest simple-good-turing-probabilities
  (t/testing "The simple good turing estimator switches between linear and turing"
    (let [freqs {7 1, 1 32, 4 3, 6 2, 3 10, 12 1, 2 20, 9 1, 5 1, 26 1, 10 2, 8 1}
          probs (math/frequencies->simple-good-turing-probabilities freqs)]
      (t/is (every?
             (fn [[a b]]
               (approx= a b 0.0001))
             (map
              vector
              (vals probs)
              [0.150325
               0.005617
               0.006013
               0.010137
               0.014408
               0.018754
               0.023142
               0.027557
               0.031989
               0.036434
               0.040887
               0.049813
               0.112550]))))))

