;; Fast weighted random selection thanks to the Vose algorithm.
;; https://gist.github.com/ghadishayban/a26cc402958ef3c7ce61

(ns com.owoga.prhyme.util.math
  (:import clojure.lang.PersistentQueue))


;; Vose's alias method
;; http://www.keithschwarz.com/darts-dice-coins/
(defprotocol Rand
  (nextr [_ rng]))

(deftype Vose [n ^ints alias ^doubles prob]
  Rand
  ;; returns the index of the chosen weight
  (nextr [_ rng] ;; not using the rng for now
    (let [i (rand-int n)
          p (aget prob i)]
      (if (or (= p 1.0)
              (< (rand) p))
        i
        (aget alias i)))))

(defn ^:private make-vose [dist]
  (let [N (count dist)
        alias (int-array N)
        prob  (double-array N)]
    (if (zero? N)
      (->Vose N alias prob)
      (let [^doubles ps (->> dist
                             (map (partial * N))
                             (into-array Double/TYPE))

            [small large] (loop [i 0
                                 [small large] [PersistentQueue/EMPTY
                                                PersistentQueue/EMPTY]
                                 ps (seq ps)]
                            (if (seq ps)
                              (let [p (first ps)]
                                (if (< p 1)
                                  (recur (inc i)
                                         [(conj small i) large]
                                         (rest ps))
                                  (recur (inc i)
                                         [small          (conj large i)]
                                         (rest ps))))
                              [small large]))

            [small large] (loop [small small
                                 large large]
                            (if (and (seq small) (seq large))
                              (let [l (first small)
                                    g (first large)
                                    small (pop small)
                                    large (pop large)]
                                (aset-double prob l (aget ps l))
                                (aset-int alias l g)
                                (let [pg (- (+ (aget ps g) (aget ps l))
                                            1.0)]
                                  (aset-double ps g pg)
                                  (if (< pg 1)
                                    (recur (conj small g) large)
                                    (recur small (conj large g)))))
                              [small large]))]
        (doseq [g (concat large small)]
          (aset-double prob g 1))
        (->Vose N alias prob)))))

(defn from-weights [ws]
  (let [N (count ws)
        tot (reduce + 0.0 ws)
        dist (if (zero? tot)
                  (repeat N (/ 1 tot))
                  (map #(/ % tot) ws))]
    (make-vose (vec dist))))

(comment
  (let [ws [1 2 1 3 3]
        rng (from-weights ws)
        chosen (repeatedly 1000000 #(nextr rng nil))
        accuracy (mapv (comp float
                             #(/ % 100000)
                             (frequencies chosen))
                       (range (count ws)))]
    accuracy))

(defn weighted-selection
  "If given a coll, assumes the coll is weights and returns the selected index by
  weighted random selection.

  If given a key function and a collection, uses the key function to get a
  collection of weights and returns the value at the randomly selected index."
  ([coll]
   (let [rng (from-weights coll)
         index (nextr rng nil)]
     index))
  ([key-fn coll]
   (let [rng (from-weights (map key-fn coll))
         index (nextr rng nil)
         selection (nth coll index)]
     selection)))

;;;; Simple Good-Turing Frequency Estimation
;;
;; Good-Turing Smoothing
;;
;; There are 4 steps to perform the GT smoothing, which are:
;; 1. Count the frequency of frequency (Nr)
;; 2. Average all the non-zero counts using Zr = Nr / 0.5 (t - q)
;; 3. Fit a linear regression model log(Zr) = a + b log(r)
;; 4. Update r with r* using Katz equation and constant k, with
;; updated Zr corresponding to specific r read out from the linear
;; regression model.

(defn least-squares-linear-regression
  "Returns a 'Good-Turing Estimator' as defined on page 4 of
  https://www.csie.ntu.edu.tw/~b92b02053/print/good-turing-smoothing-without.pdf

  A precise statement of the theorem underlying the Good-Turing method is that
  r* = (r + 1) * E(Nᵣ + 1) / E(Nᵣ)
  Where E(x) represents the expectation of random variable x.

  It's not unreasonable for E to be identity, simply substituting
  Nᵣ for E(Nᵣ). In fact, that is known as the Turing Estimator.

  However, the larger r is, the less reasonable this substitution is, due
  to how much noise there is in large values of r.

  So, this function acts as a more reasonable E.

  Note! Your raw data is probably exponential. This is linear regression
  and doesn't handle linearization for you. `log` your data befor sending it here.

  The paper also states that you should use the Turing estimator so long
  as the Turing estimate is significantly different from the Linear Good-Turing
  estimate. It defines significantly different as exceeding 1.65 times the
  standard deviation of the Turing estimate."
  [xs ys]
  (let [n (count xs)
        sum-x (apply + xs)
        sum-y (apply + ys)
        mean-x (/ sum-x n)
        mean-y (/ sum-y n)
        err-x (map #(- % mean-x) xs)
        err-y (map #(- % mean-y) ys)
        err-x-sqr (map #(* % %) err-x)
        m (/ (apply + (map #(apply * %) (map vector err-x err-y)))
             (apply + err-x-sqr))
        b (/ (- sum-y (* m sum-x)) n)]
    (assert (< m -1) "See Good-Turing Without Tears for why slope must be less than -1.")
    (fn [x]
      (+ b (* m x)))))

(defn average-consecutives
  "Average all the non-zero frequency of observations (frequency of frequencies)
  using the equation Zr = Nr / 0.5 (t - q)
  where q, r, and t are consecutive observations.

  An intuitive reason for this is that you can't see something a fraction of a time,
  but seeing something a fraction of a time is a truer representation of its
  expectation.

  For example, in a typical linguistic corpus, you'll see many tokens once, many twice,
  fewer 3 times, fewer 4 times, etc... By the time you get up to tokens that have been
  seen 20 times, you might only see 1 token. Then 0 occurrences of
  21, 22, and 23 tokens. Then you might once see a token 24 times.
  Then 0 occurrences of 25, 26, or 27 tokens, then 1 occurence of 28 tokens.

  Even though frequencies of 24 and 28 have both occurred once, that doesn't mean
  their expected occurence is each once. In actuality, 28 is less likely than 24.

  This averaging accomplishes that.

  It's known as Zᵣ in most papers on Good-Turing estimation. It's used in place of
  Nᵣ as soon as possible, since it's more accurate.

  Let's say you observered
  observation		frequency	Zr
  1				32			32
  2				20			20
  3				9			6
  5				2			1
  7				1			0.5

  If observations occur consecutively, then Zr is unchanged from the observed frequency.
  But if there is a gap in observation occurence, then this effectively
  redistributes some of the observations that we did make into some of the observations that
  we didn't make.

  For example, we saw some 3's and 5's, but no 4's. So this algorithm shaves a little off
  of the 3's and 5's so that if we fit a line to the frequencies the line will more accurately
  go through the `4` observations that we just so happened to miss.

  This assumes some type of distribution amongst the data where that assumption is valid,
  that the observations are independent of each other and that are either linear or logarithmic
  (not polynomial... maybe another word for that is monotonic?)
  "
  [freqs Nrs]
  (let [freqs (vec freqs)
        Nrs (vec Nrs)]
    (loop [i 0
           result []]
      (let [q (if (= i 0) 0 (nth freqs (dec i)))
            Nr (nth Nrs i)
            r (nth freqs i)
            t (if (= (inc i) (count freqs))
                (- (* 2 r) q)
                (nth freqs (inc i)))]
        (println q Nr r t)
        (cond
          (= (inc i) (count freqs))
          (conj result (/ (* 2 Nr) (- t q)))

          :else
          (recur
           (inc i)
           (conj result (/ (* 2 Nr) (- t q)))))))))

(comment
  (let [xs [1 2 3 4 5 6 7 8 9 10 12 26]
        ys [32 20 10 3 1 2 1 1 1 2 1 1]
        ys-avg-cons (average-consecutives xs ys)
        log-xs (map #(Math/log %) xs)
        log-ys (map #(Math/log %) ys-avg-cons)
        lm (least-squares-linear-regression log-xs log-ys)
        zs (map lm log-xs)]
    ;; => [32 20 10 3 1 2 1 1 1 2 1/2 1/14]
    [log-ys log-xs zs (map #(Math/pow Math/E %) zs)])

  )

(defn stdv-for-turing-estimate
  "The Simple Good-Turing paper suggests using a Turing estimator
  for small values of r and switching to a Linear Good Turing estimator
  once the differences between the two are no longer significantly different.

  Turing estimate are considered significantly different from LGT estimates
  if their difference exceeds 1.65 times the standard deviation of
  the Turing estimate.

  The variance for the Turing estimate is approximately
  (r + 1)² * Nᵣ₊₁ / N²ᵣ * (1 + Nᵣ₊₁ / N²ᵣ)"
  [r1 nr nr1]
  (Math/sqrt
   (* (Math/pow r1 2)
      (/ nr1 (Math/pow nr 2))
      (inc (/ nr1 nr)))))

