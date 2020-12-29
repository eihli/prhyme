(ns com.owoga.prhyme.generation.simple-good-turing)

;; Pythons NLTK is a great resource for this.
;; https://github.com/nltk/nltk/blob/2.0.4/nltk/probability.py


(defn least-squares-log-log-linear-regression
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

  The paper also states that you should use the Turing estimator so long
  as the Turing estimate is significantly different from the Linear Good-Turing
  estimate. It defines significantly different as exceeding 1.65 times the
  standard deviation of the Turing estimate."
  [xs ys]
  (let [xs (map #(Math/log %) xs)
        ys (map #(Math/log %) ys)
        n (count xs)
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
    (assert (< m -1)
            (format
             (str "See Good-Turing Without Tears"
                  " for why slope must be less than -1."
                  "\nSlope: %.2f Intersect %.2f")
             (float m)
             (float b)))
    (fn [x]
      (Math/pow Math/E (+ b (* m (Math/log x)))))))

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
        (cond
          (= (inc i) (count freqs))
          (conj result (/ (* 2 Nr) (- t q)))

          :else
          (recur
           (inc i)
           (conj result (/ (* 2 Nr) (- t q)))))))))

(defn stdv-for-turing-estimate
  "The Simple Good-Turing paper suggests using a Turing estimator
  for small values of r and switching to a Linear Good Turing estimator
  once the differences between the two are no longer significantly different.

  Turing estimate are considered significantly different from LGT estimates
  if their difference exceeds 1.65 times the standard deviation of
  the Turing estimate.

  The variance for the Turing estimate is approximately
  (r + 1)² * Nᵣ₊₁ / N²ᵣ * (1 + Nᵣ₊₁ / N²ᵣ)"
  [r Nr Nr1]
  (Math/sqrt
   (* (Math/pow (inc r) 2)
      (/ Nr1 (Math/pow Nr 2))
      (inc (/ Nr1 (Math/pow Nr 2))))))

(defn r-stars
  "r* = (r + 1) * E(Nᵣ₊₁) / E(Nᵣ)
  Where E is an 'estimator'.

  The Turing estimator is simply the identity function, substituting Nᵧ for E(Nᵧ).

  The smoothed Linear Good-Turing estimator is a linear regression model
  over the log rs log nrs inputs.

  We choose the Turing estimator when it is significantly different from the
  smoothed estimator. Significantly different defined as having a standard
  deviation more than 1.65 times the difference between the Turing estimator
  and the Linear Good-Turing estimator."
  [rs nrs lm]
  (loop [rs rs
         nrs nrs
         lgt? false
         result []]
    (cond
      (empty? rs) result
      :else
      (if-let [lgt? lgt?]
        (recur
         (rest rs)
         (rest nrs)
         lgt?
         (conj
          result
          (* (inc (first rs))
             (/ (lm (inc (first rs)))
                (lm (first rs))))))
        (let [lgt-estimate (lm (first rs))
              turing-estimate (first nrs)
              stdv (stdv-for-turing-estimate
                    (first rs)
                    (first nrs)
                    (second nrs))
              lgt? (or (> (Math/abs (- lgt-estimate turing-estimate))
                          (* 1.65 stdv))
                       ;; Note possibility for the turing estimate to
                       ;; require an out-of-range Nr+1
                       ;; if we get to the end of nrs and still aren't
                       ;; taking the linear good-turing estimate.
                       (= 1 (count nrs)))]
          (recur
           (rest rs)
           (rest nrs)
           lgt?
           (conj
            result
            (if lgt?
              (* (inc (first rs))
                 (/ (lm (inc (first rs)))
                    (lm (first rs))))
              (* (inc (first rs))
                 (/ (second nrs)
                    (first nrs)))))))))))

(defn simple-good-turing
  [rs nrs]
  (assert (and (not-empty nrs) (not-empty rs))
          "frequencies and frequency-of-frequencies can't be empty")
  (let [N (apply + (map #(apply * %) (map vector rs nrs)))
        p0 (/ (first nrs) N)
        zrs (average-consecutives rs nrs)
        lm (least-squares-log-log-linear-regression rs zrs)
        lgts (map lm rs)
        r*s (r-stars rs zrs lm)
        N* (apply + (map #(apply * %) (map vector nrs r*s)))
        probs (cons
               (float p0)
               (map #(* (- 1 p0) (/ % N*)) r*s))
        sum-probs (apply + probs)]
    [(cons 0 rs) (map #(/ % sum-probs) probs)]))

(comment
  (let [rs  [ 1  2  3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]
        nrs [32 20 10 3 1 2 1 1 1  2  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  1]
        rs  [1 2 3 4 5 6 7 8 9 10 12 26]
        nrs [32 20 10 3 1 2 1 1 1 2 1 1]]
    (map #(apply * %) (map vector rs (simple-good-turing rs nrs)))
    (simple-good-turing rs nrs))

  (let [rs  [1 2 3 4 5 6 7 8 9 10 12 26]
        nrs [32 20 10 3 1 2 1 1 1 2 1 1]
        N (apply + (map #(apply * %) (map vector rs nrs)))
        p0 (/ (first nrs) N)
        zrs (average-consecutives rs nrs)
        lm (least-squares-log-log-linear-regression rs zrs)
        lgts (map lm rs)
        r*s (r-stars rs nrs lm)
        N* (apply + (map #(apply * %) (map vector nrs r*s)))
        probs (cons
               (float p0)
               (map #(* (- 1 p0) (/ % N*)) r*s))
        sum-probs (apply + probs)]
    (r-stars rs nrs lm))

  )


(defn stupid-backoff
  [trie probs words]
  (let [times-seen (or (get-in trie (concat words [:count])) 0)]
    (cond
      (= 1 (count words))
      (get-in probs [1 (:count (trie (first words)))])

      (< 0 times-seen)
      (/ times-seen (get-in trie (concat (butlast words) [:count])))

      :else
      (* 0.4 (stupid-backoff trie probs (butlast words))))))

(defn probability-of-sentence
  [trie probs sentence]
  ())
