(ns com.owoga.prhyme.generation.simple-good-turing
  (:require [clojure.set]
            [clojure.string :as string]
            [clojure.set :as set]))

;; Pythons NLTK is a great resource for this.
;; https://github.com/nltk/nltk/blob/2.0.4/nltk/probability.py
;;
;; Useful to check out commit 3c8a25379 and look at nltk/model/ngram.py

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn tokenize-line
  [line]
  (->> line
       (string/trim)
       (re-seq re-word)
       (map second)
       (map string/lower-case)))

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
  (let [Nr1 (or Nr1 0)]
    (Math/sqrt
     (* (Math/pow (inc r) 2)
        (/ Nr1 (Math/pow Nr 2))
        (inc (/ Nr1 Nr))))))

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
  (let [smoothed (fn [r]
                   (* (inc r)
                      (/ (lm (inc r))
                         (lm r))))
        turing (fn [r Nᵣ Nᵣ₊₁]
                 (* (inc r)
                    (/ Nᵣ₊₁ Nᵣ)))]
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
            (smoothed (first rs))))
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
                (smoothed (first rs))
                (turing (first rs) (first nrs) (second nrs)))))))))))

(defn r*
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
  (let [smoothed (fn [r]
                   (* (inc r)
                      (/ (lm (inc r))
                         (lm r))))
        turing (fn [r Nᵣ Nᵣ₊₁]
                 (* (inc r)
                    (/ Nᵣ₊₁ Nᵣ)))]
    (loop [rs rs
           nrs nrs
           lgt? false
           result []]
      (cond
        (empty? rs) result
        :else
        (let [r (first rs)
              Nᵣ (first nrs)
              Nᵣ₊₁ (or (second nrs) 0)
              lgt-estimate (lm r)
              turing-estimate Nᵣ
              stdv (stdv-for-turing-estimate r Nᵣ Nᵣ₊₁)
              lgt? (or lgt?
                       (> (Math/abs (- lgt-estimate turing-estimate))
                          (* 1.65 stdv))
                       ;; Note possibility for the turing estimate to
                       ;; require an out-of-range Nr+1
                       ;; if we get to the end of nrs and still aren't
                       ;; taking the linear good-turing estimate.
                       (nil? (second nrs)))]
          (recur
           (rest rs)
           (rest nrs)
           lgt?
           (conj
            result
            (if lgt?
              (smoothed (first rs))
              (turing (first rs) (first nrs) (second nrs))))))))))

(defn make-r*
  "Returns a function that takes an r and returns an r*."
  [rs nrs lm]
  (let [r*s (->> (r* rs nrs lm)
                 (map vector rs)
                 (into (sorted-map)))]
    (fn [r]
      (get r*s r (* (inc r)
                    (/ (lm (inc r))
                       (lm r)))))))

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

(defn flatmap
  ([m]
   (flatmap m []))
  ([m prefix]
   (mapcat
    (fn [[k v]]
      (if (map? v)
        (flatmap v (conj prefix k))
        [(conj prefix k) v]))
    m)))

(defn add-to-trie
  [trie n tokens]
  (let [pad-n n
        tokens (concat (repeat (max 1 (dec pad-n)) "<s>") tokens ["</s>"])
        partitions (partition n 1 tokens)]
    (reduce
     (fn [acc tokens]
       (update-in acc (concat tokens [:count]) (fnil inc 0)))
     trie
     partitions)))

(defn lines->trie
  [lines n]
  (->> lines
       (map tokenize-line)
       (filter #(> (count %) 1))
       (reduce
        (fn [acc tokens]
          (reduce
           (fn [acc n]
             (add-to-trie acc n tokens))
           acc
           (range 1 (inc n))))
        {})))

(lines->trie '("hi there" "hi eric" "my name is eric") 2)

(defn filter-trie-to-ngrams [trie n]
  (->> trie
       (flatmap)
       (partition 2)
       ;; Inc to account for :count
       (filter #(= (inc n) (count (first %))))))

(defn depth-of-map
  [m]
  (loop [d 0
         m m]
    (let [child-maps (filter map? (vals m))]
      (if (empty? child-maps)
        d
        (recur (inc d) (first child-maps))))))

(defn trie->r*s
  [trie]
  (let [depth (depth-of-map trie)
        ngram-rs-nrs-map
        (into
         {}
         (map
          (fn [d]
            (let [flattened (filter-trie-to-ngrams trie d)]
              [d (into (sorted-map) (frequencies (map second flattened)))]))
          (range 1 (inc depth))))]
    (into
     {}
     (map
      (fn [[ngram rs-nrs-map]]
        (let [rs (keys rs-nrs-map)
              nrs (vals rs-nrs-map)
              N (apply + (map #(apply * %) (map vector rs nrs)))
              r0 (first nrs)
              zrs (average-consecutives rs nrs)
              lm (least-squares-log-log-linear-regression rs zrs)]
          [ngram {:N N
                  :r0 r0
                  :rs rs
                  :nrs (first nrs) nrs
                  :zrs (first nrs) zrs
                  :lm lm
                  :r*s (into
                        (sorted-map)
                        (map vector rs (r-stars rs zrs lm)))}]))
      ngram-rs-nrs-map))))

 ;; zrs (average-consecutives rs nrs)
 ;; lm (least-squares-log-log-linear-regression rs zrs)


(defn stupid-backoff
  [trie probs words]
  (let [times-seen (or (get-in trie (concat words [:count])) 0)]
    #_(when (and (> times-seen 0)
                 (> (count words) 2))
        (Thread/sleep 100)
        (println "Seen" words times-seen (get-in trie (concat (butlast words) [:count]))))
    (cond
      (= 1 (count words))
      (let [r (get-in probs [1 (:count (get trie (first words) {:count 0}))])]
        (if (nil? r)
          (println "1" words)
          r))

      (< 0 times-seen)
      (let [r (/ times-seen (get-in trie (concat words [:count])))]
        (if (nil? r)
          (println "0" words)
          r))

      :else
      (* 0.4 (stupid-backoff trie probs (rest words))))))

(defn mle
  [trie words]
  (let [r (get-in trie (concat words [:count]) 0)
        q (get-in trie (concat (butlast words) [:count]))]
    (/ r q)))

(declare katz-beta-alpha)

(defn theta [x]
  (if (zero? x) 1 0))

(defn P-bar
  [trie r*s words]
  (let [n (count words)
        c (get-in trie (concat words [:count]) 0)
        r* (get-in r*s [n :r*s c])
        N (get-in r*s [n :N])]
    (if (= 1 n)
      (/ r* N)
      (let [c-1 (get-in trie (concat (butlast words) [:count]) 0)
            d (/ r* c)]
        (println "dr" d r* c)
        (* d (/ c c-1))))))

(defn P-sub-s
  [trie r*s k words]
  (let [c (get-in trie (concat (butlast words) [:count]) 0)]
    (if (> c k)
      (P-bar trie r*s words)
      (let [alpha (katz-beta-alpha trie r*s k words)]
        (println "alpha" alpha)
        (* alpha (P-sub-s trie r*s k (rest words)))))))


(defn katz-estimator
  [trie r*s k words]
  (Thread/sleep 100)
  (println words)
  (if (= 1 (count words))
    (let [c (get-in trie (concat words [:count]))]
      (if c
        (/ (get-in r*s [1 :r*s c]) (get-in r*s [1 :N]))
        (/ (get-in r*s [1 :r0])
           (get-in r*s [1 :N]))))
    (let [r (get-in trie (concat words [:count]) 0)]
      (if (> r 0)
        (let [n (count words)
              r* (get-in r*s [n :r*s r])
              r-1 (get-in trie (concat (butlast words) [:count]) 1)
              d (/ r* r)]
          (* d (/ r r-1)))
        (let [alpha (/ (katz-beta-alpha trie r*s k words)
                       (katz-beta-alpha trie r*s k (rest words)))]
          (* alpha
             (katz-estimator
              trie
              r*s
              k
              (rest words))))))))

(defn katz-beta-alpha
  [trie r*s k words]
  (let [ngrams (->> (get-in trie (butlast words))
                    (remove #(= :count (first %)))
                    (filter (fn [[_ v]] (> (:count v) k)))
                    (map first)
                    (map #(concat (butlast words) [%]))
                    (map #(P-bar trie r*s %))
                    (apply +))]
    (- 1 ngrams)))

(defn katz-alpha
  [trie r*s words b]
  (let [denom (->> (get-in trie (rest (butlast words)))
                   ((fn [a]
                      (println a)
                      a))
                   (remove #(= :count (first %)))
                   (map first)
                   (map #(concat (rest (butlast words)) %))
                   (map #(katz-estimator trie r*s %))
                   (apply +)
                   (- 1 ))]
    (println denom)
    (/ b denom)))

(defn beta
  "Estimate of the sum of conditional probabilities of all words wₘ which never
  followed wₘ₋₁."
  [trie r*s words]
  (let [n (count words)
        r (get-in trie (concat words [:count]) 0)
        r* (get-in r*s [n :r*s r])
        d (/ r* r)]
    (if (zero? r)
      1
      (* d (/ r (get-in trie (concat (butlast words) [:count])))))))

(defn sum-of-betas
  [trie r*s words]
  (let [ngrams (->> (get-in trie words)
                    (remove #(= (first %) :count))
                    (map first)
                    (map #(concat words [%])))]
    (- 1 (->> ngrams
              (map (partial beta trie r*s))
              (apply +)))))

(defn prob-observed-ngram
  [trie r*s words]
  (let [observed (->> (get-in trie words)
                      (remove #(= :count (first %))))
        dictionary (->> trie
                        (remove #(= :count (first %)))
                        (map first))
        unobserved (clojure.set/difference
                    (into #{} dictionary)
                    (into #{} (map first observed)))
        ;; The likelihood of an observed ngram
        ;; is 1 - N₁/N, so the sum of observed counts
        ;; needs to be normalized by that.
        sum-of-observed-counts (->> observed
                                    (map second)
                                    (map :count)
                                    (apply +))
        ;; The likelihood of an unobserved ngram
        ;; is N₁/N, so the sum of unobserved counts
        ;; needs to be normalized with that.
        sum-of-unobserved-counts (->> unobserved
                                      (map #(get trie %))
                                      (map :count)
                                      (apply +))]
    [sum-of-observed-counts sum-of-unobserved-counts (take 5 unobserved)]))

(declare katz-smoothing)

(defn alpha
  [trie r*s words k]
  (let [n (count words)
        beta-mass (sum-of-betas trie r*s words)
        denom (->> (get-in trie (butlast words))
                   (remove #(= (first %) :count))
                   (filter (fn [[kw v]]
                             (<= (:count v) k)))
                   (map (fn [[kw v]]
                          (let [new-words (concat (rest (butlast words)) [kw])
                                #_#__ (do (println new-words))
                                smoothed (katz-smoothing trie r*s new-words k)]
                            (println smoothed kw)
                            smoothed)))
                   (apply +))]
    (println beta-mass denom)
    (/ beta-mass denom)))

(defn katz-smoothing
  [trie r*s words k]
  (let [n (count words)
        r (get-in trie (concat words [:count]) 0)
        r-1 (get-in trie (concat (butlast words) [:count]))
        r* (get-in r*s [n :r*s r])
        d (/ r* r)]
    (if (> r k)
      (* d (/ r r-1))
      (* (alpha trie r*s (butlast words) k)
         (katz-smoothing trie r*s (rest words) k)))))

(defn katz-backoff
  [trie probs r*s words]
  (let [k 0
        n (count words)
        c (get-in trie (concat words [:count]) 0)
        d (/ (r*s c) c)
        v (* d (/ c (get-in trie (concat (butlast words) [:count]))))]
    (if (> c k)
      v
      (let [words (butlast words)
            b (->> (get-in trie words)
                   (remove (fn [[k _]] (= :count k)))
                   (filter (fn [[k v]] (> (:count v) k)))
                   (map (fn [[k v]]
                          (let [c-den (:count v)
                                c-num (get-in trie (concat words [k :count]))
                                d (/ (r*s c) )]
                            (* d (/ c-num c-den)))))
                   (apply +))]
        b))))


(defn make-r
  [trie n-gram]
  (:count (get-in trie n-gram {:count 0})))

(defn make-nᵣ
  [trie n-gram r]
  (->> trie
       (#(filter-trie-to-ngrams % (count n-gram)))
       (map second)
       (frequencies)
       (#(get % r))))

(defn linear-good-turing-frequency-estimator
  [rs Nrs]
  (let [averaged (average-consecutives rs Nrs)]
    (least-squares-log-log-linear-regression rs averaged)))

(defn simple-good-turing-estimator
  "r* = (r + 1) * E(Nᵣ₊₁) / E(Nᵣ)
  Where E is an 'estimator'.

  The Turing Estimator is simply the identity function, substituting Nᵧ for E(Nᵧ).

  The Linear Good-Turing Estimator is a linear regression model
  over the log rs log nrs inputs.

  The Simple Good-Turing Estimator switches from the Turing Estimator to the
  Linear Good-Turing Estimator whenever the difference between the two
  exceeds some value deemed 'significant' (for example, 1.65 times the standard
  deviation of the Turing estimate).

  Returns a function that takes `r`, a frequency, and returns
  the `r*`, the estimated frequency of that frequency."
  [rs Nrs]
  (let [r->Nr (into (sorted-map) (map vector rs Nrs))
        lgt-estimator (linear-good-turing-frequency-estimator rs Nrs)
        r*-fn (fn [estimator r]
                (* (inc r)
                   (/ (estimator (inc r))
                      (estimator r))))
        r*s (loop [rs rs
                   Nrs Nrs
                   lgt? false
                   result []]
              (if (empty? rs)
                result
                (let [r (first rs)
                      lgt-estimate (r*-fn lgt-estimator r)]
                  (if (nil? (r->Nr (inc r)))
                    (recur
                     (rest rs)
                     (rest Nrs)
                     lgt?
                     (conj
                      result
                      lgt-estimate))
                    (let [turing-estimate (r*-fn r->Nr r)
                          stdv (stdv-for-turing-estimate r (r->Nr r) (r->Nr (inc r)))
                          lgt? (or lgt?
                                   (> (* 1.65 stdv)
                                      (Math/abs (- lgt-estimate turing-estimate)))
                                   ;; Note possibility for the turing estimate to
                                   ;; require an out-of-range Nr+1
                                   ;; if we get to the end of Nrs and still aren't
                                   ;; taking the linear good-turing estimate.
                                   (nil? (second Nrs)))]
                      (recur
                       (rest rs)
                       (rest Nrs)
                       lgt?
                       (conj
                        result
                        (if lgt?
                          lgt-estimate
                          turing-estimate))))))))
        r->r*-map (into (sorted-map) (map vector rs r*s))
        r->r* (fn [r]
                (get r->r*-map r (r*-fn lgt-estimator r)))]
    r->r*))

(defn sgt-estimates
  "Returns list of r*s using Simple Good-Turing."
  [rs Nrs]
  (let [sgt-estimator (simple-good-turing-estimator rs Nrs)]
    (map sgt-estimator rs)))

(defn normalize-estimates
  "Normalizes r*s (and P0) probabilities."
  [rs r*s P0]
  (let [N (apply + (map #(apply * %) (map vector rs r*s)))
        probability (fn [r]
                      (* (- 1 P0)
                         (/ r N)))
        sum-probabilities (apply + (map probability r*s))]
    (map
     (fn [r*]
       (* (- 1 P0)
          (/ (probability r*) sum-probabilities)))
     r*s)))

(defn simple-good-turing-probability
  "Returns a function that given an `r` returns a probability."
  [rs Nrs]
  (let [N (apply + (map #(apply * %) (map vector rs Nrs)))
        P0 (float (/ (first Nrs) N))
        r*s (sgt-estimates rs Nrs)
        probs (normalize-estimates rs r*s P0)]
    (into
     (sorted-map)
     (map vector rs probs))))

(defn simple-good-turing-discount
  [r
   r-plus-one-estimated-frequency
   r-estimated-frequency]
  (* (inc r)
     (/ r-plus-one-estimated-frequency
        r-estimated-frequency)))

(defn turing-probablity
  [discounted-count-of-ngram sample-text-size]
  (/ discounted-count-of-ngram sample-text-size))

(defn maps-for-simple-good-turing
  [trie]
  (let [ns (range 1 (inc (depth-of-map trie)))
        n->r->nr
        (into
         {}
         (map
          (fn [d]
            (let [flattened (filter-trie-to-ngrams trie d)]
              [d (into (sorted-map) (frequencies (map second flattened)))]))
          ns))
        Ns (into
            {}
            (map
             (fn [n]
               [n
                (apply + (map #(apply * %)
                              (map
                               vector
                               (keys (n->r->nr n))
                               (vals (n->r->nr n)))))])
             ns))
        P0s (into
             {}
             (map
              (fn [n]
                [n (float (/ (get-in n->r->nr [n 1]) (Ns n)))])
              ns))
        n->r->sgt-prob
        (into
         {}
         (map
          (fn [n]
            [n
             (into
              (sorted-map)
              (simple-good-turing-probability
               (keys (n->r->nr n))
               (vals (n->r->nr n))))])
          ns))]
    [n->r->nr
     n->r->sgt-prob
     Ns
     P0s]))

(defn simple-good-turing
  [trie]
  (let [ns (range 1 (inc (depth-of-map trie)))
        [n->r->nr
         n->r->sgt-prob
         Ns
         P0s] (maps-for-simple-good-turing trie)]
    (fn [vocab-set ngram]
      (let [n (count ngram)
            c (get-in
               trie
               (concat ngram [:count])
               0)
            seen
            (into #{} (remove #{:count} (keys (get-in trie ngram))))
            unseen (set/difference vocab-set seen)]
        (get-in
         n->r->sgt-prob
         [n c]
         (float (/ (P0s n) (count unseen))))))))
