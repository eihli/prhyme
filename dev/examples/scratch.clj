(ns examples.scratch
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set]
            [com.owoga.prhyme.nlp.core :as nlp]
            [com.owoga.prhyme.util.math :as math]))

(def re-word
  "Regex for tokenizing a string into words
  (including contractions and hyphenations),
  commas, periods, and newlines."
  #"(?s).*?([a-zA-Z\d]+(?:['\-]?[a-zA-Z]+)?|,|\.|\n)")

(defn tokenize
  "Tokenizes for suffix trie. First token is end of document."
  [text]
  (->> text
       (re-seq re-word)
       (map second)
       (map string/lower-case)
       (cons :bol)
       (reverse)
       (cons :eol)))

(defn tokenize-line
  [line]
  (->> line
       (string/trim)
       (re-seq re-word)
       (map second)
       (map string/lower-case)))

(comment
  (->> (slurp "dev/examples/sandman.txt")
       (#(string/split % #"\n"))
       (map tokenize-line))

  )

(defn zero-to-n-seq
  ([coll]
   (zero-to-n-seq coll 1))
  ([coll i]
   (let [l (count coll)]
     (if
         (> i l) nil
         (cons (take i coll)
               (lazy-seq (zero-to-n-seq coll (inc i))))))))
(comment
  (zero-to-n-seq '(1 2 3 4))
  ;; => ((1) (1 2) (1 2 3) (1 2 3 4))
  )

(defn i-to-j-seq
  ([coll i j]
   (zero-to-n-seq (->> coll (drop i) (take (- j i))))))

(defn n-to-zero-seq
  ([coll]
   (n-to-zero-seq coll 0))
  ([coll i]
   (if (= i (count coll)) nil
       (cons (drop i coll)
             (lazy-seq (n-to-zero-seq coll (inc i)))))))
(comment
  (n-to-zero-seq '(1 2 3 4))
  ;; => ((1 2 3 4) (2 3 4) (3 4) (4))
  )

(defn add-to-trie [trie coll]
  (update-in trie (concat coll [:count]) (fnil inc 0)))

(defn add-multiple-to-trie [trie colls]
  (loop [colls colls
         trie trie]
    (cond
      (empty? colls) trie
      :else (recur (rest colls)
             (add-to-trie trie (first colls))))))

(defn n-gram-suffix-trie
  "Creates a suffix trie of 1-gram to n-gram.
  Useful for backoff language model (I think)."
  [n tokens]
  (let [trie {}
        windows (partition (inc n) 1 tokens)]
    (loop [trie trie
           windows windows]
      (cond
        (= 1 (count windows))
        (add-multiple-to-trie
         trie
         (concat (zero-to-n-seq (first windows))
                 (rest (n-to-zero-seq (first windows)))))
        :else
        (recur (add-multiple-to-trie
                trie
                (zero-to-n-seq (first windows)))
               (rest windows))))))

(defn add-to-trie-1
  [trie n tokens]
  (let [pad-n (dec n)
        tokens (concat (repeat pad-n :bol) tokens (repeat pad-n :eol))
        partitions (partition n 1 tokens)]
    (reduce
     (fn [acc tokens]
       (update-in acc (concat tokens [:count]) (fnil inc 0)))
     trie
     partitions)))

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

(defn filter-trie-to-ngrams [trie n]
  (->> trie
       (flatmap)
       (partition 2)
       ;; Inc to account for :count
       (filter #(= (inc n) (count (first %))))))

(comment
  (let [trie {}]
    (-> (add-to-trie-1 trie 2 '("of" "lives" "lost" "at" "sea"))
        (add-to-trie-1 1 '("of" "lives" "lost" "at" "sea"))))
  )

(defn wrand
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn completions [trie probs words]
  (let [n (inc (count words))
        possibilities (->> (get-in trie words)
                           (filter #(string? (first %)))
                           (map (fn [[k v]]
                                  [k (get-in probs [n (:count v)])]))
                           (into {}))
        sum-probs (apply + (vals possibilities))
        possibilities (into {} (map (fn [[k v]] [k (/ v sum-probs)]) possibilities))]
    possibilities))

(comment
  ;; Turning corpus into a trie.
  (let [documents (->> "dark-corpus"
                       io/file
                       file-seq
                       (remove #(.isDirectory %))
                       (drop 500)
                       (take 5))
        trie (->> documents
                  (map slurp)
                  (mapcat #(string/split % #"\n"))
                  (map tokenize-line)
                  (filter #(> (count %) 1))
                  (take 500)
                  (reduce
                   (fn [acc tokens]
                     (-> (add-to-trie-1 acc 1 tokens)
                         (add-to-trie-1 2 tokens)
                         (add-to-trie-1 3 tokens)))
                   {}))
        probs (->> (range 1 4)
                   (map #(vector % (filter-trie-to-ngrams trie %)))
                   (map (fn [[n v]] [n (map #(second %) v)]))
                   (map (fn [[n v]] [n (into (sorted-map) (frequencies v))]))
                   (map (fn [[n v]] [n (math/sgt (keys v) (vals v))]))
                   (map (fn [[n [rs probs]]]
                          [n (into {} (map vector  rs probs))]))
                   (into {}))]
    (reverse (sort-by second (completions trie probs [:bol "you"]))))

  (into {} (map vector [1 2 3] [4 5 6]))
  ;;
  ;; => ([1 (1 2 8 7 3 6 4 23) (85 18 2 2 6 3 1 1)]
  ;;     [2 (1 2 5 3 4 7) (170 25 2 4 2 2)]
  ;;     [3 (1 2 3 4 7 5) (213 30 5 1 1 3)])
  (let [last-window '("in" "the" "frat")]
    (concat (zero-to-n-seq last-window)
            (rest (n-to-zero-seq last-window))))
  ;; => (("in") ("in" "the") ("in" "the" "frat") ("the" "frat") ("frat"))

  (n-gram-suffix-trie
   2
   (string/split
    "the cat in the hat is the rat in the frat"
    #" "))

  ;; => {"the"
  ;;     {:count 3,
  ;;      "cat" {:count 1, "in" {:count 1}},
  ;;      "hat" {:count 1, "is" {:count 1}},
  ;;      "rat" {:count 1, "in" {:count 1}},
  ;;      "frat" {:count 1}},
  ;;     "cat" {:count 1, "in" {:count 1, "the" {:count 1}}},
  ;;     "in" {:count 2, "the" {:count 2, "hat" {:count 1}, "frat" {:count 1}}},
  ;;     "hat" {:count 1, "is" {:count 1, "the" {:count 1}}},
  ;;     "is" {:count 1, "the" {:count 1, "rat" {:count 1}}},
  ;;     "rat" {:count 1, "in" {:count 1, "the" {:count 1}}},
  ;;     "frat" {:count 1}}
  )

(comment
  (def unigram
    (n-gram-suffix-trie
     1
     (tokenize (slurp "dev/examples/sandman.txt"))))

  unigram
  (->> unigram
       (map (fn [[k v]] (vector k (:count v))))
       (map second)
       (apply +))

  (def bigram
    (n-gram-suffix-trie
     2
     (tokenize (slurp "dev/examples/sandman.txt"))))

  (->> bigram
       (map (fn [[k v]] (vector k (:count v))))
       (map second)
       (apply +))

  (count bigram)
  (->> bigram
       (take 4)
       (into {}))
  ;; => {"cutest" {:count 2, "the" {:count 2, "him" {:count 2}}},
  ;;     "us" {:count 3, "bring" {:count 3, "," {:count 2}, "yeesss" {:count 1}}},
  ;;     "his" {:count 2, "that" {:count 2, "him" {:count 2}}},
  ;;     "him"
  ;;     {:count 8,
  ;;      "give" {:count 4, "\n" {:count 4}},
  ;;      "tell" {:count 2, "then" {:count 2}},
  ;;      "make" {:count 2, "\n" {:count 2}}}}
  (->> bigram
       vals
       (map :count)
       frequencies
       (into [])
       sort
       (map #(apply * %))
       (apply +))
  (count (tokenize (slurp "dev/examples/sandman.txt")))
  ;; => ([1 32] [2 20] [3 10] [4 3] [5 1] [6 2] [7 1] [8 2] [9 1] [10 1] [12 1] [26 1])
  )



(defn P [trie w]
  (let [ws (trie w)
        c (get-in trie [w :count])]
    (->> ws
         (#(dissoc % :count))
         (map
          (fn [[k v]]
            [k (/ (:count v) c)])))))

(defn vals-or-seconds [m]
  (cond
    (empty? m) m
    (map? m) (apply concat (vals m))
    :else (apply concat (map second m))))

(defn flat-at-depth
  "Convenience way of getting frequencies of n-grams.
  Given a trie with a depth of 0, it will return all 1-grams key/value pairs.
  That collection can be filtered for keys that hold the freqs."
  [m depth]
  (let [m (if (map? m) (into [] m) m)]
    (cond
      (<= depth 0) m
      :else (flat-at-depth (->> m (mapcat second) (remove #(= :count (first %))))
                           (dec depth)))))

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

(defn filter-trie-to-ngrams [trie n]
  (->> trie
       (flatmap)
       (partition 2)
       ;; Inc to account for :count
       (filter #(= (inc n) (count (first %))))))

(apply hash-map '([1 2] 3 [4 5] 6))

(comment
  (apply hash-map (flatmap {1 {2 {3 4} 5 {6 7}} 8 {9 10}} []))

  (let [trie {"d" {:count 3
                   "o" {:count 3
                        "g" {:count 2}
                        "t" {:count 1}}
                   "a" {:count 1
                        "y" {:count 1}}}
              "f" {:count 2
                   "o" {:count 1
                        "g" {:count 1}}
                   "i" {:count 1
                        "g" {:count 1}}}}]
    (filter-trie-to-ngrams trie 3))

  )


;; Let Nc be the number of N-grams that occur c times.
;; Good-turing discounting:
;; c* = (c + 1) * Nc+1 / Nc

(defn n-gram-frequencies [trie n]
  (if (< n 0)
    {}
    (->> trie
         (#(flat-at-depth % (dec n)))
         (map second)
         (map :count)
         frequencies
         (into (sorted-map)))))

(defn n-gram->occurence-count-frequencies [trie n]
  (n-gram-frequencies trie n))

(comment
  (def tokens ["d" "o" "g" "\n" "d" "a" "y" "\n" "d" "o" "g" "\n" "d" "o" "t"])
  (def trie (n-gram-suffix-trie 2 tokens))
  trie
  ;; => {"d"
  ;;     {:count 4,
  ;;      "o" {:count 3, "g" {:count 2}, "t" {:count 1}},
  ;;      "a" {:count 1, "y" {:count 1}}},
  ;;     "o" {:count 2, "g" {:count 2, "\n" {:count 2}}},
  ;;     "g" {:count 2, "\n" {:count 2, "d" {:count 2}}},
  ;;     "\n" {:count 3, "d" {:count 3, "a" {:count 1}, "o" {:count 2}}},
  ;;     "a" {:count 1, "y" {:count 1, "\n" {:count 1}}},
  ;;     "y" {:count 1, "\n" {:count 1, "d" {:count 1}}}}

  (count bigram)
  (count (flat-at-depth bigram 0))
  (->> bigram
       (#(flat-at-depth % 0))
       (filter #(= :count (first %)))
       (map second)
       frequencies
       (into (sorted-map))
       (map #(apply * %))
       (apply +))

  (n-gram-frequencies trie 2)
  ;; => {3 2, 1 3, 2 2}
  ;; for bigrams
  ;; of frequency 3 occurs 2 times
  ;; of frequency 2 occurs 2 times
  ;; of frequency 1 occurs 3 times

  (n-gram-frequencies trie 1)
  ;; => {4 1, 2 2, 3 1, 1 2}

  )

(defn num-seen-n-grams [trie n]
  (->> trie
       (#(flat-at-depth % (dec n)))
       (remove #(= :count (first %)))
       count))

(defn n-gram-frequency-map
  "Map of n-gram to frequency of frequencies."
  [trie n]
  (into
   {}
   (map
    #(vector % (n-gram-frequencies trie %))
    (range 1 (inc n)))))

(comment
  (n-gram-frequencies bigram 1)

  (n-gram-frequency-map bigram 2)

  )

(defn number-of-n-grams [trie n]
  (->> trie
       (#(flat-at-depth % (dec n)))
       (remove #(= :count (first %)))
       count))

(defn number-of-possible-n-grams [dict n]
  (int (Math/pow (count dict) n)))

(defn number-of-n-grams-that-occur-c-times [trie n c]
  (if (zero? c)
    (- (number-of-possible-n-grams trie n)
       (count (flat-at-depth trie (dec n))))
    (let [frequencies-map (->> (n-gram-frequency-map trie n)
                               (#(get % n)))]
      (get frequencies-map c 0))))

(comment
  (number-of-possible-n-grams bigram 2)
  (count (flat-at-depth bigram 1))
  (count bigram)
  (->> (number-of-n-grams-that-occur-c-times bigram 1 1))
 
  (->> (number-of-n-grams-that-occur-c-times bigram 0 3)
       (filter #(= :count (first %)))
       (map second)
       frequencies
       sort)
  )

(defn mle [trie c]
  (let [N (->> trie vals (map :count) (apply +))]
    (/ c N)))

(->> bigram
     (filter (fn [[k v]] (= 3 (v :count)))))

;; Good-Turing Smoothing
;;
;; There are 4 steps to perform the GT smoothing, which are:
;; 1. Count the frequency of frequency Nr
;; 2. Average all the non-zero counts using Zr = Nr / 0.5 (t - q)
;; 3. Fit a linear regression model log(Zr) = a + b log(r)
;; 4. Update r with r* using Katz equation and constant k, with
;; updated Zr corresponding to specific r read out from the linear
;; regression model.

(defn least-squares-linear-regression [xs ys]
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
    (println (format "intercept %f slope %f" b m))
    (fn [x]
      (+ b (* m x)))))

(comment
  (float ((least-squares-linear-regression
           [1 2 3 4]
           [2 4 5 7])
          5))
  )

(defn average-consecutives
  "Average all the non-zero counts using the equation
  q, r, t
  Zr = Nr / 0.5 (t - q)
  or
  Zr = 2 Nr / (t - q)"
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
        ys-avg-cons (average-consecutives xs ys)]
    (map float ys-avg-cons))

  ;; y = (r[j] + 1) * smoothed(r[j] + 1) / smoothed(r[j]);
  (let [xs [1 2 3 4 5 6 7 8 9 10 12 26]
        ys [32 20 10 3 1 2 1 1 1 2 1 1]
        ys-avg-cons (average-consecutives xs ys)
        log-xs (map #(Math/log %) xs)
        log-ys (map #(Math/log %) ys-avg-cons)
        lm (least-squares-linear-regression log-xs log-ys)
        zs (map lm log-xs)]
    ;; => [32 20 10 3 1 2 1 1 1 2 1/2 1/14]
    [log-ys log-xs zs (map #(Math/pow Math/E %) zs)])

  (Math/log 1)
  )

(defn turings-estimate [trie n r]
  (/ (* (inc r)
        (number-of-n-grams-that-occur-c-times trie n (inc r)))
     (number-of-n-grams-that-occur-c-times trie n r)))

(defn good-turing [trie n r]
  (let [nr (number-of-n-grams-that-occur-c-times trie n r)
        nr1 (number-of-n-grams-that-occur-c-times trie n (inc r))]
    (println
     (format "cx %d nc %d ncx1 %d - %f"
             r nr nr1 (float (/ (* (inc r) nr1) nr))))
    (/ (* (inc r) nr1) nr)))

(comment
  (number-of-n-grams-that-occur-c-times bigram 1 1)
  ;; unigram counts
  (def unigram-counts
    (->> bigram
         vals
         (map :count)
         frequencies
         (into (sorted-map))))
  ;; => {1 32, 2 20, 3 10, 4 3, 5 1, 6 2, 7 1, 8 1, 9 1, 10 2, 12 1, 26 1}
  ;; revised good-turing counts
  (->> unigram-counts
       (map
        (fn [[freq freq']]
          [freq (good-turing bigram 1 freq)]))
       (into (sorted-map)))
  ;; => {1 5/4, 2 3/2, 3 6/5, 4 5/3, 5 12, 6 7/2, 7 8, 8 9, 9 20, 10 0, 12 0, 26 0}
  (map (fn [[r nr]]
         (good-turing bigram 1 r))
       unigram-counts)

  ;; => (5/4 3/2 6/5 5/3 12 7/2 8 9 20 0 0 0)
  (turings-estimate bigram 1 7)
  )

(defn revise-frequencies [frequencies N]
  (let [m (reverse (sort (keys frequencies)))]
    (loop [revised {}
           m m]
      (cond
        (empty? m) revised
        :else
        (recur
         (assoc
          revised
          (first m)
          (good-turing (get frequencies (first m) 0)
                       (get frequencies (second m) 0)
                       N))
         (rest m))))))

(comment
  (get (n-gram-frequency-map trie 3) 1)
  ;; => {4 1, 2 2, 3 1, 1 2}
  (revise-frequencies
   (get (n-gram-frequency-map trie 3) 1)
   (apply + (map :count (vals trie))))
  ;; => {4 2/13, 3 4/13, 2 3/13, 1 0}

  (def n-gram-freq-map (n-gram-frequency-map trie 3))
  (def unigram-frequencies (n-gram-freq-map 1))
  unigram-frequencies
  )

(defn number-of-n-grams-that-occur-with-count [trie n c]
  )
(defn good-turing-discount [trie c]
  )

(->> bigram
     (map second))
(count (into #{} (tokenize (slurp "dev/examples/sandman.txt"))))
(->> bigram
     (map second)
     (map #(dissoc % :count))
     (map keys)
     flatten
     (into #{})
     (clojure.set/difference (into #{} (keys bigram))))

(partition 3 1 (repeat :end) (range 6))

(let [documents (->> "dark-corpus"
                     io/file
                     file-seq
                     (remove #(.isDirectory %))
                     (take 10))]
  documents)
