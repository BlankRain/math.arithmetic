(ns
  #^{:author "Adeel Ansari",
     :doc "Empty"}
  clojure.math.arithmetic
  (:require [clojure.math.combinatorics :as comb])
  (use [clojure.math.numeric-tower]))

(defn mult-of-neibr-of-sixers? [x]
  (loop [[d & more] (range 6 (+ (sqrt x) 2) 6)]
    (if-not (nil? d)
      (if (zero? (* (rem x (inc d)) (rem x (dec d))))
        true
        (recur more))
      false)))

(defn subsets
  "It's better to use math.combinatorics/subsets, instead"
  ([s]
     (if (empty? s)
       '(())
       (subsets (first s) (next s) (conj () ()))))
  ([curr s res]
     (loop [cres res nres res]
       (cond
        (not (nil? cres)) (recur (next cres) (conj nres (conj (first cres) curr)))
        (not (nil? s)) (subsets (first s) (next s) nres)
        :else nres))))

(defn prime?
  "A trivial, rigorous, and straight forward approach. Very inefficient for large primes."
  [n]
  (if (<= n 1)
    false
    (if (or (= n 2) (= n 3))
      true
      (if (= 0 (* (rem n 2) (rem n 3)))
        false
        (not (mult-of-neibr-of-sixers? n))))))

(defn pseudoprime?
  "Fermat Pseudoprime"
  [n base]
  (if (or (prime? n) (= n 1))
    false
    (= 0 (rem (- (expt base (- n 1)) 1) n))))

(defn next-prime
  "A trivial, rigorous, and straight forward approach. Very inefficient for large numbers."
  [n]
  (if (< n 2)
    2
    (loop [k (if (odd? n) (+ n 2) (inc n))]
      (if (prime? k)
        k
        (recur (+ k 2))))))

(defn next-nprimes
  "A trivial and straight forward approach. Throws exception for n > 100."
  [from n]
  (if (< (* from n) 0)
    (throw (Exception. "Both 'from' and 'n' must be positive."))
    (if (> n 100)
      (throw (Exception. "Range exceeded the limit of 100."))
      (->> (iterate next-prime from) rest (take n) sort))))

(defn prime-factors
  "A trivial and straight forward approach. Very inefficient for large numbers."
  [n]
  (if (< 1 n)
    (loop [n n
           fcts ()
           d 2]
      (let [q (quot n d)]
        (cond
         (< q d) (sort (conj fcts n))
         (zero? (rem n d)) (recur q (conj fcts d) d)
         :else (recur n fcts (inc d)))))
    []))

(defn factors [n]
  (let [ ssets (comb/subsets (prime-factors n))]
       (loop [[f & more] ssets res #{}]
         (if-not (nil? f)
           (recur more (conj res (reduce * f)))
           (sort res)))))

(defn tau [n]
  (count (factors n)))

(defn sigma [n]
  (reduce + (factors n)))

(defn mobius [n]
  (let [pfacts (prime-factors n)]
    (let [tot-pfacts (count pfacts)
          tot-dist-pfacts (count (distinct pfacts))]
      (if (= tot-pfacts tot-dist-pfacts)
        (cond
         (and (even? tot-pfacts) ) 1
         :else -1)
        0))))

(defn practical? [n]
  (if-not (or (even? n) (== n 1))
    false
    (let [[f & more] (reverse (prime-factors n))]
      (loop [p f ps more sig (sigma (reduce * more))]
        (cond         
         (nil? ps) true
         (>= p (+ sig 1)) false
         :else (recur (first ps) (next ps) (sigma (reduce * (next ps)))))))))

(defn totient
  "Partially implemented, just for primes"
  [n]
  (if (prime? n)
    (dec n)
    nil))
