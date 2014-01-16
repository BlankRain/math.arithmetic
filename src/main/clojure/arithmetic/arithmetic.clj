(ns
  #^{:author "Adeel Ansari",
     :doc "Empty"}
  clojure.math.arithmetic
  (:require [clojure.math.combinatorics :as comb])
  (use [clojure.contrib.math]))

(defn mult-of-neibr-of-sixers? [x]
  (loop [[d & more] (range 6 (+ (sqrt x) 2) 6)]
    (if-not (nil? d)
      (if (zero? (* (rem x (inc d)) (rem x (dec d))))
        true
        (recur more))
      false)))

(defn subsets
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

(defn prime? [n]
  (if (<= n 1)
    false
    (if (or (= n 2) (= n 3))
      true
      (if (= 0 (* (rem n 2) (rem n 3)))
        false
        (not (mult-of-neibr-of-sixers? n))))))

(defn next-prime [n]
  (if (< n 2)
    2
    (loop [k (if (odd? n) (+ n 2) (inc n))]
      (if (prime? k)
        k
        (recur (+ k 2))))))

(defn next-nprimes [from n]
  (if (< (* from n) 0)
    (throw (Exception. "Both 'from' and 'n' must be positive."))
    (if (> n 100)
      (throw (Exception. "Range exceeded the limit of 100."))
      (->> (iterate next-prime from) rest (take n) sort))))

(defn prime-factors [n]
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

(defn dist-prime-factors [n]
  (distinct (prime-factors n)))

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

(defn totient [n]
  1)

(defn prob-u3-1_6 [till d]
  (let [s (bigint 1) till (inc till)]
    (loop [t (bigint 2) res 0]
      (if (<= t till)
        (recur (inc t) (+ res (reduce * (range s t))))
        (rem res 7)))))
