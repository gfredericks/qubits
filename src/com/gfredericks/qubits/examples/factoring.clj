(ns com.gfredericks.qubits.examples.factoring
  (:require [com.gfredericks.qubits.objects :as q]
            [com.gfredericks.z :as z]))

(declare factor)

(defn lazy-exponentiation
  "Returns a lazy sequence of increasing numbers where the last is
  a^b."
  [a b]
  ((fn self [factor res b]
     (if (pos? b)
       (cons factor
             (lazy-seq
              (self (*' factor factor)
                    (if (even? b) res (*' res factor))
                    (quot b 2))))
       [res]))
   a 1 b))

(defn factor-as-odd-power
  "Returns the factorization of n if n = p^k for some odd prime p
  with k>1. otherwise returns nil."
  [n]
  (let [plausible-k (range (dec (.bitLength (biginteger n))) 1 -1)]
    (some (fn [k]
            ;; binary search
            (loop [lower-p 2
                   upper-p n]
              (let [mid-p (quot (+ lower-p upper-p) 2)]
                (when-not (= lower-p mid-p)
                  (let [n'-seq (lazy-exponentiation mid-p k)]
                    (if (some #(> % n) n'-seq) (recur lower-p mid-p)
                        (let [n' (last n'-seq)]
                          (if (= n n')
                            (repeat k mid-p)
                            (recur mid-p upper-p)))))))))
          plausible-k)))

(defn ^:private gcd
  [a b]
  (if (zero? b) a (recur b (rem a b))))

(defn ^:private mod-pow
  "Returns a^b mod n"
  [a b n]
  (bigint (.modPow (biginteger a) (biginteger b) (biginteger n))))

(defn quantum-find-order
  [])

(defn classical-find-order
  [n a]
  (->> (iterate #(mod (* % a) n) a)
       (take-while #(not= % 1))
       (count)
       (inc)))

(defn factor-quantumly
  [n]
  (let [a (+ 2 (rand-int (- n 2)))
        x (gcd a n)]
    (if (> x 1)
      ;; what luck!
      (concat (factor x) (factor (/ n x)))
      (let [r (quantum-find-order n a)]
        (if (odd? r)
          (recur n)
          (let [sqrt (mod-pow a (/ r 2) n)]
            (if (#{1 (dec n)} sqrt)
              (recur n)
              (let [x (gcd (dec sqrt) n)]
                (assert (< 1 x n))
                (concat (factor x) (factor (/ n x)))))))))))

(defn factor
  [n]
  {:pre [(integer? n) (pos? n)]}
  (cond (= 1 n) []
        (.isProbablePrime (biginteger n) 100) [n] ; good enough
        (even? n) (loop [twos [2]
                         n (/ n 2)]
                    (if (even? n)
                      (recur (conj twos 2) (/ n 2))
                      (into twos (factor n))))
        :else (or (factor-as-odd-power n)
                  (factor-quantumly n))))

;;
;; SCRATCH
;;

(defn DFT
  [zs]
  (let [m (count zs)]
    (mapv (fn [j]
            (z/*
             (z/real->z (/ (Math/sqrt m)))
             (apply z/+
                    (for [k (range m)]
                      (z/* (zs k)
                           (z/polar->z
                            1
                            (/ (* 2 Math/PI j k) m)))))))
          (range m))))
