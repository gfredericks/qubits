(ns com.gfredericks.qubits.examples.nats-arithmetic
  "Binary arithmetic with non-negative integers, represented as
  vectors of qubits."
  (:refer-clojure :exclude [mod])
  (:require [com.gfredericks.qubits.objects :as q]))

(defn qvec [n] (vec (repeatedly n q/qubit)))

;; Can we do an (add a b) that adds to b in place?

(defmacro with-flipped
  [qs & body]
  `(do ~@(for [q qs] `(q/X ~q))
       ~@body
       ~@(for [q qs] `(q/X ~q))))

(defn add
  "XORs c with the sum of a and b. Takes optional control qubits."
  [a b c & controls]
  {:pre [(>= (count c) (inc (max (count a) (count b))))]}
  (let [carry (q/qubit)]
    (dotimes [i (count c)]
      (let [qa (or (get a i) (q/qubit))
            qb (or (get b i) (q/qubit))
            qc (c i)]
        (apply q/X qc qa controls)
        (apply q/X qc qb controls)
        (apply q/X qc carry controls)
        ;; the carry bit changes whenever a and b are the same and
        ;; are different from the carry bit...

        ;; can this be refactored to use a constant number of qubits
        ;; instead of creating a new one each time?
        (let [flip-carry? (q/qubit)]
          (with-flipped [carry]
            (q/X flip-carry? qa qb carry))

          (with-flipped [qa qb]
            (q/X flip-carry? qa qb carry))

          (q/X carry flip-carry?))))))

(defn multiply
  "XORs c with the product of a and b."
  [a b c]
  {:pre [(>= (count c) (+ (count a) (count b)))]}
  (loop [a a
         i 0
         acc (qvec (count a))]
    #_(clojure.pprint/pprint
     {:a a :b b :acc acc})
    (if (= i (count b))
      (dotimes [i (count c)]
        (q/X (c i) (acc i)))
      (let [acc' (qvec (+ 1 i (count c)))
            a' (qvec (count a))]
        (dotimes [j (count a)] (q/X (a' j) (a j) (b i)))
        (add acc a' acc')
        (recur (vec (cons (q/qubit) a)) (inc i) acc')))))

(defn copy
  [qs]
  (let [qs' (qvec (count qs))]
    (dotimes [i (count qs)]
      (q/X (qs' i) (qs i)))
    qs'))

(defn negate
  "XORs b with the two's complement of a."
  [a b]
  (let [a' (copy a)
        one [(doto (q/qubit) (q/X))]]
    (dotimes [i (count a')] (q/X (a' i)))
    (add a' one (conj b (q/qubit)))))

(defn subtract
  "XORs c with (a - b), and XORs the overflow qubit if a < b."
  [a b c overflow?]
  {:pre [(= (count a) (count b) (count c))]}
  (let [a (conj a (q/qubit))
        b (conj b (q/qubit))
        c (conj c overflow? (q/qubit))
        b' (qvec (count b))]
    (negate b b')
    (add a b' c)))

(defn ^:private n->qs
  [bits n]
  (let [qs (qvec bits)]
    (loop [i 0 n n]
      (when (< i bits)
        (if (odd? n) (q/X (qs i)))
        (recur (inc i) (quot n 2))))
    qs))

(defn mod
  "Reduces a mod b and XORs the result into c."
  [a b c]
  {:pre [(>= (count c) (.bitLength (biginteger (dec b))))]}
  (let [max (apply * (repeat (count a) 2N))
        multiples (->> (iterate #(* 2 %) b)
                       (take-while #(< % max))
                       (reverse))]
    (loop [a a
           multiples multiples]
      (if-let [[x & xs] (seq multiples)]
        (let [a' (qvec (count a))
              a'' (qvec (count a))
              xvec (n->qs (count a') x)
              of? (q/qubit)]
          (subtract a xvec a' of?)
          (dotimes [i (count a)]
            (q/X (a'' i) (a i) of?))
          (q/X of?)
          (dotimes [i (count a)]
            (q/X (a'' i) (a' i) of?))
          (recur a'' xs))
        (dotimes [i (count c)]
          (q/X (c i) (a i)))))))
