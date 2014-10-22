(ns com.gfredericks.qubits.examples.nats-arithmetic
  "Binary arithmetic with non-negative integers, represented as
  vectors of qubits."
  (:require [com.gfredericks.qubits.objects :as q]))

;; Can we do an (add a b) that adds to b in place?

(defmacro with-flipped
  [qs & body]
  `(do ~@(for [q qs] `(q/X ~q))
       ~@body
       ~@(for [q qs] `(q/X ~q))))

(defn add
  "XORs c with the sum of a and b."
  [a b c]
  {:pre [(>= (count c) (inc (max (count a) (count b))))
         (= (count a) (count b))]}
  (let [carry (q/qubit)]
    (dotimes [i (count c)]
      (let [qa (or (get a i) (q/qubit))
            qb (or (get b i) (q/qubit))
            qc (c i)]
        (q/X qc qa)
        (q/X qc qb)
        (q/X qc carry)
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
  {:pre [(>= (count c) (+ (count a) (count b)))]})
