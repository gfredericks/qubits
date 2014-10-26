(ns com.gfredericks.qubits.examples.nats-arithmetic-test
  (:refer-clojure :exclude [mod])
  (:require [clojure.test :refer :all]
            [com.gfredericks.qubits.examples.nats-arithmetic :refer :all]
            [com.gfredericks.qubits.objects :as q]))

(deftest addition-test
  (dotimes [_ 10]
    (let [as (n->qs 5 0)
          bs (n->qs 5 0)
          cs (n->qs 6 0)]
      (dotimes [i (count as)] (q/H (as i)))
      (dotimes [i (count bs)] (q/H (bs i)))
      (add as bs cs)
      (is (= (qs->n cs)
             (+ (qs->n as)
                (qs->n bs)))))))

(deftest multiplication-test
  (dotimes [_ 10]
    (let [as (n->qs 5 0)
          bs (n->qs 5 0)
          cs (n->qs 10 0)]
      (dotimes [i (count as)]
        (when (zero? (rand-int 3))
          (q/H (as i))))
      (dotimes [i (count bs)]
        (when (zero? (rand-int 3))
          (q/H (bs i))))
      (multiply as bs cs)
      (is (= (qs->n cs)
             (* (qs->n as)
                (qs->n bs)))))))

(deftest subtraction-test
  (dotimes [_ 30]
    (let [as (n->qs 5 0)
          bs (n->qs 5 0)
          cs (n->qs 5 0)
          overflow? (q/qubit)]
      (dotimes [i (count as)]
        (when (zero? (rand-int 3))
          (q/H (as i))))
      (dotimes [i (count bs)]
        (when (zero? (rand-int 3))
          (q/H (bs i))))
      (subtract as bs cs overflow?)
      (let [a (qs->n as)
            b (qs->n bs)
            c (qs->n cs)
            o? (= 1 (q/observe overflow?))]
        (is (if o?
              (> b a)
              (and (<= b a) (= c (- a b)))))))))

(deftest mod-test
  (dotimes [_ 10]
    (let [b (+ 2 (rand-int 30))
          as (n->qs 5 0)
          cs (n->qs 5 0)]
      (dotimes [i (count as)] (q/H (as i)))
      (mod as b cs)
      (let [a (qs->n as), c (qs->n cs)]
        (is (= (clojure.core/mod a b) c))))))

(deftest mod-pow-test
  (dotimes [_ 10]
    (let [bits 3
          as (n->qs bits 0)
          bs (n->qs bits 0)
          n (+ 2 (rand-int (-> 1 (bit-shift-left bits) (dec))))
          cs (n->qs bits 0)]
      (dotimes [i (count as)] (q/H (as i)))
      (dotimes [i (count bs)] (q/H (bs i)))
      (mod-pow as bs n cs)
      (let [a (qs->n as)
            b (qs->n bs)
            c (qs->n cs)]
        (is (= c (clojure.core/mod (apply *' (repeat b a)) n)))))))
