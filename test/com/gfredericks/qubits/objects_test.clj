(ns com.gfredericks.qubits.objects-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.qubits.complex :as c]
            [com.gfredericks.qubits.objects :refer :all]))

(defn =ish [x y] (< (- x y) 0.0000001))
(def one? #(= 1 %))

(defn probably?
  [q p0 p1]
  (let [p (probabilities q)]
    (and (=ish p0 (p 0))
         (=ish p1 (p 1)))))

(deftest single-qubit-probabilities-tests
  (testing "that qubits start out in the |0> state"
    (qubits [q]
      (is (probably? q 1 0))))
  (testing "that the X gate puts a qubit in the |1> state"
    (qubits [q]
      (X q)
      (is (probably? q 0 1))))
  (testing "that the Z gate doesn't change initial probabilities"
    (qubits [q]
      (Z q)
      (is (probably? q 1 0))))
  (testing "that the Y gate flips the initial probabilities"
    (qubits [q]
      (Y q)
      (is (probably? q 0 1))))
  (testing "that the H gate gives equal probabilities"
    (qubits [q]
      (H q)
      (is (probably? q 1/2 1/2))))
  (testing "that two H gates reverts to initial probabilities"
    (qubits [q]
      (H q)
      (H q)
      (is (probably? q 1 0))))
  (testing "that two H gates with a Z or a Y in between reverses probabilities"
    (are [G] (qubits [q]
               (doto q H Z H)
               (probably? q 0 1))
         Z
         Y))
  (testing "that H Y Z H reverts to initial probabilities"
    (qubits [q]
      (doto q H Y Z H)
      (probably? q 1 0))))

(deftest single-qubit-observation-tests
  (qubits [q]
    (is (zero? (observe q)))
    (X q)
    (is (one? (observe q)))))
