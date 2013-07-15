(ns com.gfredericks.qubits.objects-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.qubits.complex :as c]
            [com.gfredericks.qubits.objects :refer :all]))

(defn =ish [x y] (< (- x y) 0.0000001))

(deftest single-qubit-probabilities-tests
  (testing "that qubits start out in the |0> state"
    (let [q (qubit)
          p (probabilities q)]
      (is (=ish 0 (p 1)))
      (is (=ish 1 (p 0)))))
  (testing "that the X gate puts a qubit in the |1> state"
    (qubits [q]
            (X q)
            (let [p (probabilities q)]
              (is (=ish 0 (p 0)))
              (is (=ish 1 (p 1)))))))

#_(deftest single-qubit-observation-tests
  (let [q (qubit)]
    (is (zero? (observe q)))))
