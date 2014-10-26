(ns com.gfredericks.qubits.examples.deutsch-jozsa
  "http://en.wikipedia.org/wiki/Deutsch%E2%80%93Jozsa_algorithm"
  (:require [com.gfredericks.qubits.objects :as q]))

;;
;; Function constructors
;;

(defn constant-zero
  [inputs output]
  ;; noop
  )

(defn constant-one
  [inputs output]
  (q/X output))

(defn odd-inputs
  [inputs output]
  (doseq [input inputs]
    (q/X output input)))

(defn even-inputs
  [inputs output]
  (odd-inputs inputs output)
  (q/X output))

;;
;; Deutsch-Jozsa circuit
;;

(defn deutsch-jozsa
  "Returns :constant or :balanced."
  [f bit-count]
  (let [inputs (vec (repeatedly bit-count q/qubit))
        output (q/qubit)]
    (doseq [input inputs] (q/H input))
    (q/X output)
    (q/H output)
    (f inputs output)
    (doseq [input inputs] (q/H input))
    (map q/observe inputs)
    (case (q/observe (first inputs))
      0 :constant
      1 :balanced)))

(comment
  (deutsch-jozsa constant-one 3) => :constant
  (deutsch-jozsa constant-zero 3) => :constant
  (deutsch-jozsa odd-inputs 3) => :balanced
  (deutsch-jozsa even-inputs 3) => :balanced
  )
