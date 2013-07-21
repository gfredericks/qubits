(ns com.gfredericks.qubits.examples.key-distribution
  (:require [com.gfredericks.qubits.objects :refer :all]))

;;
;; Quantum Key Distribution is a protocol that allows Alice and Bob to
;; establish a random shared secret key, using quantum mechanics to
;; give them an arbitrarily high likelihood of detecting evesdropping.
;;

;; First some background. We need to introduce the idea of making
;; different kinds of observations on a qubit. Our traditional
;; function (com.gfredericks.qubits.object/observe) measures what
;; we'll call the "value" of the qubit. Here we make a function that
;; instead measures its "sign". We can implement it using the normal
;; observation function by manipulating the qubit before observation,
;; and then undoing that manipulation afterwards:

(defn observe-sign
  [q]
  (H q)
  (let [outcome (observe q)]
    (H q)
    (case outcome 0 :+ 1 :-)))

;; We also make constructors for four different qubit states that
;; we'll care about:

(defn zero  [] (qubit))
(defn one   [] (doto (qubit) (X)))
(defn plus  [] (doto (qubit) (H)))
(defn minus [] (doto (one) (H)))

;; Let's also make an easy way to get statistical information about
;; qubit states

(defmacro sample
  [times & body]
  `(frequencies (repeatedly ~times (fn [] ~@body))))

;; Now we can demonstrate how these four states relate to our two
;; different observation functions. The zero/one functions represent
;; qubits with particular "values", so when we observe their value
;; the result is deterministic:

(comment
  (sample 1000 (observe (zero)))
  ;; => {0 1000}
  (sample 1000 (observe (one)))
  ;; => {1 1000}
  )

;; But when we observe their sign the result is random:

(comment
  (sample 1000 (observe-sign (zero)))
  ;; => {:- 504, :+ 496}
  (sample 1000 (observe-sign (one)))
  ;; => {:- 491, :+ 509}
  )

;; Conversely, the plus/minus states are nondeterministic when we
;; observe their values:

(comment
  (sample 1000 (observe (plus)))
  ;; => {0 469, 1 531}
  (sample 1000 (observe (minus)))
  ;; => {0 487, 1 513}
  )

;; But deterministic when we observe their sign:

(comment
  (sample 1000 (observe-sign (plus)))
  ;; => {:+ 1000}
  (sample 1000 (observe-sign (minus)))
  ;; => {:- 1000}
  )

;; Just like the value observation function, repeated observations
;; of the sign will yield the same result, even if the first one
;; was nondeterministic:

(comment
  (let [q (zero)]
    [(observe-sign q)
     (observe-sign q)
     (observe-sign q)
     (observe-sign q)
     (observe-sign q)])
  ;; => [:+ :+ :+ :+ :+]
  )

;; When we observe a qubit's sign and see the :+ result, the qubit's
;; state is now exactly the same as a fresh qubit returned from the
;; (plus) function, and likewise [:- (minus)], [0 (zero)], and
;; [1 (one)]. This means if we alternate between different kind of
;; observations, the results will be repeatedly random:

(comment
  (let [q (zero)]
    (map #(% q) (take 20 (cycle [observe-sign observe]))))
  ;; => (:+ 1 :- 1 :- 1 :+ 1 :- 1 :- 0 :- 1 :- 0 :+ 0 :- 0)
  )

;; So what we've established is that we have two different ways to
;; measure a qubit, and whichever way we choose causes the qubit to be
;; maximally undefined with regard to the other measurement.  This is,
;; I think, the essense of the Heisenburg Uncertainty Principle.

;; Now we can describe the protocol. I'm going to model Alice and Bob
;; as maps describing the messages that each actor can receive, where
;; the values are functions taking one argument (the body of the
;; message) and returning a triple:
;;   [next-expected-message-type message-type-to-send message-body-to-send]
;;
;; The main flow of control (that passes the messages back and forth)
;; will also function as the evesdropper. Alice and Bob pass both
;; classical and quantum messages, but since we're dealing with
;; Objects in both cases we don't really have to distinguish.

(def alice
  {:start
   (fn [_]

     )})