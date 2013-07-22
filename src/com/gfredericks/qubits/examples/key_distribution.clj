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


;; Now we can describe the protocol. The basic idea is that Alice will
;; generate some random bits, and then encode those bits in a series
;; of qubits, where in each case she randomly decides to encode as
;; either the value of the qubit or the sign. She keeps track of which
;; way she encoded it for each qubit.
;;
;; Then she sends the qubits to Bob, who picks randomly for each one
;; whether to measure its sign or value, and decodes based on that
;; random choice. He then sends back to Alice the list of how he
;; chose to measure each qubit.
;;
;; Alice receives that list and compares it to her own to determine
;; which qubits Bob observed correctly. The incorrect ones can be
;; tossed since Bob would have just observed a random value. Of the
;; correct ones, a subset are chosen at random to serve as
;; verification bits. For these Alice sends the actual bit values to
;; Bob so he can check that he observed the correct value for them.
;; If he didn't eavesdropping is assumed and the protocol is aborted.
;; Otherwise Alice and Bob assume that the correctly-observed bits
;; that weren't used for verification can function as a shared secret.


;; To the code. I'm going to model Alice and Bob as maps describing
;; the messages that each actor can receive, where the values are
;; functions taking two arguments (the current state of the agent and
;; the message received) and returning a pair (the new state of the
;; agent and the message to send back).
;;
;; The main flow of control (that passes the messages back and forth)
;; will also function as the evesdropper. Alice and Bob pass both
;; classical and quantum messages, but since we're dealing with
;; Objects in both cases we don't really have to distinguish.


(def alice-responses
  ;; Step 1 of the protocol
  {:start
   (fn [{:keys [bitcount], :as state} _]
     ;; We start by generating some random bits from which the shared
     ;; secret will be drawn
     (let [bits (repeatedly bitcount #(rand-int 2))
           ;; We also need to pick how to encode each bit, as either
           ;; the value or the sign of a qubit
           bases (repeatedly bitcount #(rand-nth [:value :sign]))
           ;; Here we actually construct the qubits to send to Bob
           qubits (map (fn [bit basis]
                         (case basis
                           :value
                           (case bit 0 (zero), 1 (one))
                           :sign
                           (case bit 0 (plus), 1 (minus))))
                       bits
                       bases)]
       [(assoc state
          :bits bits
          :bases bases)
        [:qubits qubits]]))

   ;; Step 3 of the protocol -- Bob has just sent the list of bases he
   ;; used to measure the qubits we sent. I.e., it is a list like
   ;; [:value :value :sign :value ...]
   ;;
   ;; We're going to compare that to the list of bases we used to
   ;; figure out which bits Bob should have measured correctly; then
   ;; we'll pick half of those to use as verification bits, and send
   ;; the values for those bits to Bob for evesdropping-checking.
   :bases
   (fn [{:keys [bits bases], :as state} bases-from-bob]
     (let [good-indices
           ;; First figure out which qubits were measured correctly
           (for [[i basis basis-from-bob] (map list (range) bases bases-from-bob)
                 :when (= basis basis-from-bob)]
             i)

           ;; Split the correctly measured qubits into verification
           ;; and key groups
           [verification-indices key-indices]
           (->> good-indices
                (shuffle)
                (split-at (quot (count good-indices) 2))
                (map sort))

           ;; Assemble the verification message
           verification (into {} (for [i verification-indices]
                                   [i (nth bits i)]))]
       [(assoc state
          :key-indices key-indices)
        [:verify {:verification verification
                  :key-indices key-indices}]]))

   ;; Step 5 just prints a success message.
   :success
   (fn [{:keys [key-indices bits]} _]
     (let [key-bits (map #(nth bits %) key-indices)]
       (println "Alice succeeds with: " key-bits)))
   :abort (constantly nil)})

(def bob-responses
  ;; Step 2 of the protocol -- Alice has just sent the qubits.
  {:qubits
   (fn [state qubits]
     (let [bitcount (count qubits)
           ;; Randomly pick how to measure the qubits
           bases (repeatedly bitcount #(rand-nth [:value :sign]))
           ;; Do the measurements
           bits (map (fn [qubit basis]
                       (case basis
                         :value
                         (case (observe qubit) 0 0, 1 1)
                         :sign
                         (case (observe-sign qubit) :+ 0, :- 1)))
                     qubits
                     bases)]
       ;; Send back to Alice the information about how we measured the
       ;; qubits
       [(assoc state
          :bits bits
          :bases bases)
        [:bases bases]]))

   ;; Step 4: Alice sends back the verification info, and the list of
   ;; which qubits to use for the key. We check the verification info
   ;; against the values we observed in step 2.
   :verify
   (fn [{:keys [bits], :as state} {:keys [verification key-indices]}]
     (let [error-count (apply + (for [[i v] verification
                                      :when (not= v (nth bits i))]
                                  1))]
       (if (pos? error-count)
         [state [:abort error-count]]
         (let [key-bits (map #(nth bits %) key-indices)]
           (println "Bob succeeds with: " key-bits)
           [state [:success nil]]))))})

(defn make-actor
  "Returns a stateful function that accepts messages and executes the responses."
  [responses init-state]
  (let [state (atom init-state)]
    (fn [[msg body]]
      (let [[new-state msg'] ((responses msg) @state body)]
        (reset! state new-state)
        msg'))))

;; The basic runner just passes messages back and forth
(defn run-without-evesdropping
  []
  (let [alice (make-actor alice-responses {:bitcount 100})
        bob (make-actor bob-responses {})]
    (loop [next-msg [:start nil]
           people (cycle [alice bob])]
      (println "Sending" (first next-msg))
      (if-let [resp ((first people) next-msg)]
        (recur resp (rest people))))))

(comment
  (run-without-evesdropping)
  ;; prints:
  ;;   Sending :start
  ;;   Sending :qubits
  ;;   Sending :bases
  ;;   Sending :verify
  ;;   Bob succeeds with:  (0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 0 0 1 1 1)
  ;;   Sending :success
  ;;   Alice succeeds with:  (0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 0 0 1 1 1)
  )

;; This is just like run-without-evesdropping except we observe the
;; qubits in transit
(defn run-with-evesdropping
  []
  (let [alice (make-actor alice-responses {:bitcount 100})
        bob (make-actor bob-responses {})]
    (loop [next-msg [:start nil]
           people (cycle [alice bob])]
      (println "Sending" (first next-msg))

      ;; Here we do the actualy evesdropping. We can observe all the
      ;; qubits, or just some. We could also be more creative about
      ;; whether to measure the sign or the value
      ;;
      ;; The fewer qubits you observe, the less of a chance Alice and
      ;; Bob will notice, but the less information you get.
      (when (= :qubits (first next-msg))
        (let [qubits (second next-msg)
              observed (doall (for [q (take 20 qubits)]
                                (observe q)))]
          (println "Evesdropped and observed" observed)))

      (if-let [resp ((first people) next-msg)]
        (recur resp (rest people))))))

(comment
  (run-with-evesdropping)
  ;; prints:
  ;;   Sending :start
  ;;   Sending :qubits
  ;;   Evesdropped and observed (0 1 1 0 1 0 1 0 1 0 1 0 1 1 1 1 1 1 0 0)
  ;;   Sending :bases
  ;;   Sending :verify
  ;;   Sending :abort
  )
