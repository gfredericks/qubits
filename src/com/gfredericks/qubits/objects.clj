(ns com.gfredericks.qubits.objects
  "Qubits as Objects."
  (:require [com.gfredericks.qubits.complex :as c]))

;; pure quantum math stuff; probably its own ns eventually
;;
;; any references to qubits in this part of the code use them
;; only for equality checks, I think

(defn amplitude->probability
  [c]
  (let [m (c/mag c)] (* m m)))

(defn system?
  "Checks that m looks roughly like a decent system map."
  [m]
  (and (vector? (:qubits m))
       (map? (:amplitudes m))
       (every? (fn [[vals amp]]
                 (and (vector? vals)
                      (every? #{0 1} vals)
                      (satisfies? c/IComplex amp)))
               (:amplitudes m))))

(defn single-qubit-system
  "Given a qubit, returns a system map that consists of just that qubit
   in the |0> state."
  [q]
  {:qubits [q]
   :amplitudes {[0] c/ONE}})

(defn merge-systems
  "Given two system maps, returns a new map with the systems merged."
  [system1 system2]
  (let [qs (into (:qubits system1) (:qubits system2))]
    (assert (apply distinct? qs) "Why do these systems already overlap?")
    (let [amplitudes
          (for [[vs amp] (:amplitudes system1)
                [vs' amp'] (:amplitudes system2)]
            [(into vs vs') (c/* amp amp')])]
      {:qubits qs, :amplitudes (into {} amplitudes)})))

(defn apply-single-qubit-gate
  "Gate is in the form [[a b] [c d]]. Returns a new system map."
  [gate system q controls]
  {:post [(system? %)]}
  (let [{:keys [qubits amplitudes]} system
        qi (.indexOf qubits q)
        controls-i (map #(.indexOf qubits %) controls)

        new-amplitudes
        (->> (for [[vals amp] amplitudes
                   :let [control-vals (map vals controls-i)]]
               (if (every? #{1} control-vals)
                 (let [q-val (vals qi)
                       [amp0 amp1] (gate q-val)]
                   {(assoc vals qi 0) (c/* amp0 amp)
                    (assoc vals qi 1) (c/* amp1 amp)})
                 {vals amp}))
             (apply merge-with c/+)
             (remove (comp c/zeroish? val))
             (into {}))]
    (assoc system :amplitudes new-amplitudes)))

(defn ^:private weighted-choice
  "Given a sequence of [x w], chooses an x with probability
   governed by the weights w."
  [pairs]
  (let [total (apply + (map second pairs))
        z (rand total)]
    (loop [[[x w] & more] pairs, z z]
      (if (or (empty? more) (< z w))
        x
        (recur more (- z w))))))

(defn observe*
  "Given a system map and one of the qubits in the system,
   chooses a measurement outcome according to the current
   probabilities, and returns [outcome new-system]."
  [system qubit]
  (let [{:keys [qubits amplitudes]} system
        qi (.indexOf qubits qubit)
        vals (weighted-choice
              (for [[vals amp] amplitudes]
                [vals (amplitude->probability amp)]))
        v (vals qi)

        filtered-amps
        (filter (fn [[vals _]] (= v (vals qi))) amplitudes)

        normalizer (->> filtered-amps
                        (map second)
                        (map amplitude->probability)
                        (apply +))

        new-amplitudes
        (for [[vals amp] filtered-amps]
          [vals (c/* amp (-> normalizer Math/sqrt / c/->real))])]
    [v (assoc system :amplitudes (into {} new-amplitudes))]))


;; qubits as objects

(deftype Qubit [name system]
  Object
  (toString [this]
    (format "#<Qubit[%s]>" name)))

(defmethod print-method Qubit
  [q ^java.io.Writer w]
  (.write w (str q)))

(defn init-system
  "Initializes a qubit to 0 inside its own system."
  [^Qubit q]
  (let [system (single-qubit-system q)]
    (dosync
     (alter (.system q) (constantly system)))
    (set-validator! (.system q) system?)))

(defn qubit
  ([] (qubit (gensym "qubit-")))
  ([name']
     (doto (->Qubit (name name') (ref nil))
       (init-system))))

(defmacro qubits
  "Macro for creating new qubits with the same name as their local bindings.
   E.g.:

     (qubits [a b c]
       (some)
       (functions))

   creates three qubits named \"a\" \"b\" and \"c\", binds them to the locals
   a, b, and c, and executes the body."
  [name-vector & body]
  {:pre [(vector? name-vector)
         (every? symbol? name-vector)]}
  `(let [~@(mapcat (juxt identity (fn [name] `(qubit ~(str name))))
                   name-vector)]
     ~@body))

(defn probabilities
  "Returns a map like {0 p1, 1 p2}."
  [^Qubit q]
  (let [{:keys [qubits amplitudes] :as system} @(.system q)
        i (.indexOf qubits q)]
    (reduce
     (fn [ret [vals amp]]
       (update-in ret [(nth vals i)] +
                  (amplitude->probability amp)))
     {0 0, 1 0}
     amplitudes)))

(defn update-system-pointers!
  "Given a system-map, updates all the .system refs of the :qubits
   list to point to that map."
  [system]
  (doseq [q (:qubits system)]
    (alter (.system q) (constantly system))))

(defn merge-systems!
  "Updates the system properties of the qubits so that they are all
  together."
  [qs]
  (dosync
   (let [systems (distinct (map (fn [^Qubit q] (deref (.system q))) qs))]
     (when (> (count systems) 1)
       (let [system (reduce merge-systems systems)]
         (update-system-pointers! system))))))

(defn single-qubit-gate-fn
  "Given a gate definition [[a b] [c d]], returns a function that
   takes a primary qubit and optional control qubits and executes
   the gate on it."
  [gate]
  (fn [^Qubit q & controls]
    (dosync
     (when (seq controls)
       (merge-systems! (cons q controls)))
     (let [new-system (apply-single-qubit-gate gate @(.system q) q controls)]
       (update-system-pointers! new-system)))
    q))

(let [z0 c/ZERO
      z1 c/ONE
      zi c/I
      -zi (c/- zi)
      -z1 (c/- z1)
      inv-root2 (c/->real (/ (Math/sqrt 2)))
      -inv-root2 (c/- inv-root2)]
  (def X (single-qubit-gate-fn [[z0 z1] [z1 z0]]))
  (def Y (single-qubit-gate-fn [[z0 zi] [-zi z0]]))
  (def Z (single-qubit-gate-fn [[z1 z0] [z0 -z1]]))
  (def H (single-qubit-gate-fn [[inv-root2 inv-root2] [inv-root2 -inv-root2]])))

(defn observe
  "Returns 0 or 1."
  [q]
  ;; TODO: extract q from its system if it is entangled
  (dosync
   (let [[outcome new-system] (observe* @(.system q) q)]
     (update-system-pointers! new-system)
     outcome)))
