(ns com.gfredericks.qubits.objects
  "Qubits as Objects."
  (:require [com.gfredericks.qubits.complex :as c]))

;; pure quantum math stuff; probably its own ns eventually

(defn amplitude->probability
  [c]
  (let [m (c/mag c)] (* m m)))

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
  [q]
  (let [system {:qubits [q]
                :amplitudes {[0] c/ONE}}]
    (dosync
     (alter (.system q) (constantly system)))))

(defn qubit
  ([] (qubit (gensym "qubit-")))
  ([name']
     (doto (->Qubit (name name') (ref nil))
       (init-system))))

(defn probabilities
  "Returns a map like {0 p1, 1 p2}."
  [q]
  (let [{:keys [qubits amplitudes]} @(.system q)
        i (.indexOf qubits q)]
    (reduce
     (fn [ret [vals amp]]
       (update-in ret [(nth vals i)] +
                  (amplitude->probability amp)))
     {0 0, 1 0}
     amplitudes)))

(defn merge-systems
  "Given two system maps, returns a new map with the systems merged."
  [system1 system2]
  (let [qs (concat (:qubits system1) (:qubits system2))]
    (assert (apply distinct? qs) "Why do these systems already overlap?")
    (let [amplitudes
          (for [[vs amp] (:amplitudes system1)
                [vs' amp'] (:amplitudes system2)]
            [(into vs vs') (c/* amp amp')])]
      {:qubits qs, :amplitudes (into {} amplitudes)})))

(defn merge-systems
  "Updates the system properties of the qubits so that they are all
  together."
  [qs]
  (dosync
   (let [systems (distinct (map #(deref (.system %)) qs))]
     (when (> (count systems) 1)
       (let [system (reduce merge-systems systems)]
         (doseq [q qs]
           (alter (.system q) (constantly system))))))))

(defn X
  [q & controls]
  (when (seq controls)
    (merge-systems (cons q controls)))

  )
