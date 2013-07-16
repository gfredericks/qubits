(ns com.gfredericks.qubits.objects
  "Qubits as Objects."
  (:require [com.gfredericks.qubits.data :as data]))

(deftype Qubit [name system]
  Object
  (toString [this]
    (format "#<Qubit-%s: %s>"
            name
            (str (or (deterministic-value this) \?)))))

(defmethod print-method Qubit
  [q ^java.io.Writer w]
  (.write w (str q)))

(defn init-system
  "Initializes a qubit to 0 inside its own system."
  [^Qubit q]
  (let [system (data/single-qubit-system q 0)]
    (dosync
     (alter (.system q) (constantly system)))
    (set-validator! (.system q) data/system?)))

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
                  (data/amplitude->probability amp)))
     {0 0, 1 0}
     amplitudes)))

(defn deterministic-value
  "Given a qubit, returns a 0 or a 1 if it has a deterministic value,
   or nil otherwise."
  [q]
  (data/deterministic-value @(.system q) q))

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
       (let [system (reduce data/merge-systems systems)]
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
     (let [new-system (data/apply-single-qubit-gate gate @(.system q) q controls)]
       (update-system-pointers! new-system)))
    q))

(let [g data/single-qubit-gates]
  (def X (single-qubit-gate-fn (g :X)))
  (def Y (single-qubit-gate-fn (g :Y)))
  (def Z (single-qubit-gate-fn (g :Z)))
  (def S (single-qubit-gate-fn (g :S)))
  (def T (single-qubit-gate-fn (g :T)))
  (def H (single-qubit-gate-fn (g :H))))

(defn observe
  "Returns 0 or 1."
  [q]
  (dosync
   (let [[outcome new-system] (data/observe @(.system q) q)]
     ;; if the qubit was previously entangled, detangle it
     (if (> (count (:qubits new-system)) 1)
       (let [new-system-1 (data/factor-qubit-from-system new-system q)
             new-system-2 (data/single-qubit-system q outcome)]
         (update-system-pointers! new-system-1)
         (update-system-pointers! new-system-2))
       (update-system-pointers! new-system))
     outcome)))
