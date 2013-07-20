(ns com.gfredericks.qubits.data
  "The logical underpinnings."
  (:require [com.gfredericks.qubits.complex :as c]))

;; it feels messy to need indexOf. Should we be using maps instead?
;;
;; For that matter should we be using the term "PureState" instead of
;; "system"?
(defn index-of
  [^clojure.lang.APersistentVector v x]
  (.indexOf v x))

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
  "Given a qubit and a 0/1, returns a system map that consists of just
   that qubit in the |0> state or the |1> state."
  [q v]
  {:pre [(#{0 1} v)]}
  {:qubits [q]
   :amplitudes {[v] c/ONE}})

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

(defn vec-remove
  [v i]
  (cond (zero? i)
        (subvec v 1)

        (= i (dec (count v)))
        (pop v)

        :else
        (into (subvec v 0 i) (rest (drop i v)))))

(defn factor-qubit-from-system
  "Given a system of at least two qubits, and one of the qubits from
   that system, returns a new system without that qubit. The given
   qubit must (currently) have only one possible value, so it can be
   assumed to be unentangled."
  [system q]
  (let [{:keys [qubits amplitudes]} system
        qi (index-of qubits q)]
    (assert (> (count qubits) 1))
    ;; check that it has the same value in all cases
    (assert (apply = (map #(% qi) (keys amplitudes))))
    (let [amplitudes' (into {}
                            (for [[vals amp] amplitudes]
                              [(vec-remove vals qi) amp]))]
      {:qubits (vec-remove qubits qi)
       :amplitudes amplitudes'})))

(defn probabilities
  [system q]
  (let [{:keys [qubits amplitudes]} system
        i (index-of qubits q)]
    (reduce
     (fn [ret [vals amp]]
       (update-in ret [(nth vals i)] +
                  (amplitude->probability amp)))
     {0 0, 1 0}
     amplitudes)))

(defn apply-single-qubit-gate
  "Gate is in the form [[a b] [c d]]. Returns a new system map."
  [gate system q controls]
  {:post [(system? %)]}
  (let [{:keys [qubits amplitudes]} system
        qi (index-of qubits q)
        controls-i (map #(index-of qubits %) controls)

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

(defn observe
  "Given a system map and one of the qubits in the system,
   chooses a measurement outcome according to the current
   probabilities, and returns [outcome new-system]."
  [system qubit]
  (let [{:keys [qubits amplitudes]} system
        qi (index-of qubits qubit)
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

(def single-qubit-gates
  (let [z0 c/ZERO
        z1 c/ONE
        zi c/I
        -zi (c/- zi)
        -z1 (c/- z1)
        inv-root2 (c/->real (/ (Math/sqrt 2)))
        -inv-root2 (c/- inv-root2)]
    {:X [[z0 z1] [z1 z0]]
     :Y [[z0 zi] [-zi z0]]
     :Z [[z1 z0] [z0 -z1]]
     :S [[z1 z0] [z0 zi]]
     :T [[z1 z0] [z0 (c/->PolarComplex 1 (/ c/TAU 8))]]
     :H [[inv-root2 inv-root2] [inv-root2 -inv-root2]]}))

(defn phase-gate
  [theta]
  (assoc-in (single-qubit-gates :Z) [1 1] (c/->PolarComplex 1 theta)))

(defn deterministic-value
  "If q has a deterministic value in the system, return it (0 or 1);
  else return nil."
  [system q]
  (let [{:keys [qubits amplitudes]} system
        qi (index-of qubits q)
        vals (->> amplitudes
                  keys
                  (map #(% qi)))]
    (if (apply = vals) (first vals))))
