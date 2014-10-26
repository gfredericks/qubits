(ns com.gfredericks.qubits.data
  "The logical underpinnings."
  (:require [com.gfredericks.z :as z]
            [com.gfredericks.z.impl :refer [IComplex]]))

(set! *warn-on-reflection* true)

(def ^:const TAU (* 2 Math/PI))

(defn amplitude->probability
  [c]
  (let [m (z/magnitude c)] (* m m)))

(defn system?
  "Checks that m looks roughly like a decent system map."
  [m]
  (= ::system (type m)))

(defn single-qubit-system
  "Given a qubit and a 0/1, returns a system map that consists of just
   that qubit in the |0> state or the |1> state."
  [q v]
  {:pre [(#{0 1} v)]}
  (with-meta
    {:qubits {q 0}
     :amplitudes {[v] z/ONE}}
    {:type ::system}))

(defn merge-systems
  "Given two system maps, returns a new map with the systems merged."
  [system1 system2]
  (let [system1-size (count (:qubits system1))
        qs (into (:qubits system1)
                 (for [[q idx] (:qubits system2)]
                   [q (+ idx system1-size)]))]
    (assert (apply distinct? (keys qs)) "Why do these systems already overlap?")
    (let [amplitudes
          (for [[vs amp] (:amplitudes system1)
                [vs' amp'] (:amplitudes system2)]
            [(into vs vs') (z/* amp amp')])]
      (with-meta
        {:qubits qs, :amplitudes (into {} amplitudes)}
        {:type ::system}))))

(defn vec-remove
  [v i]
  (cond (zero? i)
        (subvec v 1)

        (= i (dec (count v)))
        (pop v)

        :else
        (into (subvec v 0 i) (subvec v (inc i)))))

(defn factor-qubit-from-system
  "Given a system of at least two qubits, and one of the qubits from
   that system, returns a new system without that qubit. The given
   qubit must (currently) have only one possible value, so it can be
   assumed to be unentangled."
  [system q]
  (let [{:keys [qubits amplitudes]} system
        qi (qubits q)]
    (assert (> (count qubits) 1))
    ;; check that it has the same value in all cases
    (assert (apply = (map #(% qi) (keys amplitudes))))
    (let [amplitudes' (into {}
                            (for [[vals amp] amplitudes]
                              [(vec-remove vals qi) amp]))
          qubits' (into {}
                        (for [[q idx] qubits]
                          [q (cond-> idx (> idx qi) (dec))]))]
      (with-meta
        {:qubits     qubits'
         :amplitudes amplitudes'}
        {:type ::system}))))

(defn probabilities
  [system q]
  (let [{:keys [qubits amplitudes]} system
        i (qubits q)]
    (reduce
     (fn [ret [vals amp]]
       (update-in ret [(nth vals i)] +
                  (amplitude->probability amp)))
     {0 0, 1 0}
     amplitudes)))

(defn ^:private zeroish? [z] (< (z/magnitude z) 1e-10))

(defn apply-single-qubit-gate
  "Gate is in the form [[a b] [c d]]. Returns a new system map."
  [gate system q controls]
  {:post [(system? %)]}
  (let [{:keys [qubits amplitudes]} system
        qi (qubits q)
        controls-i (map qubits controls)

        new-amplitudes
        (->> (for [[vals amp] amplitudes
                   :let [control-vals (map vals controls-i)]]
               (if (every? #{1} control-vals)
                 (let [q-val (vals qi)
                       [amp0 amp1] (gate q-val)]
                   {(assoc vals qi 0) (z/* amp0 amp)
                    (assoc vals qi 1) (z/* amp1 amp)})
                 {vals amp}))
             (apply merge-with z/+)
             (remove (comp zeroish? val))
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
        qi (qubits qubit)
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
          [vals (z/* amp (-> normalizer Math/sqrt / z/real->z))])]
    [v (assoc system :amplitudes (into {} new-amplitudes))]))

(def single-qubit-gates
  (let [z0 z/ZERO
        z1 z/ONE
        zi z/I
        -zi (z/- zi)
        -z1 (z/- z1)
        inv-root2 (z/real->z (/ (Math/sqrt 2)))
        -inv-root2 (z/- inv-root2)]
    {:X [[z0 z1] [z1 z0]]
     :Y [[z0 zi] [-zi z0]]
     :Z [[z1 z0] [z0 -z1]]
     :S [[z1 z0] [z0 zi]]
     :T [[z1 z0] [z0 (z/polar->z 1 (/ TAU 8))]]
     :H [[inv-root2 inv-root2] [inv-root2 -inv-root2]]}))

(defn phase-gate
  [theta]
  (assoc-in (single-qubit-gates :Z) [1 1] (z/polar->z 1 theta)))

(defn deterministic-value
  "If q has a deterministic value in the system, return it (0 or 1);
  else return nil."
  [system q]
  (let [{:keys [qubits amplitudes]} system
        qi (qubits q)
        vals (->> amplitudes
                  keys
                  (map #(% qi)))]
    (if (apply = vals) (first vals))))
