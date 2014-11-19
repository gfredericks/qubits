(ns com.gfredericks.qubits.examples.bell-violations
  (:require [com.gfredericks.qubits.objects :refer :all]))

;;
;; CHSH game
;;
;; Following along clumsily with
;; http://blog.sigfpe.com/2010/11/beating-odds-with-entangled-qubits.html
;;

;;
;; The main idea is that Alice and Bob are playing a "game" where they
;; each receive a single bit (x for Alice and y for Bob) and are
;; expected to individually choose bits a (from Alice) and b (from
;; Bob), without communicating, and they win the game iff
;; (= (bit-and x y) (bit-xor a b)).
;;
;; Supposedly the best Alice and Bob can do classically (assuming x
;; and y are uniform and independent) is to win 75% of the time. They
;; can trivially do this by always outputting a=0 and b=0. People say
;; it is "easy" to prove that they can't do any better, even with
;; shared randomness, but I haven't tried.
;;

;; Here's the code for the classical game, showing a 75% win rate with
;; the zeros strategy:

(def classical-alice (constantly 0))
(def classical-bob   (constantly 0))

(defn play-classical
  []
  (let [x (rand-int 2)
        y (rand-int 2)]
    (= (bit-and x y)
       (bit-xor (classical-alice x) (classical-bob y)))))

(defn report
  [{t true, f false}]
  (format "%.3f%%" (double (* 100 (/ t (+ f t))))))

(comment
  (report (frequencies (repeatedly 100000 play-classical)))
  ;; => "75.076%"
  )

;;
;; The interesting part is that they can do better than 75% if they each
;; have half of a pair of entangled qubits. They each manipulate their
;; qubit in a particular way depending on the value of x/y, and then
;; observe the value of the qubit, outputting the result directly.
;;
;; Through some trigonometric magic that I don't understand, this can
;; give them a win rate of ~85%.
;;

(def TAU (* 2 Math/PI)) ; of course

;; Alice and Bob's strategies are the same except for the angles they
;; use in the phase gate.

(defn majority-vote
  [xs]
  (->> xs
       (frequencies)
       (apply max-key val)
       (key)))

(defn quantum-alice
  [qs x]
  (majority-vote
   (for [q qs]
     (observe
      (doto q
        S
        H
        (phase (case x
                 0 0
                 1 (/ TAU 4)))
        H
        S)))))

(defn quantum-bob
  [qs y]
  (majority-vote
   (for [q qs]
     (let [TAU8 (/ TAU 8)]
       (observe
        (doto q
          S
          H
          (phase (case y
                   0 TAU8
                   1 (- TAU8)))
          H
          S))))))

(defn bell-qubits
  "Returns a pair of entangled qubits, a superposition of [0 0] and [1 1]."
  []
  (qubits [x y]
          (H x)
          (X y x)
          [x y]))

(defn play-quantum
  []
  (let [pairs (repeatedly 100 bell-qubits)
        x (rand-int 2)
        y (rand-int 2)]
    (= (bit-and x y)
       (bit-xor (quantum-alice (map first pairs) x) (quantum-bob (map second pairs) y)))))

(comment
  (report (frequencies (repeatedly 1000 play-quantum)))
    => "76.300%"
  )



;;
;; Mermin-Peres Magic Square Game
;;
;; http://en.wikipedia.org/wiki/Quantum_pseudo-telepathy#The_Mermin-Peres_magic_square_game
;;

;;
;; I can't quite figure this one out. If anybody else could that'd be
;; cool.
;;

(defn alice
  [q1 q2 row-num]
  {:pre [(#{0 1 2} row-num)]}
  )

(defn bob
  [q1 q2 col-num]
  {:pre [(#{0 1 2} col-num)]}
  )
