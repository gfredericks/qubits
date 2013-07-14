(ns com.gfredericks.qubits.complex
  (:refer-clojure :exclude [+ - *])
  (:require [clojure.core :as clj]))

(def PI Math/PI)
(def TAU (clj/* 2 PI))

(defprotocol IComplex
  (real [this])
  (imag [this])
  (mag  [this])
  (arg  [this])
  (add  [this other])
  (negate [this])
  (multiply [this other])
  (conjugate [this])) ;; not using division currently so I'll leave it off

(deftype RectComplex [r i]
  IComplex
  (real [this] r)
  (imag [this] i)
  (mag  [this] (Math/sqrt (clj/+ (clj/* r r) (clj/* i i))))
  ;; TODO: Check that this is right. Wikipedia has good pseudopsuedocode in
  ;; the Complex Number article
  (arg  [this]
    (if (zero? r)
      (if (pos? i) (/ PI 2) (clj/* PI 1.5))
      (let [theta (Math/atan (/ i r))]
        (if (neg? r)
          (clj/+ PI theta)
          theta))))
  (add  [this other] (RectComplex. (clj/+ r (real other)) (clj/+ i (imag other))))
  (negate [this] (RectComplex. (clj/- r) (clj/- i)))
  (multiply [this other]
    (let [r' (real other)
          i' (imag other)]
      (RectComplex. (clj/- (clj/* r r')
                           (clj/* i i'))
                    (clj/+ (clj/* r i')
                           (clj/* i r')))))
  (conjugate [this] (RectComplex. r (clj/- i)))
  Object
  (toString [this]
    (str "#<" r " + i" i ">"))
  ;; Screw hashCode
  (equals [this other]
    (and (satisfies? IComplex other)
         (= r (real other))
         (= i (imag other)))))

(deftype PolarComplex [m a]
  IComplex
  (real [this] (clj/* m (Math/cos a)))
  (imag [this] (clj/* m (Math/sin a)))
  (mag  [this] m)
  (arg  [this] a)
  (add  [this other]
    (add (RectComplex. (real this) (imag this)) other))
  (negate [this]
    (PolarComplex. m (rem (clj/+ a PI) TAU)))
  (multiply [this other]
    (PolarComplex. (clj/* m (mag other))
                   (clj/+ a (arg other))))
  (conjugate [this]
    (PolarComplex. m (rem (clj/- TAU a) TAU)))
  Object
  (toString [this]
    (str "#<" m " e^i(" a ")>"))
  ;; who cares about hashCode amirite
  (equals [this other]
    (and (satisfies? IComplex other)
         (= m (mag other))
         (= a (arg other)))))

(defmethod print-method RectComplex
  [z ^java.io.Writer w]
  (.write w (str z)))

(defmethod print-method PolarComplex
  [z ^java.io.Writer w]
  (.write w (str z)))

(def ZERO (RectComplex. 0 0))
(def ONE (RectComplex. 1 0))
(def I (RectComplex. 0 1))

(defn +
  ([] ZERO)
  ([x] x)
  ([x y] (add x y))
  ([x y z & more] (reduce + (list* x y z more))))

(defn -
  ([x] (negate x))
  ([x y & more] (+ x (- (apply + y more)))))

(defn *
  ([] ONE)
  ([x] x)
  ([x y] (multiply x y))
  ([x y z & more] (reduce * (list* x y z more))))

(defn ->real
  [x]
  (RectComplex. x 0))

(defn ->imag
  [x]
  (RectComplex. 0 x))

(defn zeroish?
  "Do we actually have to worry about this? I don't even know."
  [z]
  (< (mag z) 0.0000001))
