(ns active.random
  (:use clojure.math.numeric-tower))

; This is a purely functional random generator, based on Lennart
; Augustsson's generator shipped with Hugs.

; Its comment says:

; This implementation uses the Portable Combined Generator of L'Ecuyer
; ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
; Lennart Augustsson.  It has a period of roughly 2.30584e18.

(defrecord Random-generator [s1 s2])

(def min-bound (- (expt 2N 31)))
(def max-bound (- (expt 2N 31) 1))
(def int-range (- max-bound min-bound))

(defn make-random-generator
  [s]
  (if (neg? s)
    (make-random-generator (- s))
    (let [q (quot s 2147483562)
          s1 (rem s 2147483562)
          s2 (rem q 2147483398)]
      (Random-generator. (+ 1 s1) (+ 1 s2)))))

(defn random-generator-next
  [rg]
    (let [s1 (:s1 rg)
          s2 (:s2 rg)
          k (quot s1 53668)
	  k* (quot s2 52774)
          s1*  (- (* 40014 (- s1 (* k 53668)))
                  (* k 12211))
          s2* (- (* 40692 (- s2 (* k* 52774)))
                 (* k* 3791))
          s1** (if (neg? s1*)
                 (+ s1* 2147483563)
                 s1*)
          s2** (if (neg? s2*)
                 (+ s2* 2147483399)
                 s2*)
          z (- s1** s2**)
          z* (if (< z 1)
                (+ z 2147483562)
                z)]
    (list z* (Random-generator. s1** s2**))))

(defn random-generator-split
  [rg]
  (let [s1 (:s1 rg)
        s2 (:s2 rg)
        new-s1 (if (= s1 2147483562)
                 1
                 (+ s1 1))
        new-s2 (if (= s2 1)
                 2147483398
                 (- s2 1))
        [_ nrg] (random-generator-next rg)]
    (list (Random-generator. new-s1
                             (:s2 nrg))
          (Random-generator. (:s1 nrg)
                             new-s2))))

; The intervals are inclusive.

(declare ilogbase)

(defn random-integer 
  [rg low high]
  (let [low (bigint low)
        high (bigint high)
        b 2147483561
	k (+ (- high low) 1)]
    (loop [n (ilogbase b k)
           acc low
           rg rg]
      (if (zero? n)
	  [(+ low (mod acc k))
           rg]
          (let [[x rgn] (random-generator-next rg)]
            (recur (- n 1) (+ x (* acc b)) rgn))))))

(defn random-float
  [rg low high]
  (let [[x nrg] (random-integer rg min-bound max-bound)]
    (let [scaled-x (+ (/ (+ low high) 2)
                      (* (/ (- high low) int-range)
                         x))]
      (list scaled-x nrg))))

(defn- ilogbase
  [b i]
  (if (< i b)
      1
      (+ 1 (ilogbase b (quot i b)))))
