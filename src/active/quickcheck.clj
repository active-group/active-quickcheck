; Copyright (c) Active Group GmbH. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

; quickcheck.clj: QuickCheck clone for Clojure

(ns ^{:author "Michael Sperber, working from John Hughes's original Haskell version"
      :doc "A QuickCheck clone for Clojure."}
  active.quickcheck
  (:use active.random)
  (:require [active.clojure.record :refer [define-record-type]])
  (:require [active.clojure.monad :as monad])
  (:require [active.tree :as tree])
  (:require [active.generator-applicative :refer [integrated
                                                  combine-generators
                                                  combine-generators-curry]])
  (:require [active.shrink :as shrink])
  (:require [active.clojure.condition :as c])
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.test :as t])
  (:require [clojure.test.check.generators :as gen])
  (:use clojure.math.numeric-tower)
  (:use [clojure.test :only [assert-expr do-report]]))

(defn lift->generator
  "Lift a function on values to generators."
  [func & gens]
  (monad/monadic
    [vals (monad/sequ gens)]
    (monad/return (apply func vals))))
           
(define-record-type Get-random-generator-type
  ^{:doc "Get the random generator."}
  (make-get-random-generator)
  get-random-generator?
  [])
(def get-random-generator (make-get-random-generator))

(define-record-type Get-size-type
  ^{:doc "Get the size of the random generator."}
  (make-get-size)
  get-size?
  [])
(def get-size (make-get-size))


;; Basic generator combinators
;; ---------------------------

; TODO change all (tree/make-Tree n []) with proper shrink trees
; [lower, upper]
(defn choose-integer
  "Generator for integers within a range, bounds are inclusive."
  [lower upper]
  (monad/monadic
    [rgen get-random-generator]
    (let [[n _] (random-integer rgen lower upper)
          ;shrinkTowards the nearest number to zero within the range
          towards-num (cond
                        (neg? upper) upper
                        (neg? lower) 0
                        :else lower)])
    (monad/return (tree/unfold (partial shrink/shrink-towards towards-num) n))))

(def choose-byte
  "Generator for bytes in [-128, 127]."
  (combine-generators
   byte
   (choose-integer Byte/MIN_VALUE Byte/MAX_VALUE)))
  
(def choose-unsigned-byte
  "Generator for bytes in [0, 255]."
  (combine-generators
   short
   (choose-integer 0 (- (expt 2 Byte/SIZE) 1))))
  
(def choose-short
  "Generator for shorts in [-32768, 32767]."
  (combine-generators
   short
   (choose-integer Short/MIN_VALUE Short/MAX_VALUE)))

(def choose-unsigned-short
  "Generator for bytes in [0, 65535]."
  (combine-generators
   int
   (choose-integer 0 (- (expt 2 Short/SIZE) 1))))

(def choose-int
  "Generator for ints in [-2147483648, 2147483647]."
  (combine-generators
   int
   (choose-integer Integer/MIN_VALUE Integer/MAX_VALUE)))

(def choose-unsigned-int
  "Generator for bytes in [0, 4294967295]."
  (combine-generators
   long
   (choose-integer 0 (- (expt 2 Integer/SIZE) 1))))

(def choose-long
  "Generator for longs in [-9223372036854775808, 9223372036854775807]."
  (combine-generators
   long
   (choose-integer Long/MIN_VALUE Long/MAX_VALUE)))

(def choose-unsigned-long
  "Generator for bytes in [0, 18446744073709551615]."
  (combine-generators
   bigint
   (choose-integer 0 (- (expt 2 Long/SIZE) 1))))

(defn choose-float
  "Generator for floats within a range, bounds are inclusive."
  [lower upper]
  (monad/monadic
    [rgen get-random-generator]
    (let [[n _] (random-float rgen lower upper)])
    (monad/return (tree/make-Tree n []))))

(def choose-ascii-char
  "Generator for ASCII characters."
  (combine-generators char (choose-integer 0 127)))

(def choose-ascii-letter
  "Generator for ASCII alphabetic letters."
  (combine-generators #(get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" %)
                      (choose-integer 0 51)))

(def choose-printable-ascii-char
  "Generator for printable ASCII characters."
  (combine-generators char (choose-integer 32 127)))

(defn- choose-char-with-property
  [pred]
  (monad/monadic
    [rgen get-random-generator]
;   ;; loop until proper char is found; otherwise we could build a
;   ;; map of all chars, but that's not that good either.
    (loop [rg rgen]
          (let [[i ngen] (random-integer rg 0 0xffff)
                c (char i)]
            (if (pred c)
             (monad/return (tree/make-Tree c []))
             (recur ngen))))))

(def choose-non-numeric-char
  (letfn [(is-non-numeric? [c]
            (let [t (java.lang.Character/getType (int c))]
              (or (= t java.lang.Character/UPPERCASE_LETTER)
                  (= t java.lang.Character/LOWERCASE_LETTER))))]
    (choose-char-with-property is-non-numeric?)))

(def choose-alphanumeric-char
  (letfn [(is-alphanumeric? [c]
            (let [t (java.lang.Character/getType (int c))]
              (or (= t java.lang.Character/UPPERCASE_LETTER)
                  (= t java.lang.Character/LOWERCASE_LETTER)
                  (= t java.lang.Character/DECIMAL_DIGIT_NUMBER)
                  (= t java.lang.Character/LETTER_NUMBER)
                  (= t java.lang.Character/OTHER_NUMBER)))
            )]
    (choose-char-with-property is-alphanumeric?)))

(defn choose-char
  "Generator for chars within a range, bonds are inclusive."
  [lower upper]
  (combine-generators char (choose-integer (int lower) (int upper))))

; int (generator a) -> (generator a)
(define-record-type Variant-type
  ^{:doc "Make generator that transforms random number seed depending on v."}
  (make-variant v gen) ; int (generator a)
  variant?
  [v variant-v
   gen variant-generator])
(def variant make-variant)

; (vals -> (generator b)) -> (generator (vals -> b))
(define-record-type Promote-type
  ^{:doc "Promote a function to generators to a generator of functions."}
  (make-promote func) ; vals -> generator b
  promote?
  [func promote-func])
(def promote make-promote)

(define-record-type With-size-type
  ^{:doc "Make a generator with a specified size."}
  (make-with-size size generator)
  with-size?
  [size with-size-size
   generator with-size-generator])
(def resize make-with-size)
; int random-gen (generator a) -> a
(defn generate ; aka run
  "Extract a value from a generator, using size n and random generator rgen."
  [n rgen gen]
  (let [[size nrgen] (random-integer rgen 0 n)]
    (letfn [(run [m size rgen]
      (cond
        (monad/free-return? m) (monad/free-return-val m)
                
        (monad/free-bind? m)
        (let [m1 (monad/free-bind-monad m)
              cont (monad/free-bind-cont m)
              [rgen1 rgen2] (random-generator-split rgen)]
          (cond
            (monad/free-return? m1) (recur (cont (monad/free-return-val m1)) size rgen)
            
            (monad/free-bind? m1) (c/assertion-violation `run "nested bind; should not happen" m m1)
            
            (get-random-generator? m1) (recur (cont rgen2) size rgen1)
            
            (get-size? m1) (recur (cont size) size rgen)
           
            (with-size? m1)
            (let [size1 (with-size-size m1)
                  gen1 (with-size-generator m1)]
              (recur (cont (run gen1 size1 rgen1)) size rgen2))
            
            (variant? m1)
            (let [v (variant-v m1)
                  next-rgen (integer-variant v rgen)]
              (run (cont (run (variant-generator m1) size next-rgen)) size next-rgen))
            
            (promote? m1)
            (recur
              (cont
                (let [func (promote-func m1)]
                  (fn [& vals]
                     (let [b (run (apply func vals) size rgen1)]
                       b))))
              size rgen2)
            :else (assert false
                          (str "invalid generator: " (pr-str m1)))))
                
        (get-random-generator? m) rgen
                
        (get-size? m) size
        
        (with-size? m)
        (let [size (with-size-size m)
              gen (with-size-generator m)]
          (recur gen size rgen))
                
        (variant? m)
        (let [v (variant-v m)
              next-rgen (integer-variant v rgen)]
          (run (variant-generator m) size next-rgen))
                
        (promote? m)
        (let [func (promote-func m)]
          (fn [& vals]
             (let [b (run (apply func vals) size rgen)]
               b)))
        
      :else (assert false
                    (str "invalid gen: " (pr-str m)))))]
      (run gen size nrgen))))

; (int -> (generator a)) -> (generator a)
(defn sized
  "Apply a size to a generator."
  [func]
  (monad/monadic
    [size get-size]
    (func size)))

; (list a) -> (generator a)
; TODO does it make sense to shrink this like an integer?
(defn choose-one-of
  "Make a generator that yields one of a list of values."
  [lis]
  (combine-generators #(nth lis %)
    (choose-integer 0 (- (count lis) 1))))

; (list (gen a)) -> (gen a)
; TODO doesn't work with trees. Remove it?
(defn oneof
  "Haskell QuickCheck's oneof"
  [gs]
  (when (< (count gs) 1)
    (assert false "oneof used with empty list"))
  (monad/free-bind (choose-integer 0 (- (count gs) 1))
                   #(nth gs 1)))

; vector from the paper
; (generator a) int -> (generator (list a))
(defn choose-list
  "Generator for a list of values with size n."
  [el-gen n]
  (apply combine-generators list (repeat n el-gen)))

; (generator char) int -> (generator string)
(defn choose-string
  "Generator for a string with size n."
  [char-gen n]
  (combine-generators #(apply str %) (choose-list char-gen n)))

(declare choose-mixed)
; TODO make it work with trees
(defn choose-symbol
  "Generator for a symbol with size n+1."
  [n]
  (let
    [fst (choose-string choose-non-numeric-char 1)
     rst (choose-string (choose-mixed (list choose-alphanumeric-char
                                        (choose-one-of (seq "*+!-_?"))))
           n)]
    (combine-generators-curry (fn [f] (fn [r] (symbol (str f r)))) fst rst)))

(defn choose-keyword
  "Generator for a keyword with size n+1."
  [n]
  (monad/monadic
    [s (choose-symbol n)]
    (monad/return (tree/map-tree keyword s))))

(defn choose-vector
  "Generator for a vector with size n."
  [el-gen n]
  (combine-generators vec (choose-list el-gen n)))

(defn choose-byte-array
  "Generator for a byte array with size n."
  [n]
  (combine-generators byte-array (choose-list choose-byte n)))

(defn- map-of-tuples
  [tups]
  (reduce (fn [m [k v]] (assoc m k v)) {} tups))

; TODO map-of-tuples doesn't preserve length. Maybe change this
#_(map-of-tuples [['a 1] ['a 2]])

(defn choose-map
  "Generator for a map with size n. The passed element generator must
  generate key-value pairs."
  [el-gen n]
  (combine-generators map-of-tuples (choose-list el-gen n)))

(defn choose-set
  "Generator for a set with size <= n"
  [el-gen n]
  (combine-generators set (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(defn choose-mixed
  "Generator that chooses from a sequence of generators.
  This has no shrinking between the gens"
  [gens]
  (monad/monadic
   [n (choose-integer 0 (- (count gens) 1))]
   (force (nth gens (tree/tree-outcome n)))))
  ;(monad/free-bind (choose-one-of gens) force)) ; ???

; (list (list int (generator a))) -> (generator a)
(declare pick)
(defn choose-with-frequencies
  "Generator that chooses from a sequence of (frequency generator) pairs."
  [lis]
  (monad/monadic
    [n (choose-integer 1 (apply + (map first lis)))]
    (monad/return (pick n lis))))

(defn pick
  "Pick an element from a sequence of (frequency, generator) pairs."
  [n lis]
  (let [f (first lis)
        k (first f)]
    (if (<= n k)
      (second f)
      (recur (- n k) (rest lis)))))

(define-record-type Arbitrary-type
  ^{:doc "Generalization of generator, suitable for producing function generators."}
  (make-arbitrary generator)
  arbitrary?
  [generator arbitrary-generator])

(define-record-type Coarbitrary-type
  ^{:doc "Coarbitrary typeclass in original Haskell implementation"}
  (make-coarbitrary 
   coarbitrary) ;; a (generator b) -> (generator b)
  coarbitrary?
  [coarbitrary coarbitrary-coarbitrary])

(define-record-type Property-type
  ^{:doc "QuickCheck property"}
  (make-property func arg-names args)
  property?
  [func property-func
   arg-names property-arg-names
   ;; (seq (union arbitrary generator))
   args property-args])


;; Advanced generator combinators
;; ------------------------------

(defn such-that-maybe
  [gen pred]
  (letfn [(mytry [k n]
               (if (= 0 n)
                 (monad/monadic (monad/return nil))
                 (monad/monadic [x (resize (+ (* 2 k) n) gen)]
                          (if (pred (tree/tree-outcome x))
                            (monad/return (first (tree/filter-tree pred x)))
                            (mytry (+ k 1) (- n 1))))))]
    (sized (fn [n] (mytry 0 (max 1 n))))))


(defn such-that-generator
  [gen pred]
  (monad/monadic
   [x (such-that-maybe gen pred)]
   (if x
     (monad/return x)
     (sized (fn [n] (resize (+ n 1) (such-that-generator gen pred)))))))

(defn such-that
  "Takes a generator and a predicate and
  returns a new generator that satisfies
  the predicate."
  [arb pred]
  (let [gen (arbitrary-generator arb)
        newgen (such-that-generator gen pred)]
    (make-arbitrary newgen))) ;; TODO: write coarbitrary implementation

(defn generate-one-of
  "Randomly choose one of a list of given arbitraries"
  [arbs]
  (monad/free-bind (choose-one-of arbs)
                   arbitrary-generator))


;; Arbitraries
;; -----------

(def arbitrary-boolean
  "Arbitrary boolean."
  (make-arbitrary
    (choose-one-of '(true false))))

(def coarbitrary-boolean
  "Coarbitrary boolean"
  (make-coarbitrary
   (fn [a gen]
     (variant (if a 0 1) gen))))

(def arbitrary-integer
  "Arbitrary integer."
  (make-arbitrary
   (sized
    (fn [n]
      (let [hi (expt 4 n)
            lo (- hi)]
        (choose-integer lo hi))))))

(def coarbitrary-integer
  "Arbitrary integer."
  (make-coarbitrary
    (fn [n gen]
      (variant (if (>= n 0)
                 (* 2 n)
                 (+ (* 2 (- n)) 1))
        gen))))

(def arbitrary-natural
  "Arbitrary natural number."
  (make-arbitrary
   (sized
    (fn [n]
      (choose-integer 0 n)))))

(def coarbitrary-natural
  "Coarbitrary natural number"
  (fn [n gen]
      (variant n gen)))

(defn arbitrary-integer-from-to
  "Arbitrary integer from range."
  [from to]
  (make-arbitrary
   (sized
    (fn [n]
      (choose-integer from to)))))

(defn coarbitrary-integer-from-to
  "Coarbitrary integer from range."
  [from to]
  (fn [n gen]
      (variant (- n from) gen)))

(defn- arbitrary-int-like
  [gen to-int]
  (make-arbitrary 
   gen))

(defn- coarbitrary-int-like
  [gen to-int]
  (make-coarbitrary (fn [v rgen]
                      (variant (to-int v) rgen))))

(def arbitrary-byte
  "Arbitrary byte."
  (arbitrary-int-like choose-byte byte))

(def coarbitrary-byte
  "Coarbitrary byte."
  (coarbitrary-int-like choose-byte byte))

(def arbitrary-short
  "Arbitrary short."
  (arbitrary-int-like choose-short short))

(def coarbitrary-short
  "Coarbitrary short."
  (coarbitrary-int-like choose-short short))

(def arbitrary-int
  "Arbitrary int."
  (arbitrary-int-like choose-int int))

(def coarbitrary-int
  "Coarbitrary int."
  (coarbitrary-int-like choose-int int))

(def arbitrary-long
  "Arbitrary long."
  (arbitrary-int-like choose-long long))

(def coarbitrary-long
  "Coarbitrary long."
  (coarbitrary-int-like choose-long long))

(def arbitrary-unsigned-byte
  "Arbitrary unsigned byte."
  (arbitrary-int-like choose-unsigned-byte short))

(def coarbitrary-unsigned-byte
  "Coarbitrary unsigned byte."
  (coarbitrary-int-like choose-unsigned-byte short))

(def arbitrary-unsigned-short
  "Arbitrary unsigned short."
  (arbitrary-int-like choose-unsigned-short int))

(def coarbitrary-unsigned-short
  "Coarbitrary unsigned short."
  (coarbitrary-int-like choose-unsigned-short int))

(def arbitrary-unsigned-int
  "Arbitrary unsigned int."
  (arbitrary-int-like choose-unsigned-int long))

(def coarbitrary-unsigned-int
  "Coarbitrary unsigned int."
  (coarbitrary-int-like choose-unsigned-int long))

(def arbitrary-unsigned-long
  "Arbitrary unsigned long."
  (arbitrary-int-like choose-unsigned-long bigint))

(def coarbitrary-unsigned-long
  "Coarbitrary unsigned long."
  (coarbitrary-int-like choose-unsigned-long bigint))

(def arbitrary-ascii-char
  "Arbitrary ASCII character."
  (arbitrary-int-like choose-ascii-char int))

(def coarbitrary-ascii-char
  "Coarbitrary ASCII character."
  (coarbitrary-int-like choose-ascii-char int))

(def arbitrary-ascii-letter
  "Arbitrary ASCII letter."
  (arbitrary-int-like choose-ascii-letter int))

(def coarbitrary-ascii-letter
  "Coarbitrary ASCII letter."
  (coarbitrary-int-like choose-ascii-letter int))

(def arbitrary-printable-ascii-char
  "Arbitrary printable ASCII character."
  (arbitrary-int-like choose-printable-ascii-char int))

(def coarbitrary-printable-ascii-char
  "Coarbitrary printable ASCII character."
  (coarbitrary-int-like choose-printable-ascii-char int))

(def arbitrary-char
  "Arbitrary char."
  (arbitrary-int-like (sized
                       (fn [n]
                         (choose-char \u0000 (char (min n 0xffff)))))
                      int))

(def coarbitrary-char
  "Coarbitrary char."
  (coarbitrary-int-like (sized
                         (fn [n]
                           (choose-char \u0000 (char (min n 0xffff)))))
                        int))

(defn- make-rational
  [a b]
  (/ a
    (+ 1 b)))

(def arbitrary-rational
  "Arbitrary rational number."
  (make-arbitrary
    (combine-generators make-rational
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-natural))))

(def coarbitrary-rational
  "Coarbitrary rational number."
  (make-coarbitrary
   (fn [^clojure.lang.Ratio r gen]
     ((coarbitrary-coarbitrary coarbitrary-integer)
      (.numerator r)
      ((coarbitrary-coarbitrary coarbitrary-integer)
       (.denominator r) gen)))))

(defn- fraction
  [a b c]
  (+ a
    (float (/ b
             (+ (abs c) 1)))))

(def arbitrary-float
  "Arbitrary float."
  (make-arbitrary
   (combine-generators fraction
                              (arbitrary-generator arbitrary-integer)
                              (arbitrary-generator arbitrary-integer)
                              (arbitrary-generator arbitrary-integer))))

(def coarbitrary-float
  "Coarbitrary float."
  (make-coarbitrary
   (fn [r gen]
     (let [^clojure.lang.Ratio fr (rationalize r)]
       ((coarbitrary-coarbitrary coarbitrary-integer)
        (.numerator fr)
        ((coarbitrary-coarbitrary coarbitrary-integer)
         (.denominator fr) gen))))))

(declare coerce->generator)

(defn arbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-arbitrary
   (choose-mixed (map #(delay (coerce->generator (force (second %))))
                      pred+arbitrary-promise-list))))

(defn coarbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis pred+arbitrary-promise-list
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-mixed: value matches none of the predicates"))
          ((first (first lis)) val) (variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-one-of
  "Arbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-arbitrary
   (choose-one-of vals)))

(defn coarbitrary-one-of
  "Coarbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis vals
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-one-of: value matches none of the predicates"))
          (eql? (first lis) val) (variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-tuple
  "Arbitrary fixed-size vector."
  [& arbitrary-els]
  (make-arbitrary
    (apply combine-generators
      vector
      (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-tuple
  [& coarbitrary-els]
  (make-coarbitrary
   (fn [lis gen]
     (letfn [(recurse [coarbitrary-els lis]
               (if (seq coarbitrary-els)
                 ((coarbitrary-coarbitrary (first coarbitrary-els))
                  (first lis)
                  (recurse (rest coarbitrary-els)
                           (rest lis)))
                 gen))]
       (recurse coarbitrary-els lis)))))

(defn arbitrary-record
  "Arbitrary record."
  [construct accessors & arbitrary-els]
  (make-arbitrary
   (apply combine-generators
          construct
          (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-record
  "Coarbitrary record."
  [construct accessors & coarbitrary-els]
  (make-coarbitrary
    (fn [rec gen]
      (letfn [(recurse [coarbitrary-els lis]
                (if (seq coarbitrary-els)
                  ((coarbitrary-coarbitrary (first coarbitrary-els))
                    (first lis)
                    (recurse (rest coarbitrary-els) (rest lis)))
                  gen))]
        (recurse coarbitrary-els
          (map #(% rec) accessors))))))

(defn arbitrary-coll-of
  "Arbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  (let [opts (apply hash-map kwargs)
        {kind :kind, :or {kind 'clojure.core/vector?}} opts
        list->sequence (cond
                         (= kind 'clojure.core/vector?) vec
                         (= kind 'clojure.core/list?) #(into () %)
                         (= kind 'clojure.core/set?) set)
        {count :count} opts
        {min-count :min-count, :or {min-count 0}} opts
        {max-count :max-count} opts
        generator-el (arbitrary-generator arbitrary-el)]
    (make-arbitrary
     (sized
      (fn [n]
        (if count
          (choose-list (coerce->generator arbitrary-el) count)
          (monad/monadic
           [length-tree (choose-integer min-count (if max-count max-count n))]
           (let [length (tree/tree-outcome length-tree)])
           [list-of-trees (monad/sequ
                           (map coerce->generator(repeat length arbitrary-el)))]
           (monad/return
            (tree/map-tree list->sequence (shrink/sequence-shrink-list
                                           list-of-trees))))))))))

(defn coarbitrary-coll-of
  "Coarbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  :not-supported-yet)

(defn arbitrary-sequence-like
  "Arbitrary sequence-like container."
  [list->sequence arbitrary-el]
  (make-arbitrary
    (sized
      (fn [n]
        (monad/monadic
         [length-tree (choose-integer 0 n)]
         (let [length (tree/tree-outcome length-tree)])
         [list-of-trees (monad/sequ (map coerce->generator(repeat length arbitrary-el)))]
         (monad/return (tree/map-tree list->sequence (shrink/sequence-shrink-list list-of-trees))))))))

(defn coarbitrary-sequence-like
  "Coarbitrary sequence-like container."
  [choose-sequence sequence->list coarbitrary-el]
  (make-coarbitrary
    (fn [sequ gen]
      (letfn [(recurse [lis]
                (if (seq lis)
                  ((coarbitrary-coarbitrary coarbitrary-el)
                    (first lis)
                    (variant 1 (recurse (rest lis))))
                  (variant 0 gen)))]
        (recurse (sequence->list sequ))))))

(defn arbitrary-list
  "Arbitrary list."
  [arbitrary-el]
  (arbitrary-sequence-like #(into () %) arbitrary-el))

(defn coarbitrary-list
  "Coarbitrary list."
  [coarbitrary-el]
  (coarbitrary-sequence-like choose-list identity coarbitrary-el))

(defn arbitrary-vector
  "Arbitrary vector."
  [arbitrary-el]
  (arbitrary-sequence-like vec arbitrary-el))

(defn coarbitrary-vector
  "Coarbitrary vector."
  [coarbitrary-el]
  (coarbitrary-sequence-like choose-vector #(into () %) coarbitrary-el))

(def arbitrary-byte-array
  "Arbitrary byte-array."
  (arbitrary-sequence-like byte-array arbitrary-byte))

(def coarbitrary-byte-array
  "coarbitrary byte-array."
  (coarbitrary-sequence-like (fn [_ n] (choose-byte-array n)) #(into () %) coarbitrary-byte))

(defn arbitrary-map
  "Arbitrary map over the given arbitrary key and value."
  [arbitrary-key arbitrary-value]
  (arbitrary-sequence-like map-of-tuples (arbitrary-tuple arbitrary-key arbitrary-value)))

(defn coarbitrary-map
  "coarbitrary map over the given arbitrary key and value."
  [coarbitrary-key coarbitrary-value]
  (coarbitrary-sequence-like choose-map #(into () %) (coarbitrary-tuple coarbitrary-key coarbitrary-value)))

(defn arbitrary-set
  "Arbitrary set."
  [arbitrary-el]
  (arbitrary-sequence-like set arbitrary-el))

(defn coarbitrary-set
  "Coarbitrary set."
  [coarbitrary-el]
  (coarbitrary-sequence-like choose-set #(into () %) coarbitrary-el))

(def arbitrary-ascii-string
  "Arbitrary string of ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-ascii-char))

(def coarbitrary-ascii-string
  "Coarbitrary string of ASCII characters."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-ascii-char))

(def arbitrary-printable-ascii-string
  "Arbitrary string of printable ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-printable-ascii-char))

(def arbitrary-string
  "Arbitrary string."
  (arbitrary-sequence-like #(apply str %) arbitrary-char))

(def coarbitrary-string
  "Coarbitrary string."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-char))

(defn- arbitrary-symbol-like
  [choose]
  (make-arbitrary
   (sized (fn [n] (choose n)))))

(defn- coarbitrary-symbol-like
  [choose]
  (make-coarbitrary
    (fn [v gen]
      ((coarbitrary-coarbitrary coarbitrary-string) (name v) gen))))

(def arbitrary-symbol
  "Arbitrary symbol."
  (arbitrary-symbol-like choose-symbol))

(def coarbitrary-symbol
  "Coarbitrary symbol."
  (coarbitrary-symbol-like choose-symbol))

(def arbitrary-keyword
  "Arbitrary keyword."
  (arbitrary-symbol-like choose-keyword))

(def coarbitrary-keyword
  "Coarbitrary keyword."
  (coarbitrary-symbol-like choose-keyword))

(defn arbitrary-function
  "Arbitrary function."
  [arbitrary-result & coarbitrary-args]
  (let [coarbitrary-arg-tuple (apply coarbitrary-tuple coarbitrary-args)]
    (make-arbitrary
     (promote
      (fn [& args]
        ((coarbitrary-coarbitrary coarbitrary-arg-tuple)
         args
         (arbitrary-generator arbitrary-result)))))))

(defn coarbitrary-function
  "Coarbitrary function."
  [coarbitrary-result & arbitrary-args]
  (let [arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)]
    (make-coarbitrary
     (fn [func gen]
       (monad/monadic
        [args (arbitrary-generator arbitrary-arg-tuple)
         t
         ((coarbitrary-coarbitrary coarbitrary-result)
          (apply func args)
          gen)]
        (monad/return t))))))


;; spec->arbitrary
;; ---------------

(declare spec->arbitrary)
(declare spec->coarbitrary)

(defn and->arbitrary
  [a & args]
  (let [arb-a (spec->arbitrary a)
        myresolve (fn [f]
                    (if (symbol? f)
                      (resolve f)
                      (eval f)))
        pred (fn [val] (every? identity (map #((myresolve %) val) args)))]
    (such-that arb-a pred)))

(defn and->coarbitrary
  "Make a coarbitrary from a spec product"
  [a & args]
  (let [coarb-a (spec->coarbitrary a)]
    coarb-a))

(defn coll-of->arbitrary
  [a & kwargs]
  (apply arbitrary-coll-of (into [(spec->arbitrary a)] kwargs)))

(defn coll-of->coarbitrary
  [a & kwargs]
  (apply coarbitrary-coll-of (into [(spec->coarbitrary a)] kwargs)))

(defn map-of->arbitrary
  [ks vs]
  (arbitrary-map (spec->arbitrary ks)
                 (spec->arbitrary vs)))

(defn map-of->coarbitrary
  [ks vs]
  (coarbitrary-map (spec->coarbitrary ks)
                   (spec->coarbitrary vs)))

(defn or->arbitrary
  [& args]
  (let [parts (partition 2 args)
        _kws (map first parts) ;; Odd that we don't need them
        specs (map second parts)
        arbs (map spec->arbitrary specs)]
    (generate-one-of arbs)))

(defn or->coarbitrary
  [& args]
  :not-supported-yet)

(defn symbol->arbitrary
  [sym]
  (cond
    (= sym `integer?) arbitrary-integer
    (= sym `string?) arbitrary-string
    (= sym `keyword?) arbitrary-keyword))

(defn symbol->coarbitrary
  [sym]
  (cond
    (= sym `integer?) coarbitrary-integer
    (= sym `string?) coarbitrary-string
    (= sym `keyword?) coarbitrary-keyword))

(defn fn->arbitrary
  [fun]
  (cond
    (= fun integer?) arbitrary-integer
    (= fun string?) arbitrary-string
    (= fun keyword?) arbitrary-keyword))

(defn fn->coarbitrary
  [fun]
  (cond
    (= fun integer?) coarbitrary-integer
    (= fun string?) coarbitrary-string
    (= fun keyword?) coarbitrary-keyword))

(defn spec-op->arbitrary
  "Make an arbitrary from a spec op"
  [op args]
  (cond
    (= op `s/and) (apply and->arbitrary args)
    (= op `s/or) (apply or->arbitrary args)
    (= op `s/coll-of) (apply coll-of->arbitrary args)
    (= op `s/map-of) (apply map-of->arbitrary args)))

(defn spec-op->coarbitrary
  "Make a coarbitrary from a spec op"
  [op args]
  (cond
    (= op `s/and) (apply and->coarbitrary args)
    (= op `s/or) (apply or->coarbitrary args)
    (= op `s/coll-of) (apply coll-of->coarbitrary args)
    (= op `s/map-of) (apply map-of->coarbitrary args)))

(defn spec-form->arbitrary
  "Make an arbitrary from a s/formed spec"
  [form]
  (if (symbol? form)
    (symbol->arbitrary form)
    (let [op (first form)
          args (rest form)]
      (spec-op->arbitrary op args))))

(defn spec-form->coarbitrary
  "Make a coarbitrary from a s/formed spec"
  [form]
  (println (pr-str form))
  (if (symbol? form)
    (symbol->coarbitrary form)
    (let [op (first form)
          args (rest form)]
      (spec-op->coarbitrary op args))))

(defn set->arbitrary
  "Make an arbitrary from a set (behaviour like enum)"
  [s]
  (apply arbitrary-one-of (into [identity] s)))

(defn set->coarbitrary
  "Make a coarbitrary from a set (behaviour like enum)"
  [s]
  (apply coarbitrary-one-of (into [identity] s)))

(defn gen->arbitrary
  "Make a spec gen specification into an arbitrary."
  [gen]
  (make-arbitrary
   (monad/return (first (gen/sample gen)))))

(defn spec->arbitrary
  "Make an arbitrary from a clojure spec"
  [spec]
  (cond
    (s/spec? (s/get-spec spec))
    (try
      (gen->arbitrary (s/gen spec))
      (catch Exception e
        (spec-form->arbitrary (s/form spec))))

    (keyword? spec)
    (spec-form->arbitrary (s/form spec))

    (symbol? spec)
    (symbol->arbitrary spec)

    (t/function? spec)
    (fn->arbitrary spec)

    (set? spec)
    (set->arbitrary spec)

    (satisfies? s/Specize spec)
    (spec-form->arbitrary (s/form spec))

    :else
    (assert false "Unknown spec shape")))

(defn spec->coarbitrary
  "Make a coarbitrary from a clojure spec"
  [spec]
  (cond
    (keyword? spec)
    (spec-form->coarbitrary (s/form spec))

    (symbol? spec)
    (symbol->coarbitrary spec)

    (t/function? spec)
    (fn->coarbitrary spec)

    (set? spec)
    (set->coarbitrary spec)

    (satisfies? s/Specize spec)
    (spec-form->coarbitrary (s/form spec))

    :else
    (assert false "Unknown spec shape")))




;; Arbitrary and Coarbitrary multimethods
;; --------------------------------------

(defmulti expand-arbitrary
  "Multimethod to expand `arbitrary' forms.

Dispatches on the symbol for atomic arbitrary forms,
and on [op] for compound arbitrary forms, where op is
the operator."
  (fn [form]
    (cond
      (symbol? form) form
      (or (not (seq? form)) (not (seq form))) :default
      (some #(= '-> %) form) :function
      :else [(first form)])))

(defmulti expand-coarbitrary
  "Multimethod to expand `coarbitrary' forms.

  Dispatches on the symbol for atomic coarbitrary forms,
  and on [op] for compound coarbitrary forms, where op is
  the operator."
  (fn [form]
    (cond
      (symbol? form) form
      (or (not (seq? form)) (not (seq form))) :default
      (some #(= '-> %) form) :function
      :else [(first form)])))

(defmethod expand-arbitrary :default [form]
  (throw (Exception. (str "invalid expand-arbitrary form: " form))))

(defmethod expand-coarbitrary :default [form]
  (throw (Exception. (str "invalid expand-coarbitrary form: " form))))

(defmethod expand-arbitrary :function [form]
  (let [[before with] (split-with #(not= % '->) form)
        after (rest with)]
    (if (not= 1 (count after))
      (throw (Exception. (str "more than one codomain for expand-arbitrary function form: " form))))
    `(arbitrary-function ~(expand-arbitrary (first after)) ~@(map expand-coarbitrary before))))

(defmethod expand-coarbitrary :function [form]
  (let [[before with] (split-with #(not= % '->) form)
        after (rest with)]
    (if (not= 1 (count after))
      (throw (Exception. (str "more than one codomain for expand-coarbitrary function form: " form))))
    `(coarbitrary-function ~(expand-coarbitrary (first after)) ~@(map expand-arbitrary before))))

(defmethod expand-arbitrary 'boolean [form]
  `arbitrary-boolean)

(defmethod expand-coarbitrary 'boolean [form]
  `coarbitrary-boolean)

(defmethod expand-arbitrary 'integer [form]
  `arbitrary-integer)

(defmethod expand-coarbitrary 'integer [form]
  `coarbitrary-integer)

(defmethod expand-arbitrary 'byte [form]
  `arbitrary-byte)

(defmethod expand-coarbitrary 'byte [form]
  `coarbitrary-byte)

(defmethod expand-arbitrary 'short [form]
  `arbitrary-short)

(defmethod expand-coarbitrary 'short [form]
  `coarbitrary-short)

(defmethod expand-arbitrary 'int [form]
  `arbitrary-int)

(defmethod expand-coarbitrary 'int [form]
  `coarbitrary-int)

(defmethod expand-arbitrary 'long [form]
  `arbitrary-long)

(defmethod expand-coarbitrary 'long [form]
  `coarbitrary-long)

(defmethod expand-arbitrary 'unsigned-byte [form]
  `arbitrary-unsigned-byte)

(defmethod expand-coarbitrary 'unsigned-byte [form]
  `coarbitrary-unsigned-byte)

(defmethod expand-arbitrary 'unsigned-short [form]
  `arbitrary-unsigned-short)

(defmethod expand-coarbitrary 'unsigned-short [form]
  `coarbitrary-unsigned-short)

(defmethod expand-arbitrary 'unsigned-int [form]
  `arbitrary-unsigned-int)

(defmethod expand-coarbitrary 'unsigned-int [form]
  `coarbitrary-unsigned-int)

(defmethod expand-arbitrary 'unsigned-long [form]
  `arbitrary-unsigned-long)

(defmethod expand-coarbitrary 'unsigned-long [form]
  `coarbitrary-unsigned-long)

(defmethod expand-arbitrary 'natural [form]
  `arbitrary-natural)

(defmethod expand-coarbitrary 'natural [form]
  `coarbitrary-natural)

(defmethod expand-arbitrary 'rational [form]
  `arbitrary-rational)

(defmethod expand-coarbitrary 'rational [form]
  `coarbitrary-rational)

(defmethod expand-arbitrary 'float [form]
  `arbitrary-float)

(defmethod expand-coarbitrary 'float [form]
  `coarbitrary-float)

(defmethod expand-arbitrary 'char [form]
  `arbitrary-char)

(defmethod expand-coarbitrary 'char [form]
  `coarbitrary-char)

(defmethod expand-arbitrary 'ascii-char [form]
  `arbitrary-ascii-char)

(defmethod expand-coarbitrary 'ascii-char [form]
  `coarbitrary-ascii-char)

(defmethod expand-arbitrary 'printable-ascii-char [form]
  `arbitrary-printable-ascii-char)

(defmethod expand-coarbitrary 'printable-ascii-char [form]
  `coarbitrary-printable-ascii-char)

(defmethod expand-arbitrary 'string [form]
  `arbitrary-string)

(defmethod expand-coarbitrary 'string [form]
  `coarbitrary-string)

(defmethod expand-arbitrary 'ascii-string [form]
  `arbitrary-ascii-string)

(defmethod expand-coarbitrary 'ascii-string [form]
  `coarbitrary-ascii-string)

(defmethod expand-arbitrary 'printable-ascii-string [form]
  `arbitrary-printable-ascii-string)

(defmethod expand-coarbitrary 'printable-ascii-string [form]
  `coarbitrary-printable-ascii-string)

(defmethod expand-arbitrary 'byte-array [form]
  `arbitrary-byte-array)

(defmethod expand-coarbitrary 'byte-array [form]
  `coarbitrary-byte-array)

(defmethod expand-arbitrary 'symbol [form]
  `arbitrary-symbol)

(defmethod expand-coarbitrary 'symbol [form]
  `coarbitrary-symbol)

(defmethod expand-arbitrary 'keyword [form]
  `arbitrary-keyword)

(defmethod expand-coarbitrary 'keyword [form]
  `coarbitrary-keyword)

(defn- expand-has-arg-count
  [form n]
  (if (not= (- (count form) 1) n)
    (throw (Exception. (str "Form should have " n " arguments: " form)))))

(defn- expand-has-at-least-arg-count
  [form n]
  (if (< (- (count form) 1) n)
    (throw (Exception. (str "Form should have at least " n " arguments: " form)))))

(defmethod expand-arbitrary '[clojure.core/unquote] [form]
  (expand-has-arg-count form 1)
  (second form))

(defmethod expand-coarbitrary '[clojure.core/unquote] [form]
  (expand-has-arg-count form 1)
  (second form))

(defmethod expand-arbitrary '[integer-from-to] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-integer-from-to ~(second form) ~@(nthrest form 2)))

(defmethod expand-coarbitrary '[integer-from-to] [form]
  (expand-has-at-least-arg-count form 2)
  `(coarbitrary-integer-from-to ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[one-of] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-one-of ~(second form) ~@(nthrest form 2)))

(defmethod expand-coarbitrary '[one-of] [form]
  (expand-has-at-least-arg-count form 2)
  `(coarbitrary-one-of ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[tuple] [form]
  `(arbitrary-tuple ~@(map expand-arbitrary (rest form))))

(defmethod expand-coarbitrary '[tuple] [form]
  `(coarbitrary-tuple ~@(map expand-coarbitrary (rest form))))

(defmethod expand-arbitrary '[list] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-list ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[list] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-list ~(expand-coarbitrary (nth form 1))))

(defmethod expand-arbitrary '[vector] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-vector ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[vector] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-vector ~(expand-coarbitrary (nth form 1))))

(defmethod expand-arbitrary '[spec] [form]
  (expand-has-arg-count form 1)
  `(spec->arbitrary ~(nth form 1)))

(defmethod expand-coarbitrary '[spec] [form]
  (expand-has-arg-count form 1)
  `(spec->coarbitrary ~(nth form 1)))

(defmethod expand-arbitrary '[map] [form]
  (expand-has-arg-count form 2)
  `(arbitrary-map ~(expand-arbitrary (nth form 1))
     ~(expand-arbitrary (nth form 2))))

(defmethod expand-coarbitrary '[map] [form]
  (expand-has-arg-count form 2)
  `(coarbitrary-map ~(expand-coarbitrary (nth form 1))
     ~(expand-coarbitrary (nth form 2))))

(defmethod expand-arbitrary '[set] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-set ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[set] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-set ~(expand-coarbitrary (nth form 1))))

; (record cons (acc ...) arb ...)
(defmethod expand-arbitrary '[record] [form]
  (expand-has-arg-count form 2)
  (let [ops (nth form 2)]
    (when (odd? (count ops))
      (throw (Exception. "Even number of field operands to record.")))
    (let [pairs (partition 2 ops)]
      `(arbitrary-record ~(nth form 1) (list ~@(map first pairs))
         ~@(map expand-arbitrary (map second pairs))))))

(defmethod expand-coarbitrary '[record] [form]
  (expand-has-arg-count form 2)
  (let [ops (nth form 2)]
    (when (odd? (count ops))
      (throw (Exception. "Even number of field operands to record.")))
    (let [pairs (partition 2 ops)]
      `(coarbitrary-record ~(nth form 1) (list ~@(map first pairs))
         ~@(map expand-coarbitrary (map second pairs))))))

; (mixed pred arb ...)
(defmethod expand-arbitrary '[mixed] [form]
  (expand-has-at-least-arg-count form 2)
  (when (even? (count form))
    (throw (Exception. "Odd number of operands to mixed.")))
  `(arbitrary-mixed (list ~@(map (fn [[pred arb]]
                                   `(list ~pred (delay ~(expand-arbitrary arb))))
                              (partition 2 (rest form))))))

(defmethod expand-coarbitrary '[mixed] [form]
  (expand-has-at-least-arg-count form 2)
  (when (even? (count form))
    (throw (Exception. "Odd number of operands to mixed.")))
  `(coarbitrary-mixed (list ~@(map (fn [[pred arb]]
                                     `(list ~pred (delay ~(expand-coarbitrary arb))))
                                   (partition 2 (rest form))))))


;; ------

(defmacro coarbitrary
  [form]
  (expand-coarbitrary form))

(defmacro arbitrary
  "Convenient syntax for constructing arbitraries.

This is usually used implicitly via the property macro.

The argument form can be one of the following:

- boolean, integer, byte, short, int, long, 
  unsigned byte, unsigned-short, unsigned-int, unsigned-long
  natural, rational, float, char, ascii-char,
  printable-ascii-char, string, ascii-string, printable-ascii-string,
  byte-array, symbol, keyword
- (one-of <equality> <expr> ...)
- (tuple <arb> ...)
- (list <arb>)
- (vector <arb>)
- (set <arb>)
- (record <constructor> [<accessor> <arb> ...])
- (mixed <pred> <arb> <pred> <arb> ...)
- (map <arb1> <arb2>) ; map with keys from <arb1>, values from <arb2>
- ~<expr>, which evaluates <expr> as a regular expression

The syntax is extensible via the expand-arbitrary multimethod."
  [form]
  (expand-arbitrary form))

(defmacro property
  "Create a property through binding identifiers to arbitraries.

The clauses are a vector of alternating identifiers and arbitraries,
which are implicitly in the syntax understood by the arbitrary macro.

The body can use the identifiers, and should evaluate to a boolean
saying whether the property is satisfied."
  [clauses body0 & bodies]
  (when (odd? (count clauses))
    (throw (Exception. "Odd number of elements in property bindings.")))
  (let [pairs (partition 2 clauses)
        ids (map first pairs)
        rhss (map second pairs)]
    `(make-property
       (fn [~@ids]
         ~body0 ~@bodies)
       '~ids
       (list ~@(map (fn [rhs] `(arbitrary ~rhs)) rhss)))))

(define-record-type Check-result-type 
  ^{:doc "Result from a QuickCheck run."}
  (make-check-result ok stamp arguments-list)
  check-result?
  [
   ;; nil = unknown, true, false
   ok check-result-ok
   stamp check-result-stamp
   ;; (list (list (pair (union #f symbol) value)))
   arguments-list check-result-arguments-list])

(defn- result-add-stamp
  [res stamp]
  (assoc res :stamp (conj (check-result-stamp res) stamp)))

; result (list (pair (union #f symbol) value)) -> result
(defn result-add-arguments
  [res args]
  (assoc res :arguments-list
    (conj (check-result-arguments-list res) args)))

(defn result-mapped
  "Monoidal plus of result."
  [result1 result2]
  (assert (check-result? result1))
  (assert (check-result? result2))
  (cond
    (check-result-ok result1) result2
    :else result1))

(defn result-add-argument-if-empty
  [res arg]
  (assert (check-result? res))
  (cond
    (empty? (check-result-arguments-list res)) (result-add-arguments res arg)
    :else res))

(def nothing
  (make-check-result nil [] []))

; A testable value is one of the following:
; - a Property object
; - a boolean
; - a Result record
; - a generator of a Result record
(declare for-all-with-shrink-with-names)

(defn coerce->result-generator
  "Coerce an object to a result generator."
  [thing]
  (cond
    (instance? Property-type thing) (for-all-with-shrink-with-names (property-func thing)
                                                                    (property-arg-names thing)
                                                                    (property-args thing))
    (instance? Boolean thing) (monad/return (assoc nothing :ok thing))
    (instance? Check-result-type thing) (monad/return thing)
    :else thing )); 
  
(defn coerce->generator
  "Coerce an object to a generator."
  [thing]
  (if (instance? Arbitrary-type thing)
    (arbitrary-generator thing)
    thing))

(defn for-all
  "Bind names to generated values."
  [func & args]
    (monad/monadic
      [args (monad/sequ (map coerce->generator args))
       res (coerce->result-generator (apply func args))]
      (monad/return (result-add-arguments res
                      (map #(conj % nil) args)))))

(defn for-all-with-names
  "Bind names to generated values, supplying informative names."
  [func arg-names args]
  (monad/monadic
    [args (monad/sequ (map coerce->generator args))
     res (coerce->result-generator (apply func args))]
    (monad/return (result-add-arguments res (map list arg-names args)))))

(defn find-failing
  [smaller func]
  (monad/monadic
   ; TODO use apply
   [results (monad/sequ (mapv coerce->result-generator (mapv (partial apply func)
                                                                (mapv tree/tree-outcome smaller))))]
   (let [failingResults (filter (fn [[_ result]] (not (check-result-ok result)))
                                (mapv vector smaller results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults)))))

(defn shrinking
  "
  get shrinks of args and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result.
  "
  [arg-names args func fuel]
  (assert (tree/tree? args) "args has to be a tree")
  (let [children (tree/tree-shrinks args)]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc nothing :ok true))
       :else (monad/monadic
              (let [[shrunk, failure] maybeFailingResult])
              [result (shrinking arg-names shrunk func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       [(vector arg-names (tree/tree-outcome shrunk))])))))))


(defn for-all-with-shrink-with-names
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-names arg-trees]
  (assert (= (count arg-names) (count arg-trees))
          "Number of arg-names does not match number of arguments")
  (let [arg-trees (map coerce->generator arg-trees)]
    (monad/monadic
      [args-tree (apply combine-generators vector arg-trees)
      res (coerce->result-generator (apply func (tree/tree-outcome args-tree)))]
      (let [result (result-add-arguments res [(vector arg-names (tree/tree-outcome args-tree))])])
      [maybe-shrunken-result (cond
                               (check-result-ok result) (monad/return result)
                               :else (shrinking arg-names args-tree func 20))]
      (monad/return (result-mapped maybe-shrunken-result result)))))

(defmacro ==>
  "Create a property that only has to hold when its prerequisite holds."
  [?bool ?prop]
  `(if ~?bool
     ~?prop
     (monad/return nothing)))

(defn label
  "Label a testable value."
  [str testable]
  (monad/monadic
    [res (coerce->result-generator testable)]
    (monad/return (result-add-stamp res str))))

(defmacro classify
  "Classify some test cases of a testable."
  [?really? ?str ?testable]
  `(let [testable# ~?testable]
     (if ~?really?
       (label ~?str testable#)
       testable#)))

(defmacro trivial
  "Classify some test cases of a testable as trivial."
  [?really? ?testable]
  `(classify ~?really? "trivial" ~?testable))

(defn collect
  "Label a testable value with an the string representation of an object."
  [lbl testable]
  (label (str lbl) testable))

; Running the whole shebang

(define-record-type Config-type
  ^{:doc "Configuration for a series of QuickCheck test runs."}
  (make-config max-test max-fail size print-every)
  make-config?
  [max-test make-config-max-test
   max-fail make-config-max-fail
   size make-config-size
   print-every make-config-print-every])

(def quick
  "Quick test-run configuration with minimal output."
  (make-config
    100
    1000
    #(+ 3 (quot % 2))
    (fn [n args] nil)))

(def verbose
  "Quick test-run configuration with verbose output."
  (make-config
    100
    1000
    #(+ 3 (quot % 2))
    (fn [n args]
      (print n)
      (println ":")
      (doseq [x args] (println x)))))

(declare tests)

(defn check-results
  "Run a property against a configuration and return results."
  [config prop]
  (let [rgen (make-random-generator 0)]
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(declare report-result)
(defn check
  "Run a property against a configuration and report results."
  [config prop]
  (let [[ntest stamps maybe-result]  (check-results config prop)]
    (report-result ntest stamps maybe-result)))

(defn quickcheck-results
  "Run a property against the `quick' configuration and return results."
  [prop]
  (check-results quick prop))

(defn quickcheck
  "Run a property against the `quick' configuration and report results."
  [prop]
  (check quick prop))

(def ntries 20)

(defn counter-example-of-size [gen size rgen]
  (loop [i ntries
         rgen rgen]
    (when-not (zero? i)
      (let [result (generate size rgen gen)
            next-rgen (first (random-generator-split rgen))]
        (case (check-result-ok result)
          true (recur (dec i) next-rgen)
          false result
          )))))

(clojure.test/deftest counter-example-of-size-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (counter-example-of-size gen 1 rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

(defn smallest-counter-example [gen size example rgen]
  (loop [size (dec size)
         smallest-example example
         rgen (second (random-generator-split rgen))]

    (if (zero? size)
      smallest-example
      (let [new-example (counter-example-of-size gen size rgen)]
        (if new-example
          ;; go smaller
          (recur (dec size) new-example (second (random-generator-split rgen)))
          ;; done
          smallest-example
          )))))

(clojure.test/deftest smallest-counter-example-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (smallest-counter-example gen 50 :failed rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

(defn- tests
  "Run a series of test runs.

returns three values:
- ntest
- stamps
- true for success, false for exhausted, result for failure"
  [config gen rgen ntest nfail stamps]
  (loop [rgen rgen
         ntest ntest
         nfail nfail
         stamps stamps]
    (cond
      (= ntest (make-config-max-test config)) (list ntest stamps true)
      (= nfail (make-config-max-fail config)) (list nfail stamps false)
      :else
      (let [[rgen1 rgen2] (random-generator-split rgen)
            size ((make-config-size config) ntest)
            result (generate size rgen2 gen)]
        ((make-config-print-every config) ntest (check-result-arguments-list result))
        (case (check-result-ok result)
          nil (recur rgen1 ntest (+ 1 nfail) stamps)
          true (recur rgen1 (+ 1 ntest) nfail (conj stamps (check-result-stamp result)))
          false
          ;; found a counter-example of size size. we now try to
          ;; recursively find a smaller counter-example of size (dec
          ;; size)
          (let [smallest-result (smallest-counter-example gen size result rgen)]
            [ntest stamps smallest-result]))))))

(declare done write-arguments)

(defn- report-result
  "Report the result of a series of test runs."
  [ntest stamps maybe-result]
  (case maybe-result
    true (done "OK, passed" ntest stamps)
    false (done "Arguments exhausted after" ntest stamps)
    (do
      (print "Falsifiable, after ")
      (print ntest)
      (println " tests:")
      (doseq [a (check-result-arguments-list maybe-result)]
        (write-arguments a)))))

; (pair (union nil symbol) value)
(defn- write-argument
  "Print out an argument binding."
  [arg]
  (when (first arg)
    (print (first arg))
    (print " = "))
  (print (second arg)))

; (list (pair (union nil symbol) value))
(defn- write-arguments
  "Print out a list of argument bindings."
  [args]
  (when (seq args)
    (write-argument (first args))
    (doseq [arg (rest args)]
      (print " ")
      (write-argument arg))
    (newline)))

(declare group-sizes stamp<?)

(defn- done
  "Print out final report."
  [mesg ntest stamps]
  (print mesg)
  (print " ")
  (print ntest)
  (print " tests")
  (let [sorted (sort stamp<? (filter #(and (seq? %) (seq %)) stamps))
        grouped (group-sizes sorted)
        entries (map (fn [p]
                       (let [n (first p)
                             lis (rest p)]
                         (str (quot (* 100 n) ntest)
                           "% "
                           (clojure.string/join ", " lis))))
                  (sort (fn [p1 p2]
                          (> (first p1) (first p2)))
                    grouped))]
    (cond
      (not (seq entries)) (println ".")
      (not (seq (rest entries))) (do
                                   (print " (")
                                   (print (first entries))
                                   (println ")."))
      :else
      (do
        (println ".")
        (doseq [entry entries]
          (print entry)
          (println "."))))))


(defn- group-sizes
  "Compute class-group sizes."
  [lis]
  (if (not (seq lis))
    []
    (loop [current (first lis)
           size 1
           lis (rest lis)
           res []]
      (cond
        (not (seq lis)) (reverse (conj res (list size current)))
        (= current (first lis)) (recur current (+ 1 size) (rest lis) res)
        :else
        (recur (first lis) 1 (rest lis) (conj res (list size current)))))))

(defn- stamp<?
  "Compare two stamps."
  [s1 s2]
  (cond
    (not (seq s1)) (seq s2)
    (not (seq s2)) true
    :else
    (let [c (compare (first s1) (first s2))]
      (cond
        (< c 0) true
        (= c 0) (stamp<? (rest s1) (rest s2))
        :else false))))

(defn ^:no-doc quickcheck-report [msg prop-sexpr result]
  (let [[ntests stamps success] result]
    (case success
      true (do-report {:type :pass, :message msg,
                       :expected prop-sexpr})
      false (do-report {:type :fail, 
                        :message (str "Arguments exhausted after " ntests " tries"),
                        :expected prop-sexpr, :actual false})
      (do-report {:type :fail,
                  :message (str "falsifiable")
                  :expected prop-sexpr
                  :actual (check-result-arguments-list success)}))))

(defmethod assert-expr 'quickchecked [msg form]
  ;; (is (quickchecked prop))
  ;; Asserts that the property passes the QuickCheck tests
  (let [prop (second form)]
    `(quickcheck-report ~msg '~prop (quickcheck-results ~prop))))

(defmethod assert-expr 'quickcheck [msg form]
  ;; deprecated alias of quickchecked.
  (assert-expr msg (apply list 'quickchecked (rest form))))

;; --- Distribution tools ---------

(let [incc (fn [i]
             (if i (inc i) 1))]

  (defn ^:no-doc occurrences [stamps]
    (reduce (fn [acc labels]
              (reduce (fn [acc label]
                        (update acc label incc)) acc labels))
            {} stamps)))

(let [map-values (fn [f m]
                   (into {} (for [[k v] m] [k (f v)])))]

  (defn ^:no-doc distribution [stamps]
    (let [n (count stamps)]
      (map-values (fn [x] (/ x n))
                  (occurrences stamps)))))

(defn ^:no-doc fraction-of [label stamps]
  (let [d (distribution stamps)]
    (or (get d label) 0)))

(defn ^:no-doc distributed?
  "Check stamp distribution"
  [stamps required-distribution]
  (let [actual-distribution (distribution stamps)]
    (every? (fn [[label lower-bound]]
              (>= (get actual-distribution label) lower-bound))
            required-distribution)))

(defn ^:no-doc with-distribution-report [msg prop-sexpr pairs result]
  (let [[ntests stamps success] result
        d? (distributed? stamps pairs)]
    (if (and success (not d?))
      (do-report {:type :fail, :message "Distribution requirements not met",
                  :expected pairs
                  :actual (distribution stamps)})
      (quickcheck-report msg prop-sexpr result))))

(defmethod assert-expr 'with-distribution [msg form]
  ;; (is (with-distribution [label fraction ...] (quickcheck prop)))
  ;; Asserts that the property passes the QuickCheck tests
  ;; and meets the required label distribution
  (let [pairs (drop-last (drop 1 form))
        qcform (last form)]
    (let [prop (second qcform)]
      `(with-distribution-report ~msg '~prop (hash-map ~@pairs) (quickcheck-results ~prop)))))
