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
  (:require [active.clojure.record :refer :all])
  (:require [active.clojure.monad :refer :all :as monad])
  (:require [active.clojure.condition :as c])
  (:use clojure.math.numeric-tower)
  (:use [clojure.test :only [assert-expr do-report]]))

(defn lift->generator
  "Lift a function on values to generators."
  [func & gens]
  (monadic
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

; [lower, upper]
(defn choose-integer
  "Generator for integers within a range, bounds are inclusive."
  [lower upper]
  (monadic
    [rgen get-random-generator]
    (let [[n _] (random-integer rgen lower upper)])
    (monad/return n)))

(def choose-byte
  "Generator for bytes in [-128, 127]."
  (lift->generator
   byte
   (choose-integer Byte/MIN_VALUE Byte/MAX_VALUE)))
  
(def choose-unsigned-byte
  "Generator for bytes in [0, 255]."
  (lift->generator
   short
   (choose-integer 0 (- (expt 2 Byte/SIZE) 1))))
  
(def choose-short
  "Generator for shorts in [-32768, 32767]."
  (lift->generator
   short
   (choose-integer Short/MIN_VALUE Short/MAX_VALUE)))

(def choose-unsigned-short
  "Generator for bytes in [0, 65535]."
  (lift->generator
   int
   (choose-integer 0 (- (expt 2 Short/SIZE) 1))))

(def choose-int
  "Generator for ints in [-2147483648, 2147483647]."
  (lift->generator
   int
   (choose-integer Integer/MIN_VALUE Integer/MAX_VALUE)))

(def choose-unsigned-int
  "Generator for bytes in [0, 4294967295]."
  (lift->generator
   long
   (choose-integer 0 (- (expt 2 Integer/SIZE) 1))))

(def choose-long
  "Generator for longs in [-9223372036854775808, 9223372036854775807]."
  (lift->generator
   long
   (choose-integer Long/MIN_VALUE Long/MAX_VALUE)))

(def choose-unsigned-long
  "Generator for bytes in [0, 18446744073709551615]."
  (lift->generator
   bigint
   (choose-integer 0 (- (expt 2 Long/SIZE) 1))))

(defn choose-float
  "Generator for floats within a range, bounds are inclusive."
  [lower upper]
  (monadic
    [rgen get-random-generator]
    (let [[n _] (random-float rgen lower upper)])
    (monad/return n)))

(def choose-ascii-char
  "Generator for ASCII characters."
  (lift->generator char (choose-integer 0 127)))
     
(def choose-ascii-letter
  "Generator for ASCII alphabetic letters."
  (lift->generator #(get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" %)
                   (choose-integer 0 51)))

(def choose-printable-ascii-char
  "Generator for printable ASCII characters."
  (lift->generator char (choose-integer 32 127)))

(defn- choose-char-with-property
  [pred]
  (monadic
    [rgen get-random-generator]
;   ;; loop until proper char is found; otherwise we could build a
;   ;; map of all chars, but that's not that good either.
    (loop [rg rgen]
          (let [[i ngen] (random-integer rg 0 0xffff)
                c (char i)]
            (if (pred c)
             (monad/return c)
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
  (monadic
    [rgen get-random-generator]
    (let [[n _] (random-integer rgen
                                   (int lower) (int upper))])
    (monad/return (char n))))

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
        (free-return? m) (free-return-val m)
                
        (free-bind? m)
        (let [m1 (free-bind-monad m)
              cont (free-bind-cont m)
              [rgen1 rgen2] (random-generator-split rgen)]
          (cond
            (free-return? m1) (recur (cont (free-return-val m1)) size rgen)
            
            (free-bind? m1) (c/assertion-violation `run "nested bind; should not happen" m m1)
            
            (get-random-generator? m1) (recur (cont rgen2) size rgen1)
            
            (get-size? m1) (recur (cont size) size rgen)
           
            (with-size? m1)
            (let [size1 (with-size-size m1)
                  gen1 (with-size-generator m1)]
              (recur (cont (run gen1 size1 rgen1)) size rgen2))
            
            (variant? m1)
            (let [v (variant-v m1)
                  rgen rgen]
              (loop [v (+ 1 v)
                     rgen rgen]
                (if (zero? v)
                  (run (cont (run (variant-generator m1) size rgen)) size rgen)
                  (let [[_ rgen2] (random-generator-split rgen)]
                    (recur (- v 1) rgen2)))))
            
            (promote? m1)
            (recur
              (cont
                (let [func (promote-func m1)]
                  (fn [& vals]
                     (let [b (run (apply func vals) size rgen1)]
                       b))))
              size rgen2)))
                
        (get-random-generator? m) rgen
                
        (get-size? m) size
        
        (with-size? m)
        (let [size (with-size-size m)
              gen (with-size-generator m)]
          (recur gen size rgen))
                
        (variant? m)
        (let [v (variant-v m)
              rgen rgen]
          (loop [v (+ 1 v)
                 rgen rgen]
            (if (zero? v)
              (run (variant-generator m) size rgen)
              (let [[_ rgen2] (random-generator-split rgen)]
                (recur (- v 1) rgen2)))))
                
        (promote? m)
        (let [func (promote-func m)]
          (fn [& vals]
             (let [b (run (apply func vals) size rgen)]
               b)))
        ))]
      (run gen size nrgen))))

; (int -> (generator a)) -> (generator a)
(defn sized
  "Apply a size to a generator."
  [func]
  (monadic
    [size get-size]
    (func size)))

; (list a) -> (generator a)
(defn choose-one-of
  "Make a generator that yields one of a list of values."
  [lis]
  (lift->generator #(nth lis %)
    (choose-integer 0 (- (count lis) 1))))

; vector from the paper
; (generator a) int -> (generator (list a))
(defn choose-list
  "Generator for a list of values with size n."
  [el-gen n]
  (letfn [(recurse [n]
            (if (zero? n)
              (monad/return '())
              (monadic
                [val el-gen
                 rest (recurse (- n 1))]
                (monad/return (conj rest val)))))]
    (recurse n)))

; (generator char) int -> (generator string)
(defn choose-string
  "Generator for a string with size n."
  [char-gen n]
  (lift->generator #(apply str %) (choose-list char-gen n)))

(declare choose-mixed)
(defn choose-symbol
  "Generator for a symbol with size n+1."
  [n]
  (monadic
    [fst (choose-string choose-non-numeric-char 1)
     rst (choose-string (choose-mixed (list choose-alphanumeric-char
                                        (choose-one-of (seq "*+!-_?"))))
           n)]
    (monad/return (symbol (str fst rst)))))

(defn choose-keyword
  "Generator for a keyword with size n+1."
  [n]
  (monadic
    [s (choose-symbol n)]
    (monad/return (keyword s))))

(defn choose-vector
  "Generator for a vector with size n."
  [el-gen n]
  (lift->generator vec (choose-list el-gen n)))

(defn choose-byte-array
  "Generator for a byte array with size n."
  [n]
  (lift->generator byte-array (choose-list choose-byte n)))

(defn- map-of-tuples
  [tups]
  (reduce (fn [m [k v]] (assoc m k v)) {} tups))

(defn choose-map
  "Generator for a map with size n. The passed element generator must
  generate key-value pairs."
  [el-gen n]
  (lift->generator map-of-tuples (choose-list el-gen n)))

(defn choose-set
  "Generator for a set with size <= n"
  [el-gen n]
  (lift->generator set (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(defn choose-mixed
  "Generator that chooses from a sequence of generators."
  [gens]
  (monad/free-bind (choose-one-of gens) force)) ; ???

; (list (list int (generator a))) -> (generator a)
(declare pick)
(defn choose-with-frequencies
  "Generator that chooses from a sequence of (frequency generator) pairs."
  [lis]
  (monadic
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
  (make-arbitrary 
    generator ;; (generator a)
    transformer) ;; a (generator b) -> (generator b)
  arbitrary?
  [generator arbitrary-generator
   transformer arbitrary-transformer])

(defn coarbitrary
  [arb val gen]
  "Modify a generator depending on val parameter."
  ((arbitrary-transformer arb) val gen))

(def arbitrary-boolean
  "Arbitrary boolean."
  (make-arbitrary
    (choose-one-of '(true false))
    (fn [a gen]
      (variant (if a 0 1) gen))))

(def arbitrary-integer
  "Arbitrary integer."
  (make-arbitrary
    (sized
      (fn [n]
        (choose-integer (- n) n)))
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
        (choose-integer 0 n)))
    (fn [n gen]
      (variant n gen))))

(defn arbitrary-integer-from-to
  "Arbitrary integer from range."
  [from to]
  (make-arbitrary
    (sized
      (fn [n]
        (choose-integer from to)))
    (fn [n gen]
      (variant (- n from) gen))))

(defn- arbitrary-int-like
  [gen to-int]
  (make-arbitrary 
    gen
    (fn [v rgen]
      (variant (to-int v) rgen))))

(def arbitrary-byte
  "Arbitrary byte."
  (arbitrary-int-like choose-byte byte))

(def arbitrary-short
  "Arbitrary short."
  (arbitrary-int-like choose-short short))

(def arbitrary-int
  "Arbitrary int."
  (arbitrary-int-like choose-int int))

(def arbitrary-long
  "Arbitrary long."
  (arbitrary-int-like choose-long long))

(def arbitrary-unsigned-byte
  "Arbitrary unsigned byte."
  (arbitrary-int-like choose-unsigned-byte short))

(def arbitrary-unsigned-short
  "Arbitrary unsigned short."
  (arbitrary-int-like choose-unsigned-short int))

(def arbitrary-unsigned-int
  "Arbitrary unsigned int."
  (arbitrary-int-like choose-unsigned-int long))

(def arbitrary-unsigned-long
  "Arbitrary unsigned long."
  (arbitrary-int-like choose-unsigned-long bigint))

(def arbitrary-ascii-char
  "Arbitrary ASCII character."
  (arbitrary-int-like choose-ascii-char int))

(def arbitrary-ascii-letter
  "Arbitrary ASCII letter."
  (arbitrary-int-like choose-ascii-letter int))

(def arbitrary-printable-ascii-char
  "Arbitrary printable ASCII character."
  (arbitrary-int-like choose-printable-ascii-char int))

(def arbitrary-char
  "Arbitrary char."
  (arbitrary-int-like (sized
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
    (lift->generator make-rational
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-natural))
    (fn [^clojure.lang.Ratio r gen]
      (coarbitrary arbitrary-integer
        (.numerator r)
        (coarbitrary arbitrary-integer
          (.denominator r) gen)))))

(defn- fraction
  [a b c]
  (+ a
    (float (/ b
             (+ (abs c) 1)))))

(def arbitrary-float
  "Arbitrary float."
  (make-arbitrary
    (lift->generator fraction
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-integer))
    (fn [r gen]
      (let [^clojure.lang.Ratio fr (rationalize r)]
        (coarbitrary arbitrary-integer
          (.numerator fr)
          (coarbitrary arbitrary-integer
            (.denominator fr) gen))))))

(declare coerce->generator)

(defn arbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-arbitrary
    (choose-mixed (map #(delay (coerce->generator (force (second %))))
                    pred+arbitrary-promise-list))
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
    (choose-one-of vals)
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
    (apply lift->generator
      vector
      (map arbitrary-generator arbitrary-els))
    (fn [lis gen]
      (letfn [(recurse [arbitrary-els lis]
                (if (seq arbitrary-els)
                  ((arbitrary-transformer (first arbitrary-els))
                    (first lis)
                    (recurse (rest arbitrary-els)
                      (rest lis)))
                  gen))]
        (recurse arbitrary-els lis)))))

(defn arbitrary-record
  "Arbitrary record."
  [construct accessors & arbitrary-els]
  (make-arbitrary
    (apply lift->generator
      construct
      (map arbitrary-generator arbitrary-els))
    (fn [rec gen]
      (letfn [(recurse [arbitrary-els lis]
                (if (seq arbitrary-els)
                  ((arbitrary-transformer (first arbitrary-els))
                    (first lis)
                    (recurse (rest arbitrary-els) (rest lis)))
                  gen))]
        (recurse arbitrary-els
          (map #(% rec) accessors))))))

(defn arbitrary-sequence-like
  "Arbitrary sequence-like container."
  [choose-sequence sequence->list arbitrary-el]
  (make-arbitrary
    (sized
      (fn [n]
        (monad/free-bind (choose-integer 0 n)
          (fn [length]
            (choose-sequence (arbitrary-generator arbitrary-el) length)))))
    (fn [sequ gen]
      (letfn [(recurse [lis]
                (if (seq lis)
                  ((arbitrary-transformer arbitrary-el)
                    (first lis)
                    (variant 1 (recurse (rest lis))))
                  (variant 0 gen)))]
        (recurse (sequence->list sequ))))))

(defn arbitrary-list
  "Arbitrary list."
  [arbitrary-el]
  (arbitrary-sequence-like choose-list identity arbitrary-el))

(defn arbitrary-vector
  "Arbitrary vector."
  [arbitrary-el]
  (arbitrary-sequence-like choose-vector #(into () %) arbitrary-el))

(def arbitrary-byte-array
  "Arbitrary byte-array."
  (arbitrary-sequence-like (fn [_ n] (choose-byte-array n)) #(into () %) arbitrary-byte))

(defn arbitrary-map
  "Arbitrary map over the given arbitrary key and value."
  [arbitrary-key arbitrary-value]
  (arbitrary-sequence-like choose-map #(into () %) (arbitrary-tuple arbitrary-key arbitrary-value)))

(defn arbitrary-set
  "Arbitrary set."
  [arbitrary-el]
  (arbitrary-sequence-like choose-set #(into () %) arbitrary-el))

(def arbitrary-ascii-string
  "Arbitrary string of ASCII characters."
  (arbitrary-sequence-like choose-string #(into () %) arbitrary-ascii-char))

(def arbitrary-printable-ascii-string
  "Arbitrary string of printable ASCII characters."
  (arbitrary-sequence-like choose-string #(into () %) arbitrary-printable-ascii-char))

(def arbitrary-string
  "Arbitrary string."
  (arbitrary-sequence-like choose-string #(into () %) arbitrary-char))

(defn- arbitrary-symbol-like
  [choose]
  (make-arbitrary
    (sized (fn [n] (choose n)))
    (fn [v gen]
      (coarbitrary arbitrary-string (name v) gen))))

(def arbitrary-symbol
  "Arbitrary symbol."
  (arbitrary-symbol-like choose-symbol))

(def arbitrary-keyword
  "Arbitrary keyword."
  (arbitrary-symbol-like choose-keyword))

(defn arbitrary-function
  "Arbitrary function."
  [arbitrary-result & arbitrary-args]
  (let [arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)]
    (make-arbitrary
      (promote
        (fn [& args]
          ((arbitrary-transformer arbitrary-arg-tuple)
            args
            (arbitrary-generator arbitrary-result))))
      (fn [func gen]
        (monadic
          [args (arbitrary-generator arbitrary-arg-tuple)
           t
           ((arbitrary-transformer arbitrary-result)
             (apply func args)
             gen)]
          (monad/return t))))))

(define-record-type Property-type
  ^{:doc "QuickCheck property"}
  (make-property func arg-names args)
  property?
  [func property-func
   arg-names property-arg-names
   ;; (seq (union arbitrary generator))
   args property-args])

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

(defmethod expand-arbitrary :default [form]
  (throw (Exception. (str "invalid expand-arbitrary form: " form))))

(defmethod expand-arbitrary :function [form]
  (let [[before with] (split-with #(not= % '->) form)
        after (rest with)]
    (if (not= 1 (count after))
      (throw (Exception. (str "more than one codomain for expand-arbitrary function form: " form))))
    `(arbitrary-function ~(expand-arbitrary (first after)) ~@(map expand-arbitrary before))))

(defmethod expand-arbitrary 'boolean [form]
  `arbitrary-boolean)

(defmethod expand-arbitrary 'integer [form]
  `arbitrary-integer)

(defmethod expand-arbitrary 'byte [form]
  `arbitrary-byte)

(defmethod expand-arbitrary 'short [form]
  `arbitrary-short)

(defmethod expand-arbitrary 'int [form]
  `arbitrary-int)

(defmethod expand-arbitrary 'long [form]
  `arbitrary-long)

(defmethod expand-arbitrary 'unsigned-byte [form]
  `arbitrary-unsigned-byte)

(defmethod expand-arbitrary 'unsigned-short [form]
  `arbitrary-unsigned-short)

(defmethod expand-arbitrary 'unsigned-int [form]
  `arbitrary-unsigned-int)

(defmethod expand-arbitrary 'unsigned-long [form]
  `arbitrary-unsigned-long)

(defmethod expand-arbitrary 'natural [form]
  `arbitrary-natural)

(defmethod expand-arbitrary 'rational [form]
  `arbitrary-rational)

(defmethod expand-arbitrary 'float [form]
  `arbitrary-float)

(defmethod expand-arbitrary 'char [form]
  `arbitrary-char)

(defmethod expand-arbitrary 'ascii-char [form]
  `arbitrary-ascii-char)

(defmethod expand-arbitrary 'printable-ascii-char [form]
  `arbitrary-printable-ascii-char)

(defmethod expand-arbitrary 'string [form]
  `arbitrary-string)

(defmethod expand-arbitrary 'ascii-string [form]
  `arbitrary-ascii-string)

(defmethod expand-arbitrary 'printable-ascii-string [form]
  `arbitrary-printable-ascii-string)

(defmethod expand-arbitrary 'byte-array [form]
  `arbitrary-byte-array)

(defmethod expand-arbitrary 'symbol [form]
  `arbitrary-symbol)

(defmethod expand-arbitrary 'keyword [form]
  `arbitrary-keyword)

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

(defmethod expand-arbitrary '[integer-from-to] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-integer-from-to ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[one-of] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-one-of ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[tuple] [form]
  `(arbitrary-tuple ~@(map expand-arbitrary (rest form))))

(defmethod expand-arbitrary '[list] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-list ~(expand-arbitrary (nth form 1))))

(defmethod expand-arbitrary '[vector] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-vector ~(expand-arbitrary (nth form 1))))

(defmethod expand-arbitrary '[map] [form]
  (expand-has-arg-count form 2)
  `(arbitrary-map ~(expand-arbitrary (nth form 1))
     ~(expand-arbitrary (nth form 2))))

(defmethod expand-arbitrary '[set] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-set ~(expand-arbitrary (nth form 1))))

; (record cons (acc ...) arb ...)
(defmethod expand-arbitrary '[record] [form]
  (expand-has-arg-count form 2)
  (let [ops (nth form 2)]
    (when (odd? (count ops))
      (throw (Exception. "Even number of field operands to record.")))
    (let [pairs (partition 2 ops)]
      `(arbitrary-record ~(nth form 1) (list ~@(map first pairs))
         ~@(map expand-arbitrary (map second pairs))))))

; (mixed pred arb ...)
(defmethod expand-arbitrary '[mixed] [form]
  (expand-has-at-least-arg-count form 2)
  (when (even? (count form))
    (throw (Exception. "Odd number of operands to mixed.")))
  `(arbitrary-mixed (list ~@(map (fn [[pred arb]]
                                   `(list ~pred (delay ~(expand-arbitrary arb))))
                              (partition 2 (rest form))))))

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
(defn- result-add-arguments
  [res args]
  (assoc res :arguments-list
    (conj (check-result-arguments-list res) args)))

(def nothing
  (make-check-result nil [] []))

; A testable value is one of the following:
; - a Property object
; - a boolean
; - a Result record
; - a generator of a Result record
(declare for-all-with-names)

(defn- coerce->result-generator
  "Coerce an object to a result generator."
  [thing]
  (cond
    (instance? Property-type thing) (for-all-with-names (property-func thing) (property-arg-names thing)
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
    (monadic
      [args (monad/sequ (map coerce->generator args))
       res (coerce->result-generator (apply func args))]
      (monad/return (result-add-arguments res
                      (map #(conj % nil) args)))))

(defn for-all-with-names
  "Bind names to generated values, supplying informative names."
  [func arg-names args]
  (monadic
    [args (monad/sequ (map coerce->generator args))
     res (coerce->result-generator (apply func args))]
    (monad/return (result-add-arguments res (map list arg-names args)))))

(defmacro ==>
  "Create a property that only has to hold when its prerequisite holds."
  [?bool ?prop]
  `(if ~?bool
     ~?prop
     (monad/return nothing)))

(defn label
  "Label a testable value."
  [str testable]
  (monadic
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
            result (generate ((make-config-size config) ntest) rgen2 gen)]
        ((make-config-print-every config) ntest (check-result-arguments-list result))
        (case (check-result-ok result)
          nil (recur rgen1 ntest (+ 1 nfail) stamps)
          true (recur rgen1 (+ 1 ntest) nfail (conj stamps (check-result-stamp result)))
          false (list ntest stamps result))))))

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

(defmethod assert-expr 'quickcheck [msg form]
  ;; (is (quickcheck prop))
  ;; Asserts that the property passes the QuickCheck tests
  (let [prop (second form)]
    `(let [prop-sexpr# '~prop
           prop# ~prop
           [ntests# stamps# success#] (quickcheck-results prop#)]
       (case success#
         true (do-report {:type :pass, :message ~msg,
                          :expected prop-sexpr#})
         false (do-report {:type :fail, 
                           :message (str "Arguments exhausted after " ntests# " tries"),
                           :expected prop-sexpr#, :actual false})
         (do-report {:type :fail,
                     :message (str "falsifiable")
                     :expected prop-sexpr#
                     :actual (check-result-arguments-list success#)})))))