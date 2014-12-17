; Copyright (c) Michael Sperber. All rights reserved.
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
  (:use clojure.math.numeric-tower)
  (:use clojure.algo.monads)
  (:use [clojure.test :only [assert-expr do-report]]))

(defrecord
    ^{:doc "Generator monad for random values."}
    Generator
    ;; int(size) random-generator -> val
    [func])

(defn return
  "Monadic return for generators."
  [val]
  (Generator.
   (fn [size rgen]
     val)))

(defn bind
  "Monadic bind for generators."
  [m1 k]
  (let [func1 (:func m1)]
    (Generator.
     (fn [size rgen]
       (let [[rgen1 rgen2] (random-generator-split rgen)]
         (let [gen (k (func1 size rgen1))]
           ((:func gen) size rgen2)))))))

(defmonad 
  ^{:doc "Generator monad instance for clojure.algo.monads."}
  generator-m
  [m-result return
   m-bind bind])

(defn lift->generator
  "Lift a function on values to generators."
  [func & gens]
  (domonad generator-m
           [vals (m-seq gens)]
           (apply func vals)))
           
; [lower, upper]
(defn choose-integer
  "Generator for integers within a range, bounds are inclusive."
  [lower upper]
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-integer rgen lower upper)]
       n))))

(def choose-short
  "Generator for shorts in [-32768, 32767]."
  (choose-integer Short/MIN_VALUE Short/MAX_VALUE))

(def choose-long
  "Generator for shorts in [-2147483648, 2147483647]."
  (choose-integer Long/MIN_VALUE Long/MAX_VALUE))

(defn choose-float
  "Generator for floats within a range, bounds are inclusive."
  [lower upper]
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-float rgen lower upper)]
       n))))

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
  (Generator.
   (fn [size rgen]
     ;; loop until proper char is found; otherwise we could build a
     ;; map of all chars, but that's not that good either.
     (loop [rg rgen]
       (let [[i ngen] (random-integer rg 0 0xffff)
             c (char i)]
         (if (pred c)
           c
           (recur ngen)))))))

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
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-integer rgen
                                 (int lower) (int upper))]
       (char n)))))

; int (generator a) -> (generator a)
(defn variant
  "Make generator that transforms random number seed depending on v."
  [v gen]
  (let [func (:func gen)]
    (Generator.
     (fn [size rgen]
       (loop [v (+ 1 v)
              rgen rgen]
         (if (zero? v)
           (func size rgen)
           (let [[rgen1 rgen2] (random-generator-split rgen)]
             (recur (- v 1) rgen2))))))))

; int random-gen (generator a) -> a
(defn generate
  "Extract a value from a generator, using size n and random generator rgen."
  [n rgen gen]
  (let [[size nrgen] (random-integer rgen 0 n)]
    ((:func gen) size nrgen)))

; (vals -> (generator b)) -> (generator (vals -> b))
(defn promote
  "Promote a function to generators to a generator of functions."
  [func] 
  (Generator.
   (fn [size rgen]
     (fn [& vals]
       (let [g (apply func vals)]
         ((:func g) size rgen))))))

; (int -> (generator a)) -> (generator a)
(defn sized
  "Apply a size to a generator."
  [func]
  (Generator.
   (fn [size rgen]
     (let [g (func size)]
       ((:func g) size rgen)))))

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
              (return '())
              (domonad generator-m
                       [val el-gen
                        rest (recurse (- n 1))]
                       (conj rest val))))]
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
  (domonad generator-m
           [fst (choose-string choose-non-numeric-char 1)
            rst (choose-string (choose-mixed (list choose-alphanumeric-char
                                                   (choose-one-of (seq "*+!-_?"))))
                               n)]
           (symbol (str fst rst))))

(defn choose-keyword
  "Generator for a keyword with size n+1."
  [n]
  (domonad generator-m
           [s (choose-symbol n)]
           (keyword s)))

(defn choose-vector
  "Generator for a vector with size n."
  [el-gen n]
  (lift->generator vec (choose-list el-gen n)))

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
  (bind (choose-one-of gens) force))

; (list (list int (generator a))) -> (generator a)
(declare pick)
(defn choose-with-frequencies
  "Generator that chooses from a sequence of (frequency generator) pairs."
  [lis]
  (domonad generator-m
           [n (choose-integer 1 (apply + (map first lis)))]
           (pick n lis)))

(defn pick
  "Pick an element from a sequence of (frequency, generator) pairs."
  [n lis]
  (let [f (first lis)
        k (first f)]
    (if (<= n k)
      (second f)
      (recur (- n k) (rest lis)))))

(defrecord Arbitrary
    ^{:doc "Generalization of generator, suitable for producing function generators."}
    [;; (generator a)
     generator
     ;; a (generator b) -> (generator b)
     transformer])

(defn coarbitrary
  [arb val gen]
  "Modify a generator depending on val parameter."
  ((:transformer arb) val gen))

(def arbitrary-boolean
  "Arbitrary boolean."
  (Arbitrary. (choose-one-of '(true false))
              (fn [a gen]
                (variant (if a 0 1) gen))))

(def arbitrary-integer
  "Arbitrary integer."
  (Arbitrary. (sized
               (fn [n]
                 (choose-integer (- n) n)))
              (fn [n gen]
                (variant (if (>= n 0)
                           (* 2 n)
                           (+ (* 2 (- n)) 1))
                         gen))))

(def arbitrary-natural
  "Arbitrary natural number."
  (Arbitrary. (sized
               (fn [n]
                 (choose-integer 0 n)))
              (fn [n gen]
                (variant n gen))))

(defn- arbitrary-int-like
  [gen to-int]
  (Arbitrary. gen
              (fn [v rgen]
                (variant (to-int v) rgen))))

(def arbitrary-short
  "Arbitrary short."
  (arbitrary-int-like choose-short short))

(def arbitrary-long
  "Arbitrary long."
  (arbitrary-int-like choose-long long))

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
  (Arbitrary. (lift->generator make-rational
                               (:generator arbitrary-integer)
                               (:generator arbitrary-natural))
              (fn [r gen]
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
  (Arbitrary. (lift->generator fraction
                               (:generator arbitrary-integer)
                               (:generator arbitrary-integer)
                               (:generator arbitrary-integer))
              (fn [r gen]
                (let [fr (rationalize r)]
                  (coarbitrary arbitrary-integer
                               (.numerator fr)
                               (coarbitrary arbitrary-integer
                                            (.denominator fr) gen))))))

(declare coerce->generator)

(defn arbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (Arbitrary. (choose-mixed (map #(delay (coerce->generator (force (second %))))
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
  (Arbitrary. (choose-one-of vals)
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
  (Arbitrary. (apply lift->generator
                     vector
                     (map :generator arbitrary-els))
              (fn [lis gen]
                (letfn [(recurse [arbitrary-els lis]
                          (if (seq arbitrary-els)
                            ((:transformer (first arbitrary-els))
                             (first lis)
                             (recurse (rest arbitrary-els)
                                      (rest lis)))
                            gen))]
                  (recurse arbitrary-els lis)))))
                           
(defn arbitrary-record
  "Arbitrary record."
  [construct accessors & arbitrary-els]
  (Arbitrary. (apply lift->generator
                     construct
                     (map :generator arbitrary-els))
              (fn [rec gen]
                (letfn [(recurse [arbitrary-els lis]
                          (if (seq arbitrary-els)
                            ((:transformer (first arbitrary-els))
                             (first lis)
                             (recurse (rest arbitrary-els) (rest lis)))
                            gen))]
                  (recurse arbitrary-els
                           (map #(% rec) accessors))))))

(defn arbitrary-sequence-like
  "Arbitrary sequence-like container."
  [choose-sequence sequence->list arbitrary-el]
  (Arbitrary. (sized
               (fn [n]
                 (bind (choose-integer 0 n)
                       (fn [length]
                         (choose-sequence (:generator arbitrary-el) length)))))
              (fn [sequ gen]
                (letfn [(recurse [lis]
                          (if (seq lis)
                            ((:transformer arbitrary-el)
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
  (Arbitrary.
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
    (Arbitrary. (promote
                 (fn [& args]
                   ((:transformer arbitrary-arg-tuple)
                    args
                    (:generator arbitrary-result))))
                (fn [func gen]
                  (domonad generator-m
                           [args (:generator arbitrary-arg-tuple)
                            t
                            ((:transformer arbitrary-result)
                             (apply func args)
                             gen)]
                           t)))))

(defrecord ^{:doc "QuickCheck property"}
    Property
    [func
     arg-names
     ;; (seq (union arbitrary generator))
     args])

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

(defmethod expand-arbitrary 'short [form]
  `arbitrary-short)

(defmethod expand-arbitrary 'long [form]
  `arbitrary-long)

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

- boolean, integer, short, long, natural, rational, float, char, ascii-char,
  printable-ascii-char, string, ascii-string, printable-ascii-string,
  symbol, keyword
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
    `(Property. (fn [~@ids]
                  ~body0 ~@bodies)
                '~ids
                (list ~@(map (fn [rhs] `(arbitrary ~rhs)) rhss)))))

(defrecord ^{:doc "Result from a QuickCheck run."}
    Check-result
    [
     ;; nil = unknown, true, false
     ok
     stamp
     ;; (list (list (pair (union #f symbol) value)))
     arguments-list])

(defn- result-add-stamp
  [res stamp]
  (assoc res :stamp (conj (:stamp res) stamp)))

; result (list (pair (union #f symbol) value)) -> result
(defn- result-add-arguments
  [res args]
  (assoc res :arguments-list
         (conj (:arguments-list res) args)))

(def nothing
  (Check-result. nil [] []))


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
   (instance? Property thing) (for-all-with-names (:func thing) (:arg-names thing)
                                (:args thing))
   (instance? Boolean thing) (return (assoc nothing :ok thing))
   (instance? Check-result thing) (return thing)
   (instance? Generator thing) thing
   :else (throw (Error. (str "cannot be coerced to a result generator: " (.toString thing))))))

(defn- coerce->generator
  "Coerce an object to a generator."
  [thing]
  (cond
   (instance? Generator thing) thing
   (instance? Arbitrary thing) (:generator thing)
   :else (throw (Error. (str "cannot be coerced to a generator: " (.toString thing))))))


(defn for-all
  "Bind names to generated values."
  [func & args]
  (domonad generator-m
           [args (m-seq (map coerce->generator args))
            res (coerce->result-generator (apply func args))]
           (result-add-arguments res
                                 (map #(conj % nil) args))))

(defn for-all-with-names
  "Bind names to generated values, supplying informative names."
  [func arg-names args]
  (domonad generator-m
           [args (m-seq (map coerce->generator args))
            res (coerce->result-generator (apply func args))]
           (result-add-arguments res (map list arg-names args))))

(defmacro ==>
  "Create a property that only has to hold when its prerequisite holds."
  [?bool ?prop]
  `(if ~?bool
     ~?prop
     (return nothing)))

(defn label
  "Label a testable value."
  [str testable]
  (domonad generator-m
           [res (coerce->result-generator testable)]
           (result-add-stamp res str)))

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
  (label (.toString lbl) testable))
  
; Running the whole shebang

(defrecord ^{:doc "Configuration for a series of QuickCheck test runs."}
    Config
    [max-test max-fail size print-every])

(def quick
  "Quick test-run configuration with minimal output."
  (Config. 100
           1000
           #(+ 3 (quot % 2))
           (fn [n args] nil)))

(def verbose
  "Quick test-run configuration with verbose output."
  (Config. 100
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
     (= ntest (:max-test config)) (list ntest stamps true)
     (= nfail (:max-fail config)) (list nfail stamps false)
     :else
     (let [[rgen1 rgen2] (random-generator-split rgen)
           result (generate ((:size config) ntest) rgen2 gen)]
       ((:print-every config) ntest (:arguments-list result))
       (case (:ok result)
         nil (recur rgen1 ntest (+ 1 nfail) stamps)
         true (recur rgen1 (+ 1 ntest) nfail (conj stamps (:stamp result)))
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
      (doseq [a (:arguments-list maybe-result)]
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
                     :actual (:arguments-list success#)})))))
