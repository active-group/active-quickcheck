(ns deinprogramm.quickcheck
  (:use deinprogramm.random)
  (:use clojure.math.numeric-tower)
  (:use clojure.algo.monads)
  (:use [clojure.test :only [assert-expr do-report]]))

(defrecord Generator
    ;; int(size) random-generator -> val
    [proc])

(defn return
  [val]
  (Generator.
   (fn [size rgen]
     val)))

(defn bind
  [m1 k]
  (let [proc1 (:proc m1)]
    (Generator.
     (fn [size rgen]
       (let [[rgen1 rgen2] (random-generator-split rgen)]
         (let [gen (k (proc1 size rgen1))]
           ((:proc gen) size rgen2)))))))

(defmonad generator-m
  [m-result return
   m-bind bind])

(defn lift->generator
  [proc & gens]
  (domonad generator-m
           [vals (m-seq gens)]
           (apply proc vals)))
           
; [lower, upper]
(defn choose-integer
  [lower upper]
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-integer rgen lower upper)]
       n))))

(defn choose-real
  [lower upper]
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-real rgen lower upper)]
       n))))

(def choose-ascii-char
  (lift->generator char (choose-integer 0 127)))
     
(def choose-ascii-letter
  (lift->generator #(get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" %)
                   (choose-integer 0 51)))

(def choose-printable-ascii-char
  (lift->generator char (choose-integer 32 127)))

(defn choose-char
  [lower upper]
  (Generator.
   (fn [size rgen]
     (let [[n _] (random-integer rgen
                                 (int lower) (int upper))]
       (char n)))))

; int (generator a) -> (generator a)
(defn variant
  [v gen]
  (let [proc (:proc gen)]
    (Generator.
     (fn [size rgen]
       (loop [v (+ 1 v)
              rgen rgen]
         (if (zero? v)
           (proc size rgen)
           (let [[rgen1 rgen2] (random-generator-split rgen)]
             (recur (- v 1) rgen2))))))))

; int random-gen (generator a) -> a
(defn generate
  [n rgen gen]
  (let [[size nrgen] (random-integer rgen 0 n)]
    ((:proc gen) size nrgen)))

; (vals -> (generator b)) -> (generator (vals -> b))
(defn promote
  [proc]
  (Generator.
   (fn [size rgen]
     (fn [& vals]
       (let [g (apply proc vals)]
         ((:proc g) size rgen))))))

; (int -> (generator a)) -> (generator a)
(defn sized
  [proc]
  (Generator.
   (fn [size rgen]
     (let [g (proc size)]
       ((:proc g) size rgen)))))

; (list a) -> (generator a)
(defn choose-one-of
  [lis]
  (lift->generator #(get lis %)
                   (choose-integer 0 (- (count lis) 1))))

; vector from the paper
; (generator a) int -> (generator (list a))
(defn choose-list
  [el-gen n]
  (letfn [(recurse [n]
            (if (zero? n)
              (return '())
              (domonad generator-m
                       [val el-gen
                        rest (recurse (- n 1))]
                       (cons val rest))))]
    (recurse n)))

; (generator char) int -> (generator string)
(defn choose-string
  [char-gen n]
  (lift->generator #(apply str %) (choose-list char-gen n)))

(defn choose-symbol
  [char-gen n]
  (domonad generator-m
           [s (choose-string char-gen n)]
           (symbol s)))

(defn choose-vector
  [el-gen n]
  (lift->generator vec (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(defn choose-mixed
  [gens]
  (bind (choose-one-of gens) force))

; (list (list int (generator a))) -> (generator a)
(declare pick)
(defn choose-with-frequencies
  [lis]
  (domonad generator-m
           [n (choose-integer 1 (apply + (map first lis)))]
           (pick n lis)))

(defn pick
  [n lis]
  (let [f (first lis)
        k (first f)]
    (if (<= n k)
      (second f)
      (recur (- n k) (rest lis)))))

(defrecord Arbitrary
    [;; (generator a)
     generator
     ;; a (generator b) -> (generator b)
     transformer])

(defn coarbitrary
  [arb val gen]
  ((:transformer arb) val gen))

(def arbitrary-boolean
  (Arbitrary. (choose-one-of '(true false))
              (fn [a gen]
                (variant (if a 0 1) gen))))

(def arbitrary-integer
  (Arbitrary. (sized
               (fn [n]
                 (choose-integer (- n) n)))
              (fn [n gen]
                (variant (if (>= n 0)
                           (* 2 n)
                           (+ (* 2 (- n)) 1))
                         gen))))

(def arbitrary-natural
  (Arbitrary. (sized
               (fn [n]
                 (choose-integer 0 n)))
              (fn [n gen]
                (variant n gen))))

(def arbitrary-ascii-char
  (Arbitrary. choose-ascii-char
              (fn [ch gen]
                (variant (int ch) gen))))

(def arbitrary-ascii-letter
  (Arbitrary. choose-ascii-letter
              (fn [ch gen]
                (variant (int ch) gen))))

(def arbitrary-printable-ascii-char
  (Arbitrary. choose-printable-ascii-char
              (fn [ch gen]
                (variant (int ch) gen))))

(def arbitrary-char
  (Arbitrary. (sized
               (fn [n]
                 (choose-char \u0000 \uffff)))
              (fn [ch gen]
		    (variant (int ch) gen))))

(defn make-rational
  [a b]
  (/ a
     (+ 1 b)))

(def arbitrary-rational
  (Arbitrary. (lift->generator make-rational
                               (:generator arbitrary-integer)
                               (:generator arbitrary-natural))
              (fn [r gen]
                (coarbitrary arbitrary-integer
                             (.numerator r)
                             (coarbitrary arbitrary-integer
                                          (.denominator r) gen)))))

(defn fraction
  [a b c]
  (+ a
     (float (/ b
               (+ (abs c) 1)))))

(def arbitrary-real
  (Arbitrary. (lift->generator fraction
                               (:generator arbitrary-integer)
                               (:generator arbitrary-integer)
                               (:generator arbitrary-integer))
              (fn [r gen]
                (let [fr (rationalize r 1/1000)]
                  (coarbitrary arbitrary-integer
                               (.numerator fr)
                               (coarbitrary arbitrary-integer
                                            (.denominator fr) gen))))))

(defn arbitrary-mixed
  [pred+arbitrary-promise-list]
  (Arbitrary. (choose-mixed (map (fn [p]
                                   (delay (:generator (force (second p)))))
                                 pred+arbitrary-promise-list))
              (fn [val gen]
                (loop [lis pred+arbitrary-promise-list
                       n 0]
                   (cond
                    (not (seq lis)) (throw (Error. "arbitrary-mixed:value matches none of the predicates"))
                    ((first (first lis)) val) (variant n gen)
                    :else (recur (rest lis) (+ 1 n)))))))



(defn arbitrary-one-of
  [eql? & vals]
  (Arbitrary. (choose-one-of vals)
              (fn [val gen]
                (loop [lis vals
                       n 0]
                   (cond
                    (not (seq lis)) (throw (Error. "arbitrary-mixed:value matches none of the predicates"))
                    (eql? (first lis) val) (variant n gen)
                    :else (recur (rest lis) (+ 1 n)))))))

; a tuple is just a non-uniform sequence
(defn arbitrary-tuple
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
                           (map (fn [acccessor] (accessor rec)) accessors))))))

(defn arbitrary-sequence
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
  [arbitrary-el]
  (arbitrary-sequence choose-list identity arbitrary-el))

(defn arbitrary-vector
  [arbitrary-el]
  (arbitrary-sequence choose-vector #(into () %) arbitrary-el))

(def arbitary-ascii-string
  (arbitrary-sequence choose-string #(into () %) arbitrary-ascii-char))

(def arbitary-printable-ascii-string
  (arbitrary-sequence choose-string #(into () %) arbitrary-printable-ascii-char))

(def arbitrary-string
  (arbitrary-sequence choose-string #(into () %) arbitrary-char))

(def arbitrary-symbol
  (arbitrary-sequence choose-symbol
                      #(into () (str symbol))
		      arbitrary-ascii-letter))

(defn arbitrary-procedure
  [arbitrary-result % arbitrary-args]
  (let [arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)]
    (Arbitrary. (promote
                 (fn [& args]
                   ((:transformer arbitrary-arg-tuple)
                    args
                    (:generator arbitrary-result))))
                (fn [proc gen]
                  (domonad generator-m
                           [args (:generator arbitrary-arg-tuple)
                            t
                            ((:transformer arbitrary-result)
                             (apply proc args)
                             gen)]
                           t)))))

(defrecord Property
    [proc
     arg-names
     ;; (seq (union arbitrary generator))
     args])

(defmacro property
  [clauses body0 & bodies]
  (when (odd? (count clauses))
    (throw (Exception. "Odd number of elements in property bindings.")))
  (let [pairs (partition 2 clauses)
        ids (map first pairs)
        rhss (map second pairs)]
    `(Property. (fn [~@ids]
                  ~body0 ~@bodies)
                '~ids
                (list ~@rhss))))

(defrecord Check-result
    [
     ;; nil = unknown, true, false
     ok
     stamp
     ;; (list (list (pair (union #f symbol) value)))
     arguments-list])

(defn result-with-ok
  [res ok]
  (Check-result. ok
           (:stamp res)
           (:argument-list res)))

(defn result-add-stamp
  [res stamp]
  (Check-result. (:ok res)
           (conj stamp (:stamp res))
           (:arguments-list res)))

; result (list (pair (union #f symbol) value)) -> result
(defn result-add-arguments
  [res args]
  (Check-result. (:ok res)
           (:stamp res)
           (conj (:arguments-list res) args)))

(def nothing
  (Check-result. nil [] []))


; A testable value is one of the following:
; - a Property object
; - a boolean
; - a Result record
; - a generator of a Result record
(declare for-all-with-names)

(defn coerce->result-generator
  [thing]
  (cond
   (instance? Property thing) (for-all-with-names (:proc thing) (:arg-names thing)
                                (:args thing))
   (instance? Boolean thing) (return (result-with-ok nothing thing))
   (instance? Check-result thing) (return thing)
   (instance? Generator thing) thing
   :else (throw (Error. (str "cannot be coerced to a result generator: " (.toString thing))))))

(defn coerce->generator
  [thing]
  (cond
   (instance? Generator thing) thing
   (instance? Arbitrary thing) (:generator thing)
   :else (throw (Error. (str "cannot be coerced to a generator: " (.toString thing))))))


(defn for-all
  [proc & args]
  (domonad generator-m
           [args (m-seq (map coerce->generator args))
            res (coerce->result-generator (apply proc args))]
           (result-add-arguments res
                                 (map #(cons nil %) args))))

(defn for-all-with-names
  [proc arg-names args]
  (domonad generator-m
           [args (m-seq (map coerce->generator args))
            res (coerce->result-generator (apply proc args))]
           (result-add-arguments res (map list arg-names args))))

(defmacro ==>
  [?bool ?prop]
  `(if ~?bool
     ?prop
     (return nothing)))


(defn label
  [str testable]
  (domonad generator-m
           [res (coerce->result-generator testable)]
           (result-add-stamp res str)))


(defmacro classify
  [?really? ?str ?testable]
  `(let [testable# ~?testable]
     (if ~?really?
       (label ~?str testable#)
       testable#)))

(defmacro trivial
  [?really? ?testable]
  `(classify ~?really? "trivial" ~?testable))

(defn collect
  [lbl testable]
  (label (.toString lbl) testable))
  
; Running the whole shebang

(defrecord Config
    [max-test max-fail size print-every])

(def quick
  (Config. 100
           1000
           #(+ 3 (quot % 2))
           (fn [n args] nil)))

(def verbose
  (Config. 100
           1000
           #(+ 3 (quot % 2))
           (fn [n args]
             (print n)
             (println ":")
             (doseq [x args] (println x)))))

(declare tests)

(defn check-results
  [config prop]
  (let [rgen (make-random-generator 0)]
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(declare report-result)
(defn check
  [config prop]
  (let [[ntest stamps maybe-result]  (check-results config prop)]
    (report-result ntest stamps maybe-result)))

(defn quickcheck-results
  [prop]
  (check-results quick prop))

(defn quickcheck
  [prop]
  (check quick prop))

; returns three values:
; - ntest
; - stamps
; - true for success, false for exhausted, result for failure

(defn tests
  [config gen rgen ntest nfail stamps]
  (loop [rgen rgen
         ntest ntest
         nfail nfail
         stamps stamps]
    (cond
     (= ntest (:max-test config)) (list ntest stamps true)
     (= ntest (:max-fail config)) (list ntest stamps false)
     :else
     (let [[rgen1 rgen2] (random-generator-split rgen)
           result (generate ((:size config) ntest) rgen2 gen)]
       ((:print-every config) ntest (:arguments-list result))
       (case (:ok result)
         nil (recur rgen1 ntest (+ 1 nfail) stamps)
         true (recur rgen1 (+ 1 ntest) nfail (conj stamps (:stamp result)))
         false (list ntest stamps result))))))

(declare done write-arguments)

(defn report-result 
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
(defn write-argument
  [arg]
  (when (first arg)
    (print (first arg))
    (print " = "))
  (print (second arg)))

; (list (pair (union nil symbol) value))
(defn write-arguments
  [args]
  (when (seq args)
    (write-argument (first args))
    (doseq [arg (rest args)]
      (print " ")
      (write-argument arg))
    (newline)))

(declare group-sizes stamp<?)

(defn done
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


(defn group-sizes
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

(defn stamp<?
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
