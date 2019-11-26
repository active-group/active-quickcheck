(ns active.quickcheck-test
  (:require [active.clojure.record-spec :refer [define-record-type]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [active.random :as random]
            [active.tree :as tree]
            [active.generator-applicative :refer [integrated]]
            [active.clojure.monad :as monad]
            [clojure.math.numeric-tower :as num])
  (:use active.quickcheck))
;; --- shrink tree tests -----

(def test-gen (random/make-random-generator 12))

(defn test-generate [m] (generate 5 test-gen m))

(deftest choose-int-works
  (testing "choose-int produces tree of ints"
    (is (tree/approx-valid-tree? 5 (test-generate choose-int)))
    (is (every? int? (take 100 (tree/to-list (test-generate choose-int)))))))

(deftest choose-byte-works
  (testing "choose-byte produces tree of byte"
    (is (tree/approx-valid-tree? 5 (test-generate choose-byte)))))

(deftest choose-ascii-char-works
  (testing "choose-ascii-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate choose-ascii-char)))
    (is (every? char? (take 100 (tree/to-list (test-generate choose-ascii-char)))))))

(deftest choose-non-numeric-char-works
  (testing "choose-non-numeric-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate choose-non-numeric-char)))
    (is (every? char? (take 100 (tree/to-list (test-generate choose-non-numeric-char)))))))

(deftest choose-char-works
  (testing "choose-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-char 10 24))))
    (is (every? char? (take 100 (tree/to-list (test-generate (choose-char 0 20))))))))

(deftest choose-list-works
  (testing "choose-list produce tree of lists"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-list choose-int 0))))
    (is (every? coll? (take 100 (tree/to-list (test-generate (choose-list choose-int 2))))))
    (is (every? (partial every? int?) (take 100 (tree/to-list (test-generate (choose-list choose-int 2))))))
    (is (tree/approx-valid-tree? 5 (test-generate (choose-list choose-int 4))))
    (is (every? coll? (take 100 (tree/to-list (test-generate (choose-list choose-int 3))))))))

(deftest choose-one-of-works
  (testing "choose-one-of produces tree of given stuff"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-one-of (seq "abc")))))
    (is (every? char? (take 100 (tree/to-list
                                 (test-generate (choose-one-of (seq "abc")))))))))

(nth (list (choose-char 0 1)) 0)

(deftest choose-mixed-works
  (testing "choose-mixed produces produces tree of mixed"
    (is (tree/approx-valid-tree? 5 (test-generate
                                     (choose-mixed (list choose-non-numeric-char
                                                         (choose-char 0 1))))))
    (is (every? char? (take 100 (tree/to-list
                                 (test-generate
                                   (choose-mixed (list choose-non-numeric-char
                                                        (choose-char 0 1))))))))))

(deftest choose-symbol-works
  (testing "choose-symbol produces tree of symbols"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-symbol 2))))
    (is (every? symbol? (take 100 (tree/to-list (test-generate (choose-symbol 2))))))))

(deftest arbitrary-sequence-like-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like list
                                                              choose-int)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-sequence-like list
                                                            choose-int)))))))
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              choose-int)))))
    (is (every? vector? (take 100 (tree/to-list
                                   (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              choose-int)))))))))

(deftest arbitrary-list-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-list choose-int)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-list choose-int)))))))
    (is (every? (partial every? int?) (take 100 (tree/to-list
                                                 (test-generate
                                                  (arbitrary-generator
                                                   (arbitrary-list choose-int)))))))))

(def t-0 (tree/lazy-tree [0] []))
(def t-12 (tree/lazy-tree [12] []))
(def t-5 (tree/lazy-tree [5] []))
(def t-3 (tree/lazy-tree [3] []))
(def t-1 (tree/lazy-tree [1] []))
(def t-13 (tree/lazy-tree [13] []))
(def t-14 (tree/lazy-tree [14] []))
(def t-2 (tree/lazy-tree [2] []))
(def t-4 (tree/lazy-tree [4] []))
(def t-6 (tree/lazy-tree [6] []))
(def t-7 (tree/lazy-tree [7] []))
(def t-0 (tree/lazy-tree [0] []))
(def t-8 (tree/lazy-tree [8] []))
(def t-9 (tree/lazy-tree [9] []))
(def tree-list (tree/lazy-tree ['(1 2 3)] []))

(deftest find-failing-finds-nothing
  (testing "if the vector of shrunks contains no counterexample find-failing returns :no-failing-result"
    (is (= :no-failing-result (test-generate (find-failing [] (partial < 13)))))
    (is (= :no-failing-result (test-generate (find-failing [t-12] (partial < 5)))))
    (is (= :no-failing-result (test-generate (find-failing [t-5 t-3 t-1] (partial < 0)))))
    (is (= :no-failing-result (test-generate (find-failing [t-12 t-13 t-14] (partial > 20)))))
    (is (= :no-failing-result (test-generate (find-failing [tree-list] list?))))))

(deftest find-failing-finds-result
  (testing "if the vector of shrunks contains at least one  counterexample it returns the first one"
    (is (= [t-1 (make-check-result false [] [])]
           (test-generate (find-failing [t-1] (partial = 3)))))
    (is (= [t-2 (make-check-result false [] [])]
           (test-generate (find-failing [t-3 t-4 t-2 t-8 t-2] (partial < 2)))))
    (is (= [tree-list (make-check-result false [] [])]
           (test-generate (find-failing [tree-list] (fn [x] (< (count x) 2))))))))

(defn is-counterexample
  [mresult]
  (not (check-result-ok (test-generate mresult))))

(defn get-counterexample
  [mresult]
  (second (first (first (check-result-arguments-list (test-generate mresult))))))

(defn numshrink
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(defn listshrink
  [xs]
  (if (empty? xs) [] [(rest xs)]))

(deftest for-all-with-shrink-with-name-counterexample
   (testing "for-all-with-shrink-with-name shrinks an counterexample"
     (is (= [1] (get-counterexample (for-all-with-shrink-with-names (partial = 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0] (get-counterexample (for-all-with-shrink-with-names (partial > 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [1] (get-counterexample (for-all-with-shrink-with-names (partial = 0)
                                                                     "x"
                                                                     [(integrated numshrink (monad/return 5))]))))
     (is (= [3] (get-counterexample (for-all-with-shrink-with-names (partial > 3)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0 1] (get-counterexample (for-all-with-shrink-with-names (partial =)
                                                                     ["x" "y"]
                                                                     [(integrated numshrink (monad/return 5))
                                                                      (integrated numshrink (monad/return 6))]))))
     (is (= [[3]] (get-counterexample
                   (for-all-with-shrink-with-names empty?
                                                   ["x" ]
                                                   [(integrated listshrink (monad/return [1 2 3]))]))))))

(defn numshrinkv [[x]] (map vector (numshrink x)))
(defn listshrinkv [[xs]] (map vector (listshrink xs)))

(deftest shrinking-gives-counterexample
  (testing "if shrinking gets an counterexample it returns an counterexample"
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [2]) (partial = 0) 20)))
    (is (is-counterexample (shrinking ["y"] (tree/unfold numshrinkv [7]) (partial > 3) 20)))
    (is (is-counterexample (shrinking ["z"] (tree/unfold numshrinkv [3]) (partial < 5) 15)))
    (is (is-counterexample (shrinking ["v"] (tree/unfold numshrinkv [4]) (partial = 5) 11)))
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [4]) (partial = 3) 11)))
    (is (is-counterexample (shrinking ["xs"] (tree/unfold listshrinkv [[1 2 3]]) empty? 11)))))


;; -- quickcheck tests --

(defn check-quick
  [prop]
  (let [[ntests stamps success] (quickcheck-results prop)]
    success))

(deftest ok
  (testing "trivial property"
    (is
     (quickcheck
      (property [x integer]
                (= x x))))
    (is
     (quickcheck
      (property [x char]
                (= x x))))))



(deftest ok-multible-arguments
  (testing "trivaial property with multible arguments"
    (is
     (quickcheck
      (property [x integer
                 y integer
                 z integer]
                (and (= x x ) (= y y) (= z z)))))))

(deftest unqotueq
  (testing "unquote syntax"
    (is
     (quickcheck
      (property [x ~arbitrary-integer]
                (= x x))))))

(deftest not-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x integer]
                      (not= x x)))))))

(deftest sometimes-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x integer]
                      (< x 5)))))))

(deftest ok-list
  (testing "trivial property for list"
    (is
     (quickcheck
      (property [xs (list integer)]
                (= xs xs))))))

(deftest reverse-distributes-over-concat
  (testing "reverse distributes over concat"
    (is
     (quickcheck
      (property [xs (list integer)
                 ys (list integer)]
                (= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

(deftest reverse-distributes-over-concat-broken
  (testing "reverse distributes over concat: broken"
    (is
     (not= true
           (check-quick
            (property [xs (list integer)
                       ys (list integer)]
                      (= (reverse (concat ys xs)) (concat (reverse ys) (reverse xs)))))))))

(deftest clojure-symbol
  (testing "clojure symbol"
    (is
     (quickcheck
      (property [x symbol]
                (symbol? x))))))

(deftest clojure-spec-simple
  (testing "clojure spec integer"
    (is
      (quickcheck
        (property [x (spec integer?)]
                  (s/valid? integer? x)))))
  (testing "clojure spec string"
    (is
      (quickcheck
        (property [x (spec string?)]
                  (s/valid? string? x)))))
  (testing "clojure spec keyword"
    (is
     (quickcheck
      (property [x (spec keyword?)]
                (s/valid? keyword? x))))))

(deftest clojure-spec-def
  (s/def ::deffed string?)
  (is
   (quickcheck
    (property [x (spec ::deffed)]
              (s/valid? ::deffed x)))))

(deftest clojure-spec-map-of
  (is
   (quickcheck
    (property [x (spec (s/map-of integer? string?))]
              (s/valid? (s/map-of integer? string?) x)))))

(deftest clojure-spec-coll-of
  (testing "coll-of integer?"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer?))]
                (s/valid? (s/coll-of integer?) x)))))
  (testing "coll-of integer? :kind vector?"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer? :kind vector?))]
                (s/valid? (s/coll-of integer? :kind vector?) x)))))
  (testing "coll-of integer? :kind list?"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer? :kind list?))]
                (s/valid? (s/coll-of integer? :kind list?) x)))))
  (testing "coll-of integer? :kind set?"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer? :kind set?))]
                (s/valid? (s/coll-of integer? :kind set?) x)))))
  (testing "coll-of integer? :count 23"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer? :count 23))]
                (s/valid? (s/coll-of integer? :count 23) x)))))
  (testing "coll-of integer? :min-count 23 :max-count 25"
    (is
     (quickcheck
      (property [x (spec (s/coll-of integer? :min-count 23 :max-count 25))]
                (s/valid? (s/coll-of integer? :min-count 23 :max-count 25) x)))))
  (testing "coll-of ::deffed"
    (s/def ::deffed string?)
    (is
     (quickcheck
      (property [x (spec (s/coll-of ::deffed))]
                (s/valid? (s/coll-of ::deffed) x))))))

(deftest clojure-spec-and-such-that
  (testing "and integer? even?"
    (is
     (quickcheck
      (property [x (spec (s/and integer? even?))]
                (and (integer? x) (even? x))))))
  (testing "and integer? even? >10"
    (is
     (quickcheck
      (property [x (spec (s/and integer? even? #(> % 10)))]
                (and (integer? x) (even? x) (> x 10)))))))

(deftest clojure-spec-or
  (s/def ::uno integer?)
  (s/def ::due string?)
  (is
    (quickcheck
      (property [x (spec (s/or :eins ::uno
                               :zwei ::due))]
                (or (integer? x) (string? x))))))

(deftest clojure-spec-set
  (is
   (quickcheck
    (property [x (spec #{:a :b :c})]
              (or (= x :a)
                  (= x :b)
                  (= x :c))))))


(deftest clojure-spec-recursive
  (s/def ::leaf string?)
  (s/def ::other (s/coll-of ::inner))
  (s/def ::inner (s/or :leaf ::leaf :other ::other))
  (is
    (quickcheck
      (property [x (spec ::inner)]
                (s/valid? ::inner x)))))

(deftest huge-integer
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x integer]
                      (< x (num/expt 2 40))))))))

(deftest natural
  (testing "arbitrary-natural generates only natural numbers"
    (is
     (quickcheck
      (property [x natural]
                (and (integer? x)
                     (>= x 0)))))))

(deftest rational
  (testing "arbitrary-rational generates only rational numbers"
    (is
     (quickcheck
      (property [x rational]
                (rational? x))))))

(deftest resizeq
  (testing "whether resize works"
    (is
     (quickcheck
      (property [x ~(resize 100 (coerce->generator arbitrary-natural))]
                (and (integer? x)
                     (>= x 0)
                     (<= x 100)))))))

(deftest floatq
  (testing "arbitrary-float generates only floats"
    (is
     (quickcheck
      (property [x float]
                (float? x))))))

(deftest charq
  (testing "arbitrary-char generates only chars"
    (is
     (quickcheck
      (property [x char]
                (char? x))))))

(deftest ascii-char
  (testing "arbitrary-ascii-char generates only ASCII chars."
    (is
     (quickcheck
      (property [x ascii-char]
                (and (char? x)
                     (< (int x) 128)))))))

(deftest byteq
  (testing "arbitrary-byte generates only shorts."
    (is
     (quickcheck
      (property [x byte]
                (and (integer? x)
                     (instance? java.lang.Byte x)
                     (>= x -128)
                     (<= x 127)))))))

(deftest unsigned-byteq
  (testing "arbitrary-unsigned-byte generates only unsigned bytes."
    (is
     (quickcheck
      (property [x unsigned-byte]
                (and (integer? x)
                     (instance? java.lang.Short x)
                     (>= x 0)
                     (<= x 255)))))))

(deftest shortq
  (testing "arbitrary-short generates only shorts."
    (is
     (quickcheck
      (property [x short]
                (and (integer? x)
                     (instance? java.lang.Short x)
                     (>= x -32768)
                     (<= x 32767)))))))

(deftest unsigned-shortq
  (testing "arbitrary-unsigned-short generates only unsigned shorts."
    (is
     (quickcheck
      (property [x unsigned-short]
                (and (integer? x)
                     (instance? java.lang.Integer x)
                     (>= x 0)
                     (<= x 65535)))))))

(deftest intq
  (testing "arbitrary-long generates only longs."
    (is
     (quickcheck
      (property [x int]
                (and (integer? x)
                     (int? x)
                     (>= x -2147483648)
                     (<= x 2147483647)))))))

(deftest unsigned-intq
  (testing "arbitrary-unsigned-int generates only unsigned ints."
    (is
     (quickcheck
      (property [x unsigned-int]
                (and (integer? x)
                     (int? x)
                     (>= x 0)
                     (<= x 4294967295)))))))

(deftest longq
  (testing "arbitrary-long generates only longs."
    (is
     (quickcheck
      (property [x long]
                (and (integer? x)
                     (instance? java.lang.Long x)
                     (>= x -9223372036854775808)
                     (<= x 9223372036854775807)))))))

(deftest unsigned-longq
  (testing "arbitrary-unsigned-long generates only unsigned longs."
    (is
     (quickcheck
      (property [x unsigned-long]
                (and (integer? x)
                     (>= x 0)
                     (<= x 18446744073709551615)))))))

(deftest integer-from-toq
  (testing "arbitrary-integer-from-to generates only longs."
    (is
     (quickcheck
      (property 
       [a integer
        b integer]
       (let [from (min a b)
             to (max a b)]
       (property
        [x (integer-from-to from to)]
        (and (integer? x)
             (>= x from)
             (<= x to)))))))))

(deftest stringq
  (testing "arbitrary-string generates only strings."
    (is
     (quickcheck
      (property [x string]
                (string? x))))))

(deftest byte-arrayq
  (testing "arbitrary-byte-array generates only byte arrays."
    ;; http://stackoverflow.com/questions/14796964/how-to-check-if-a-clojure-object-is-a-byte-array
    (let [check (type (byte-array []))]
      (is
       (quickcheck
        (property [x byte-array]
                  (instance? check x)))))))

(deftest symbolq
  (testing "arbitrary-symbol generates only symbols."
    (is
     (quickcheck
      (property [x symbol]
                (symbol? x))))))

(defn valid-symbol-name?
  [name]
  ;; read-string knows the rules
  (try
    (= (keyword name) (read-string (str ":" name)))
    (catch RuntimeException e
      ;; EOF while reading, if not a proper s-expr
      false)))

(deftest symbol-valid
  (testing "arbitrary-symbol generates valid symbols."
    (is
     (quickcheck
      (property [x symbol]
                (and (symbol? x) (valid-symbol-name? (name x))))))))

(deftest keywordq
  (testing "arbitrary-keyword generates only keywords."
    (is
     (quickcheck
      (property [x keyword]
                (keyword? x))))))

(deftest keyword-valid
  (testing "arbitrary-keyword generates valid keywords."
    (is
     (quickcheck
      (property [x keyword]
                ;; same rules as for symbols..
                (and (keyword? x) (valid-symbol-name? (name x))))))))

(deftest ascii-string
  (testing "arbitrary-ascii-string generates only ASCII strings"
    (is
     (quickcheck
      (property [x ascii-string]
                (and (string? x)
                     (every? #(< (int %) 128) x)))))))

(deftest mixed
  (testing "arbitrary-mixed works"
    (is
     (quickcheck
      (property [x (mixed integer? integer
                          string? string)]
                (or (integer? x) (string? x)))))))

(deftest one-of
  (testing "arbitrary-one-of works"
    (is
     (quickcheck
      (property [x (one-of = "foo" "bar" "baz")]
                (contains? #{"foo" "bar" "baz"} x))))))

(deftest listq
  (testing "arbitrary-list works"
    (is
     (quickcheck
      (property [x (list integer)]
                (and (list? x)
                     (every? integer? x)))))))

(deftest vectorq
  (testing "arbitrary-vector works"
    (is
     (quickcheck
      (property [x (vector integer)]
                (and (vector? x)
                     (every? integer? x)))))))

(deftest setq
  (testing "arbitrary-set works"
    (is
     (quickcheck
      (property [x (set integer)]
                (and (set? x)
                     (every? integer? x)))))))

(deftest mapq
  (testing "arbitrary-map works"
    (is
     (quickcheck
      (property [x (map integer string)]
                (and (map? x)
                     (every? integer? (keys x))
                     (every? string? (vals x))))))))

(deftest boolean-function
  (testing "creating a function bool -> int works"
    (is
     (quickcheck
      (property [proc (boolean -> integer)]
                (and (function? proc)
                     (integer? (proc true))))))))

(deftest integer-function
  (testing "creating a function int -> int works"
    (is
     (quickcheck
      (property [proc (integer -> integer)]
                (and (function? proc)
                     (integer? (proc 17))))))))

(deftest natural-function
  (testing "creating a function nat -> int works"
    (is
     (quickcheck
      (property [proc (integer -> natural)]
                (and (function? proc)
                     (and (integer? (proc 17)))))))))



(deftest rational-function
  (testing "creating a function rational -> int works"
    (is
     (quickcheck
      (property [proc (rational -> integer)]
                (and (function? proc)
                     (integer? (proc 2/3))))))))

(deftest real-function
  (testing "creating a function nat -> int works"
    (is
      (quickcheck
       (property [proc (float -> integer)]
                 (and (function? proc)
                      (integer? (proc 17.5))))))))

(deftest char-function
  (testing "creating a function char -> int works"
    (is
     (quickcheck
      (property [proc (char -> integer)]
                (and (function? proc)
                     (integer? (proc \a))))))))

(deftest ascii-char-function
  (testing "creating a function ascii-char -> int works"
    (is
     (quickcheck
      (property [proc (ascii-char -> integer)]
                (and (function? proc)
                     (integer? (proc \a))))))))
     
(deftest string-function
  (testing "creating a function string -> int works"
    (is
     (quickcheck
      (property [proc (string -> integer)]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest ascii-string-function
  (testing "creating a function ascii-string -> int works"
    (is
     (quickcheck
      (property [proc (ascii-string -> integer)]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest symbol-function
  (testing "creating a function symbol -> int works"
    (is
     (quickcheck
      (property [proc (symbol -> integer)]
                (and (function? proc)
                     (integer? (proc 'foo))))))))

(deftest mixed-function
  (testing "creating a function mixed -> int works"
    (is
     (quickcheck
      (property [proc ((mixed integer? integer
                              string?  string)
                       -> integer)]
                (and (function? proc)
                     (integer? (proc 15))
                     (integer? (proc "foo"))))))))

(deftest one-of-function
  (testing "creating a function one-of -> int works"
    (is
     (quickcheck
      (property [proc ((one-of = "foo" "bar" "baz") -> integer)]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest vector-function
  (testing "creating a function [int] -> int works"
    (is
     (quickcheck
      (property [proc ((vector integer) -> integer)]
                (and (function? proc)
                     (integer? (proc [15 13]))))))))

(deftest set-function
  (testing "creating a function #{int} -> int works"
    (is
     (quickcheck
      (property [proc ((set integer) -> integer)]
                (and (function? proc)
                     (integer? (proc #{15 13}))))))))

(deftest map-function
  (testing "creating a function {:kw, int} -> int works"
    (is
     (quickcheck
      (property [proc ((map keyword integer) -> integer)]
                (and (function? proc)
                     (integer? (proc {:b 0 :a 42}))))))))

(deftest function-function
  (testing "creating a function function -> int works"
    (is
      (quickcheck
       (property [proc ((char -> boolean) -> integer)]
                 (and (function? proc)
                      (integer? (proc #(= % \A)))))))))

(deftest simple-spec-function
  (testing "creating a function spec -> int works"
    (is
     (quickcheck
      (property [proc ((spec integer?) -> integer)]
                (and (function? proc)
                     (integer? (proc 42))))))))

(deftest and-spec-function
  (testing "creating a function and-spec -> int works"
    (is
     (quickcheck
      (property [proc ((spec (s/and integer? even? #(> % 10))) -> integer)]
                (and (function? proc)
                     (integer? (proc 42))))))))

(deftest ==>q
  (testing "==> works"
    (is
     (quickcheck
      (property [x integer]
                (==> (even? x)
                     (integer? (/ x 2))))))))

(deftest labelq
  (testing "label works"
    (is
     (let [[ntests stamps success]
           (quickcheck-results (property [x integer]
                                         (label "yo" (integer? x))))]
       (and (true? success)
            (every? (fn [el]
                      (= el '("yo")))
                    stamps))))))

(deftest classifyq
  (testing "classify works"
    (is
     (let [[ntests stamps success]
           (quickcheck-results (property [x integer]
                                         (classify (even? x) "even" (integer? x))))]
       (and (true? success)
            (every? (fn [el]
                      (or (= el [])
                          (= el ["even"])))
                    stamps))))))

(deftest trivialq
  (testing "trivial works"
    (is
     (let [[ntests stamps success]
           (quickcheck-results  (property [x integer]
                                          (trivial (even? x) (integer? x))))]
       (and (true? success)
            (every? (fn [el]
                      (or (= el [])
                          (= el ["trivial"])))
                    stamps))))))

(defrecord Foo [bar baz])

(deftest recordq
  (testing "arbitrary-record works"
    (is
     (quickcheck
      (property [x (record ->Foo [:bar integer 
                                  :baz string])]
                (and (instance? Foo x) (integer? (:bar x)) (string? (:baz x))))))))

(macroexpand '(property [x (record ->Foo [:bar integer 
                            :baz string])]
          (and (instance? Foo x) (integer? (:bar x)) (string? (:baz x)))))

(deftest record2
  (testing "arbitrary-record works"
    (is
      (quickcheck 
       (property [proc ((record ->Foo [:bar integer :baz string])
                        -> integer)]
                 (integer? (proc (Foo. 47 "foo"))))))))

(defrecord Bar [bla blu])

(deftest record3
  (testing "arbitrary-record works"
    (is
      (quickcheck 
       (property [proc ((record ->Bar [:bla integer :blu integer])
                        -> integer)]
                 (integer? (proc (Bar. 23 42))))))))

(define-record-type foo
  (make-foo bar bla)
  foo?
  [^{:spec string?} bar foo-bar
   ^{:spec string?} bla foo-bla])

(deftest active-clojure-record-spec
  (is
   (quickcheck
    (property
     [f (spec ::foo)]
     (string? (foo-bar f))))))


;; --- Distribution tools ---------

(deftest occurrences-t
  (is (= (occurrences [["even" "huge"] ["odd" "huge"] []])
         {"even" 1
          "odd" 1
          "huge" 2})))

(deftest distribution-t
  (is (= (distribution [["even" "huge"] ["odd" "huge"] []])
         {"even" 1/3
          "odd" 1/3
          "huge" 2/3})))

(deftest fraction-of-t
  (let [stamps [[] [] []]]
    (is (= 0 (fraction-of "no exist" stamps))))

  (let [stamps [["even" "huge"] ["odd" "huge"] []]]
    (is (= 0 (fraction-of "no exist" stamps)))
    (is (= 1/3 (fraction-of "even" stamps)))
    (is (= 1/3 (fraction-of "odd" stamps)))
    (is (= 2/3 (fraction-of "huge" stamps)))))

(deftest distributed-t
  (testing "distribution checker works"
    (testing "base case"
      (let [stamps-1 [["even" "huge"] ["even"] ["odd" "huge"] ["odd"]]]
        (is (distributed? stamps-1
                          {"even" 0.48
                           "odd" 0.48
                           "huge" 0.48}))))

    (testing "negative case"
      (let [stamps-1 [["even" "huge"] ["even"] ["odd" "huge"] ["odd"]]]
        (is (not
             (distributed? stamps-1
                           {"even" 0.52
                            "odd" 0.48
                            "huge" 0.48})))))

    (testing "empty requirements"
      (let [stamps-1 [["even"] ["even"] ["odd"] ["odd"]]]
        (is (distributed? stamps-1 {}))))

    (testing "empty requirements and stamps"
      (let [stamps-1 []]
        (is (distributed? stamps-1 {}))))

    (testing "empty requirements and labels"
      (let [stamps-1 [[] [] [] []]]
        (is (distributed? stamps-1 {}))))

    (testing "multiple"
      (let [stamps-1 [["a" "b"] ["a" "b"] ["a" "b"] ["a" "b"]]]
        (is (distributed? stamps-1
                          {"a" 1
                           "b" 1}
                          ))))))

(deftest with-distribution-t
  (testing "some odd some even"
    (is

     (with-distribution
       "even" 0.4
       "odd" 0.4

       (quickcheck
        (property [x integer]
                  (label (cond
                           (even? x) "even"
                           (odd? x) "odd")
                         (integer? x))))))))


;; --- Performance ---------

;; `variant` used to be O(n), should now be O(log n)

(deftest log-t
  (is (= 0 (random/log 1)))
  (is (= 1 (random/log 2)))
  (is (= 1 (random/log 3)))
  (is (= 3 (random/log 15))))

(deftest binary-string
  (is (= "0" (random/binary-string 0)))
  (is (= "1" (random/binary-string 1)))
  (is (= "10" (random/binary-string 2)))
  (is (= "10000000000000000000000000000000"
         (random/binary-string (inc Integer/MAX_VALUE)))))

(deftest gamma-encoding
  (is (= [\1] (random/gamma-encoding 1)))
  (is (= [\0 \1 \0] (random/gamma-encoding 2)))
  (is (= [\0 \0 \0 \1 \0 \1 \1] (random/gamma-encoding 11))))

(deftest variant-performance-t
  (is
   (quickcheck
    (property [proc (integer -> integer)]
              (and (function? proc)
                   (integer? (proc 0))
                   (integer? (proc 999999999)))))))

(deftest variant-performance-bind-t
  (is
   (quickcheck
    (property [proc ((integer -> integer) -> integer)]
              (and (function? proc)
                   (integer? (proc inc))))))

  (is
   (quickcheck
    (property [proc (integer -> (integer -> integer))]
              (and (function? proc)
                   (function? (proc 999999999))
                   (integer? ((proc 999999999) 999999999)))))))


;; --- Counter example shrinkage ---------

(deftest shrinking-t
  (let [counter-list
        (-> (property [x (list integer)]
                      (every? even? x))
            (check-quick)
            (check-result-arguments-list)

            ;; well ...
            (first)
            (first)
            (second))]
    (is (= 1 (count counter-list)))))

