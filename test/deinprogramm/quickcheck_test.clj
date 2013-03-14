(ns deinprogramm.quickcheck-test
  (:use clojure.test)
  (:use deinprogramm.quickcheck))

(defn check-quick
  [prop]
  (let [[ntests stamps success] (quickcheck-results prop)]
    success))

(deftest ok
  (testing "trivial property"
    (is
     (quickcheck
      (property [x arbitrary-integer]
                (= x x))))))

(deftest not-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x arbitrary-integer]
                      (not= x x)))))))

(deftest sometimes-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x arbitrary-integer]
                      (< x 5)))))))

(deftest reverse-distributes-over-concat
  (testing "reverse distributes over concat"
    (is
     (quickcheck
      (property [xs (arbitrary-list arbitrary-integer)
                 ys (arbitrary-list arbitrary-integer)]
                (= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

(deftest reverse-distributes-over-concat-broken
  (testing "reverse distributes over concat: broken"
    (is
     (not= true
           (check-quick
            (property [xs (arbitrary-list arbitrary-integer)
                       ys (arbitrary-list arbitrary-integer)]
                      (= (reverse (concat ys xs)) (concat (reverse ys) (reverse xs)))))))))

(deftest natural
  (testing "arbitrary-natural generates only natural numbers"
    (is
     (quickcheck
      (property [x arbitrary-natural]
                (and (integer? x)
                     (>= x 0)))))))

(deftest rational
  (testing "arbitrary-rational generates only rational numbers"
    (is
     (quickcheck
      (property [x arbitrary-rational]
                (rational? x))))))

(deftest floatq
  (testing "arbitrary-float generates only floats"
    (is
     (quickcheck
      (property [x arbitrary-float]
                (float? x))))))

(deftest charq
  (testing "arbitrary-char generates only chars"
    (is
     (quickcheck
      (property [x arbitrary-char]
                (char? x))))))

(deftest ascii-char
  (testing "arbitrary-ascii-char generates only ASCII chars."
    (is
     (quickcheck
      (property [x arbitrary-ascii-char]
                (and (char? x)
                     (< (int x) 128)))))))

(deftest stringq
  (testing "arbitrary-string generates only strings."
    (is
     (quickcheck
      (property [x arbitrary-string]
                (string? x))))))

(deftest symbolq
  (testing "arbitrary-symbol generates only symbols."
    (is
     (quickcheck
      (property [x arbitrary-symbol]
                (symbol? x))))))

(deftest keywordq
  (testing "arbitrary-keyword generates only keywords."
    (is
     (quickcheck
      (property [x arbitrary-keyword]
                (keyword? x))))))

(deftest ascii-string
  (testing "arbitrary-ascii-string generates only ASCII strings"
    (is
     (quickcheck
      (property [x arbitrary-ascii-string]
                (and (string? x)
                     (every? #(< (int %) 128) x)))))))

(deftest mixed
  (testing "arbitrary-mixed works"
    (is
     (quickcheck
      (property [x (arbitrary-mixed (list (list integer? (delay arbitrary-integer))
                                          (list string? (delay arbitrary-string))))]
                (or (integer? x) (string? x)))))))


(deftest one-of
  (testing "arbitrary-one-of works"
    (is
     (quickcheck
      (property [x (arbitrary-one-of = "foo" "bar" "baz")]
                (contains? #{"foo" "bar" "baz"} x))))))

(deftest listq
  (testing "arbitrary-list works"
    (is
     (quickcheck
      (property [x (arbitrary-list arbitrary-integer)]
                (and (list? x)
                     (every? integer? x)))))))

(deftest vectorq
  (testing "arbitrary-vector works"
    (is
     (quickcheck
      (property [x (arbitrary-vector arbitrary-integer)]
                (and (vector? x)
                     (every? integer? x)))))))

(deftest mapq
  (testing "arbitrary-map works"
    (is
     (quickcheck
      (property [x (arbitrary-map arbitrary-integer arbitrary-string)]
                (and (map? x)
                     (every? integer? (keys x))
                     (every? string? (vals x))))))))

(deftest boolean-function
  (testing "creating a function bool -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-boolean)]
                (and (function? proc)
                     (integer? (proc true))))))))

(deftest integer-function
  (testing "creating a function int -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-integer)]
                (and (function? proc)
                     (integer? (proc 17))))))))

(deftest natural-function
  (testing "creating a function nat -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-natural arbitrary-integer)]
                (and (function? proc)
                     (and (integer? (proc 17)))))))))



(deftest rational-function
  (testing "creating a function rational -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-rational)]
                (and (function? proc)
                     (integer? (proc 2/3))))))))

(deftest real-function
  (testing "creating a function nat -> int works"
    (is
      (quickcheck
       (property [proc (arbitrary-function arbitrary-integer arbitrary-float)]
                 (and (function? proc)
                      (integer? (proc 17.5))))))))

(deftest char-function
  (testing "creating a function char -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-char)]
                (and (function? proc)
                     (integer? (proc \a))))))))

(deftest ascii-char-function
  (testing "creating a function ascii-char -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-ascii-char)]
                (and (function? proc)
                     (integer? (proc \a))))))))
     
(deftest string-function
  (testing "creating a function string -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-string)]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest ascii-string-function
  (testing "creating a function ascii-string -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-ascii-string)]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest symbol-function
  (testing "creating a function symbol -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer arbitrary-symbol)]
                (and (function? proc)
                     (integer? (proc 'foo))))))))

(deftest mixed-function
  (testing "creating a function mixed -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer
                                          (arbitrary-mixed (list (list integer? (delay arbitrary-integer) )
                                                                 (list string? (delay arbitrary-string)))))]
                (and (function? proc)
                     (integer? (proc 15))
                     (integer? (proc "foo"))))))))

(deftest one-of-function
  (testing "creating a function one-of -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer
                                          (arbitrary-one-of = "foo" "bar" "baz"))]
                (and (function? proc)
                     (integer? (proc "foo"))))))))

(deftest vector-function
  (testing "creating a function [int] -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer (arbitrary-vector arbitrary-integer))]
                (and (function? proc)
                     (integer? (proc [15 13]))))))))

(deftest map-function
  (testing "creating a function {:kw, int} -> int works"
    (is
     (quickcheck
      (property [proc (arbitrary-function arbitrary-integer (arbitrary-map arbitrary-keyword arbitrary-integer))]
                (and (function? proc)
                     (integer? (proc {:b 0 :a 42}))))))))

(deftest function-function
  (testing "creating a function function -> int works"
    (is
      (quickcheck
       (property [proc (arbitrary-function arbitrary-integer
                                           (arbitrary-function arbitrary-boolean arbitrary-char))]
                 (and (function? proc)
                      (integer? (proc #(= % \A)))))))))

(deftest ==>q
  (testing "==> works"
    (is
     (quickcheck
      (property [x arbitrary-integer]
                (==> (even? x)
                     (integer? (/ x 2))))))))

(deftest labelq
  (testing "label works"
    (is
     (let [[ntests stamps success]
           (quickcheck-results (property [x arbitrary-integer]
                                         (label "yo" (integer? x))))]
       (and (true? success)
            (every? (fn [el]
                      (= '("yo")))
                    stamps))))))

(deftest classifyq
  (testing "classify works"
    (is
     (let [[ntests stamps success]
           (quickcheck-results (property [x arbitrary-integer]
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
           (quickcheck-results  (property [x arbitrary-integer]
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
      (property [x (arbitrary-record ->Foo
                                     (list :bar :baz)
                                     arbitrary-integer
                                     arbitrary-string)]
                (and (instance? Foo x) (integer? (:bar x)) (string? (:baz x))))))))

(deftest record2
  (testing "arbitrary-record works"
    (is
      (quickcheck 
       (property [proc (arbitrary-function arbitrary-integer
                                           (arbitrary-record ->Foo
                                                             (list :bar :baz)
                                                             arbitrary-integer
                                                             arbitrary-string))]
                 (integer? (proc (Foo. 47 "foo"))))))))
