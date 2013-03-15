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
      (property [x integer]
                (= x x))))))

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

(deftest stringq
  (testing "arbitrary-string generates only strings."
    (is
     (quickcheck
      (property [x string]
                (string? x))))))

(deftest symbolq
  (testing "arbitrary-symbol generates only symbols."
    (is
     (quickcheck
      (property [x symbol]
                (symbol? x))))))

(deftest keywordq
  (testing "arbitrary-keyword generates only keywords."
    (is
     (quickcheck
      (property [x keyword]
                (keyword? x))))))

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

(deftest mapq
  (testing "arbitrary-map works"
    (is
     (quickcheck
      (property [x ~(arbitrary-map arbitrary-integer arbitrary-string)]
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

(deftest map-function
  (testing "creating a function {:kw, int} -> int works"
    (is
     (quickcheck
      (property [proc (~(arbitrary-map arbitrary-keyword arbitrary-integer) -> integer)]
                (and (function? proc)
                     (integer? (proc {:b 0 :a 42}))))))))

(deftest function-function
  (testing "creating a function function -> int works"
    (is
      (quickcheck
       (property [proc ((char -> boolean) -> integer)]
                 (and (function? proc)
                      (integer? (proc #(= % \A)))))))))

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
                      (= '("yo")))
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

(deftest record2
  (testing "arbitrary-record works"
    (is
      (quickcheck 
       (property [proc ((record ->Foo [:bar integer :baz string])
                        -> integer)]
                 (integer? (proc (Foo. 47 "foo"))))))))
