(ns deinprogramm.quickcheck-test
  (:use clojure.test)
  (:use deinprogramm.quickcheck))

(defn check-quick
  [prop]
  (let [[ntests stamps success] (quickcheck-results prop)]
    success))

(deftest trivial-ok
  (testing "trivial property"
    (is
     (quickcheck
      (property [x arbitrary-integer]
                (= x x))))))

(deftest trivial-not-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x arbitrary-integer]
                      (not= x x)))))))

(deftest trivial-sometimes-ok
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
  (testing "natural generates only natural numbers"
    (is
     (quickcheck
      (property [x arbitrary-natural]
                (and (integer? x)
                     (>= x 0)))))))

(deftest rational
  (testing "rational generates only rational numbers"
    (is
     (quickcheck
      (property [x arbitrary-rational]
                (rational? x))))))

(deftest floatq
  (testing "float generates only floats"
    (is
     (quickcheck
      (property [x arbitrary-float]
                (float? x))))))

(deftest charq
  (testing "char generates only chars"
    (is
     (quickcheck
      (property [x arbitrary-char]
                (char? x))))))

(deftest ascii-char
  (testing "ascii-char generates only ASCII chars."
    (is
     (quickcheck
      (property [x arbitrary-ascii-char]
                (and (char? x)
                     (< (int x) 128)))))))

(deftest stringq
  (testing "string generates only strings."
    (is
     (quickcheck
      (property [x arbitrary-string]
                (string? x))))))

(deftest symbolq
  (testing "symbol generates only symbols."
    (is
     (quickcheck
      (property [x arbitrary-symbol]
                (symbol? x))))))

(deftest ascii-string
  (testing "ascii-string generates only ASCII strings"
    (is
     (quickcheck
      (property [x arbitrary-ascii-string]
                (and (string? x)
                     (every? #(< (int %) 128) x)))))))

(deftest mixed
  (testing "mixed works"
    (is
     (quickcheck
      (property [x (arbitrary-mixed (list (list integer? (delay arbitrary-integer))
                                          (list string? (delay arbitrary-string))))]
                (or (integer? x) (string? x)))))))
