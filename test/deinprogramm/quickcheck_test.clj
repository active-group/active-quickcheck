(ns deinprogramm.quickcheck-test
  (:use clojure.test)
  (:use deinprogramm.quickcheck))

(defn check-quick
  [prop]
  (let [[ntests stamps success] (quickcheck-results prop)]
    (= true success)))

(deftest trivial-ok
  (testing "trivial property"
    (is
     (check-quick
      (property ((x arbitrary-integer))
                (= x x))))))

(deftest trivial-not-ok
  (testing "trivial property"
    (is
     (not
      (check-quick
       (property ((x arbitrary-integer))
                 (not= x x)))))))

(deftest trivial-sometimes-ok
  (testing "trivial property"
    (is
     (not
      (check-quick
       (property ((x arbitrary-integer))
                 (< x 5)))))))

(deftest reverse-distributes-over-concat
  (testing "reverse distributes over concat"
    (is
     (check-quick
      (property ((xs (arbitrary-list arbitrary-integer))
                 (ys (arbitrary-list arbitrary-integer)))
                (= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

(deftest reverse-distributes-over-concat-broken
  (testing "reverse distributes over concat: broken"
    (is
     (not
      (check-quick
       (property ((xs (arbitrary-list arbitrary-integer))
                  (ys (arbitrary-list arbitrary-integer)))
                 (= (reverse (concat ys xs)) (concat (reverse ys) (reverse xs)))))))))
