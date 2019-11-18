(ns active.manual-shrink-test
  (:require [clojure.test :refer :all]
            [active.random :as random]
            [active.quickcheck :as qc]
            [active.clojure.monad :as monad])
  (:use active.manual-shrink))

(def test-gen (random/make-random-generator 12))

(defn generate [m] (qc/generate 5 test-gen m))

(defn is-counterexample
  [mresult]
  (not (qc/check-result-ok (generate mresult))))

(defn get-counterexample
  [mresult]
  (map second (qc/check-result-arguments-list (generate mresult))))

(deftest find-failing-finds-nothing
  (testing "if the vector of shrunks contatains no counterexample find-failing returns :no-failing-result"
    (is (= :no-failing-result (generate (find-failing [] (partial < 13)))))
    (is (= :no-failing-result (generate (find-failing [[12]] (partial < 5)))))
    (is (= :no-failing-result (generate (find-failing [[5 3 1]] (partial < 0)))))
    (is (= :no-failing-result (generate (find-failing [[12 13 14]] (partial > 20)))))
    (is (= :no-failing-result (generate (find-failing [[1 2 3] [4 5 6]] (partial <)))))
    (is (= :no-failing-result (generate (find-failing [[1 2 3] [4 5 6 7] [0 8 9]] (partial not=)))))))

(deftest find-failing-finds-result
  (testing "if the vector of shrunks contains at least one  counterexample it returns the first one"
    (is (= [[1] (qc/make-check-result false [] [])] (generate (find-failing [[1]] (partial = 3)))))
    (is (= [[2] (qc/make-check-result false [] [])] (generate (find-failing [[3 4 2 8 2]] (partial < 2)))))
    (is (= [[1 0 0] (qc/make-check-result false [] [])] (generate (find-failing [[0 1 2 3] [0 1 2 3] [0 1 4 3]] (partial =)))))))


(defn numshrink
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(deftest shrinking-gives-counterexample
  (testing "if shrinking gets an counterexample it returns an counterexample"
    (is (is-counterexample (shrinking [(fn [x] [(- x 1)])] ["x"] [5] (partial = 0) 20)))
    (is (is-counterexample (shrinking [(fn [x] [(/ x 2)])] ["y"] [7] (partial > 3) 20)))
    (is (is-counterexample (shrinking [(fn [x] [(- x 2)])] ["z"] [3] (partial < 5) 15)))
    (is (is-counterexample (shrinking [(fn [x] [(- x 3)])] ["v"] [4] (partial = 5) 11)))
    (is (is-counterexample (shrinking [numshrink numshrink] ["x" "y"] [4 3] (partial =) 11)))))

(deftest shrinking-shrinks
  (testing "shrinking shrinks an counterexample"
    (is (= [1] (get-counterexample (shrinking [(fn [x] [(- x 1)])] ["x"] [5] (partial = 0) 20))))
    (is (= [1] (get-counterexample (shrinking [(fn [x] [(- x 2)])] ["x"] [5] (partial > 0) 15))))
    (is (= [-75] (get-counterexample (shrinking [(fn [x] [(- x 4)])] ["x"] [5] (partial = 0) 20))))
    (is (= [3] (get-counterexample (shrinking [(fn [x] [(/ x  2) (- x 1)])] ["x"] [5] (partial > 3) 10))))
    (is (= [1 0] (get-counterexample (shrinking [numshrink numshrink] ["x" "y"] [4 3] (partial = 5) 11))))))

(deftest shrinking-stops
  (testing "shrinking stops after number of fuel steps"
    (is (= [5] (get-counterexample (shrinking [(fn [x] [(- x 1)])] ["x"] [25] (partial = 0) 20))))))

(deftest for-all-with-shrink-with-name-result-ok
  (testing "for-all-with-shrink-with-name gives ok result, if the argument is no counter example"
    (is (not (is-counterexample (for-all-with-shrink-with-name (partial = 5)
                                                               ["x"]
                                                               [(monad/return 5)]
                                                               [(fn [x] [(- x 1)])]))))
    (is (not (is-counterexample (for-all-with-shrink-with-name (partial > 9)
                                                               ["y"]
                                                               [(monad/return 7)]
                                                               [(fn [x] [(/ x 2)])]))))
    (is (not (is-counterexample (for-all-with-shrink-with-name (partial < 1) ["z"]
                                                               [(monad/return 3)]
                                                               [(fn [x] [(- x 2)])]))))
    (is (not (is-counterexample (for-all-with-shrink-with-name (partial = 4)
                                                               ["v"]
                                                               [(monad/return 4)]
                                                               [(fn [x] [(- x 3)])]))))
    (is (not (is-counterexample (for-all-with-shrink-with-name (partial =)
                                                               ["x" "y"]
                                                               [(monad/return 4)
                                                                (monad/return 4)]
                                                               [(fn [x] [(- x 3)])
                                                                (fn [x] [])]))))))

(deftest for-all-with-shrink-with-name-counterexample
   (testing "for-all-with-shrink-with-name shrinks an counterexample"
     (is (= [1] (get-counterexample (for-all-with-shrink-with-name (partial = 0)
                                                                 ["x"]
                                                                 [(monad/return 5)]
                                                                 [(fn [x] [(- x 1)])]))))
     (is (= [1] (get-counterexample (for-all-with-shrink-with-name (partial > 0)
                                                                 ["x"]
                                                                 [(monad/return 5)]
                                                                 [(fn [x] [(- x 2)])]))))
     (is (= [-75] (get-counterexample (for-all-with-shrink-with-name (partial = 0)
                                                                   "x"
                                                                   [(monad/return 5)]
                                                                   [(fn [x] [(- x 4)])]))))
     (is (= [3] (get-counterexample (for-all-with-shrink-with-name (partial > 3)
                                                                 ["x"]
                                                                 [(monad/return 5)]
                                                                 [(fn [x] [(/ x  2) (- x 1)])]))))
     (is (= [0 1] (get-counterexample (for-all-with-shrink-with-name (partial =)
                                                                   ["x" "y"]
                                                                   [(monad/return 5) (monad/return 6)]
                                                                   [numshrink numshrink]))))))
