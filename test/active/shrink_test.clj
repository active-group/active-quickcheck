(ns active.shrink-test
  (:require [active.clojure.record-spec :refer [define-record-type]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [active.random :as random]
            [clojure.math.numeric-tower :as num])
  (:use active.shrink)
  (:use active.tree))

(deftest cons-nub-works
  (testing "cons-nub works"
    (is [] (cons-nub :a []))
    (is [:a :b :c] (cons-nub :a [:a :b :c]))
    (is [:d :a :b :c] (cons-nub :d [:a :b :c]))))

(deftest halves-works
  (testing "halves works"
    (is (= [30 15 7 3 1] (halves 30)))
    (is (= [128 64 32 16 8 4 2 1] (halves 128)))
    (is (= [-10 -5 -2 -1] (halves -10)))))


(deftest shrink-towards-works
  (testing "shrink-towards works"
    (is (= [] (shrink-towards 10 10)))
    (is (= [0 1024 1536 1792 1920 1984 2016 2032 2040 2044 2046 2047] (shrink-towards 0 2048)))))


(deftest shrink-one-works
  (testing "shrink-one produces collection of collections"
    (is (empty? (shrink-one (partial shrink-towards 3) [])))
    (is (coll? (shrink-one (partial shrink-towards 0) [2 5 8])))
    (is (every? coll? (shrink-one (partial shrink-towards 1) [3 4 5]))))
  (testing "shrink-one works with tree-shrink"
    (is (coll? (shrink-one tree-shrinks [(unfold (partial shrink-towards 0) 5)]))))
  #_(testing "shrink-one works"
  ((is (= [[1 0] [3 2 1 0] [4 2 1 0]] (shrink-one (partial shrink-towards 0) [2 5 8]))))))

(deftest sequence-shrink-works
  (testing "sequence-shrink produces a tree of lists"
    (is (valid-tree? (sequence-shrink (fn [x] []) [])))
    (is (valid-tree? (sequence-shrink (fn [x] []) [(unfold (partial shrink-towards 0) 4)
                                                   (unfold (partial shrink-towards 0) 6)])))
    (is (every? coll? (to-list (sequence-shrink (fn [x] []) [(unfold (partial shrink-towards 0) 1)
                                                             (unfold (partial shrink-towards 0) 4)]))))))

(deftest sequence-shrink-one-works
  (testing "sequence-shrink-one produces a tree of lists"
    (is (valid-tree? (sequence-shrink-one  [])))
    (is (valid-tree? (sequence-shrink-one  [(unfold (partial shrink-towards 0) 4)
                                            (unfold (partial shrink-towards 0) 6)])))
    (is (every? coll? (to-list (sequence-shrink-one  [(unfold (partial shrink-towards 0) 1)
                                                      (unfold (partial shrink-towards 0) 4)]))))))

(deftest removes-works
  (testing "removes produces a collection of collections"
    (is (empty? (removes 3 [])))
    (is (coll? (removes 4 [1 3 4 1 2 3 4])))
    (is (every? coll? (removes 1 [1 2 3 4 5]))))
    (is (every? (partial not-any?  coll?) (removes 1 ['e 'f 'h 'g])))
  (testing "removes works right"
    (is [['b 'c] ['a 'c] ['a 'b]] (removes 1 ['a 'b 'c]))
    (is [[3 4 5 6] [1 2 5 6] [1 2 3 4]] (removes 2 [1 2 3 4 5 6]))))

(deftest shrink-list-works
  (testing "shrink-list produces a collection of collections"
    (is (empty? (shrink-list [])))
    (is (coll? (shrink-list ['a 'b 'd 'e])))
    (is (every? coll? (shrink-list ['d 'e 'f 'g 'i 'k]))))
    (is (every? (partial not-any?  coll?) (shrink-list ['e 'f 'h 'g])))
  (testing "shrink-list produces smaller sublists"
    (is (let [coll ['e 'f 't 'g 'h]]
              (every? (fn [c] (<= (count c) (count coll))) (shrink-list coll))))))

(deftest sequence-shrink-list-works
  (testing "sequence-shrink-list produces tree with lists"
    (is valid-tree? (sequence-shrink-list []))
    (is (valid-tree? (sequence-shrink-list [(unfold (partial shrink-towards 0) 5)
                                            (unfold (partial shrink-towards 0) 3)])))))
