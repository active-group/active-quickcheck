(ns active.tree-test
  (:require [clojure.test :refer :all])
  (:use active.tree))

(defn numshrink
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(deftest map-tree-produces-tree
  (testing "map for tree gives back a valid tree"
     (is (valid-tree? (map-tree (partial + 2) (unfold numshrink 5))))))

(deftest map-outcome-produces-tree
(testing "map-outcome gives back a valid tree"
  (is (valid-tree? (map-outcome (partial + 2) (unfold numshrink 3))))))

(deftest apply-tree-produces-tree
  (testing "apply-tree of two trees produces a tree"
    (is (= (make-Tree 3 []) (apply-tree (make-Tree (fn [x] (+ x 1)) [])
                                        (make-Tree 2 []))))
    (is (valid-tree? (apply-tree (map-tree (fn [x] (fn [y] (+ x y))) (unfold numshrink 7))
                                 (unfold numshrink 4))))))

(deftest unfold-produces-tree
  (testing "checks if unfold produces a valid tree"
    (is (valid-tree? (unfold numshrink 5)))
    (is (valid-tree? (unfold numshrink 7)))
    (is (valid-tree? (unfold numshrink 3)))
    (is (valid-tree? (unfold numshrink 4)))))

(deftest unfold-forest-produces-list-of-trees
  (testing "checks if unfold-forest produces a list of valid trees"
    (is (every? valid-tree? (unfold-forest numshrink 5)))
    (is (every? valid-tree? (unfold-forest numshrink 7)))
    (is (every? valid-tree? (unfold-forest numshrink 3)))
    (is (every? valid-tree? (unfold-forest numshrink 4)))))

