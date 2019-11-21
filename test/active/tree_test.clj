(ns active.tree-test
  (:require [clojure.test :refer :all])
  (:require [active.shrink :as shrink])
  (:use active.tree))

(defn numshrink
  "a bad shrinking function for trees, which produces small testable trees"
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(deftest to-list-works
  (testing "to-list produces the right list"
    (is [] (to-list (lazy-tree 1 [])))
    (is (every? (comp not coll?) (to-list (unfold numshrink 7))))
    (is (coll? (to-list (unfold numshrink 8))))))

(deftest map-tree-produces-tree
  (testing "map for tree gives back a valid tree"
     (is (valid-tree? (map-tree (partial + 2) (unfold numshrink 5))))))

(deftest map-outcome-produces-tree
(testing "map-outcome gives back a valid tree"
  (is (valid-tree? (map-outcome (partial + 2) (unfold numshrink 3))))))

(deftest apply-tree-produces-tree
  (testing "apply-tree of two trees produces a tree"
    (is (= (lazy-tree 3 []) (apply-tree (lazy-tree (fn [x] (+ x 1)) [])
                                        (lazy-tree 2 []))))
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


(deftest tree-unfold-lazy
  (testing "tree-outcome of a big tree runs fast"
    (let [bignumber 99999999999999999999999999999999]
      (is bignumber (tree-outcome (unfold (partial shrink/shrink-towards 0) bignumber))))))
