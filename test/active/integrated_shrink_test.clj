(ns active.integrated-shrink-test
  (:require [clojure.test :refer :all]
            [active.random :as random]
            [active.quickcheck :as qc]
            [active.clojure.monad :as monad]
            [active.tree :as tree])
      (:use [active.manual-shrink-test :only [generate is-counterexample get-counterexample numshrink]])
      (:use active.integrated-shrink))


(deftest generator-map-works
  (testing "generator-map works"
    (is (= (tree/make-Tree 1 []) (generate (generator-map (partial + 1)(monad/return (tree/make-Tree 0 []))))))))

(deftest generator-apply-works
  (testing "applying of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (generator-apply (monad/return (tree/make-Tree (partial + 1) []))
                                                     (monad/return (tree/make-Tree 0 []))))))))


(deftest integrated-works
  (testing "integrated returns an valid tree"
    (is (tree/valid-tree? (generate (integrated numshrink (monad/return 5)))))))

(deftest combine-generators-curry-works
  (testing "compining of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (combine-generators-curry vector
                                                              (integrated numshrink (monad/return 5))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (fn [a] (fn [b] (fn [c] (vector a b c))))
                                                              (integrated numshrink (monad/return 1))
                                                              (integrated numshrink (monad/return 3))
                                                              (integrated numshrink (monad/return 4))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (curry vector 2)
                                                              (monad/return (tree/make-Tree 1 []))
                                                              (monad/return (tree/make-Tree 2 []))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (curry vector 2)
                                                              (monad/return (tree/make-Tree 'a []))
                                                              (monad/return (tree/make-Tree 'b []))))))))

(deftest combine-generators-works
  (testing "compining of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (integrated numshrink (monad/return 5))))))
    
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (monad/return (tree/make-Tree 1 []))
                                                        (monad/return (tree/make-Tree 2 []))))))
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (monad/return (tree/make-Tree 'a []))
                                                        (monad/return (tree/make-Tree 'b []))))))))
