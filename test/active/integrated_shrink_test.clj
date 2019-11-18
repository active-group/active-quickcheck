(ns active.integrated-shrink-test
  (:require [clojure.test :refer :all]
            [active.random :as random]
            [active.quickcheck :as qc]
            [active.clojure.monad :as monad]
            [active.tree :as tree])
      (:use [active.manual-shrink-test :only [generate is-counterexample get-counterexample numshrink]])
      (:use active.integrated-shrink))

(def t-0 (tree/make-Tree [0] []))
(def t-12 (tree/make-Tree [12] []))
(def t-5 (tree/make-Tree [5] []))
(def t-3 (tree/make-Tree [3] []))
(def t-1 (tree/make-Tree [1] []))
(def t-13 (tree/make-Tree [13] []))
(def t-14 (tree/make-Tree [14] []))
(def t-2 (tree/make-Tree [2] []))
(def t-4 (tree/make-Tree [4] []))
(def t-6 (tree/make-Tree [6] []))
(def t-7 (tree/make-Tree [7] []))
(def t-0 (tree/make-Tree [0] []))
(def t-8 (tree/make-Tree [8] []))
(def t-9 (tree/make-Tree [9] []))

(deftest find-failing-finds-nothing
  (testing "if the vector of shrunks contatains no counterexample find-failing returns :no-failing-result"
    (is (= :no-failing-result (generate (find-failing [] (partial < 13)))))
    (is (= :no-failing-result (generate (find-failing [t-12] (partial < 5)))))
    (is (= :no-failing-result (generate (find-failing [t-5 t-3 t-1] (partial < 0)))))
    (is (= :no-failing-result (generate (find-failing [t-12 t-13 t-14] (partial > 20)))))))

(deftest find-failing-finds-result
  (testing "if the vector of shrunks contains at least one  counterexample it returns the first one"
    (is (= [t-1 (qc/make-check-result false [] [])] (generate (find-failing [t-1] (partial = 3)))))
    (is (= [t-2 (qc/make-check-result false [] [])] (generate (find-failing [t-3 t-4 t-2 t-8 t-2] (partial < 2)))))))


(defn numshrinkv [[x]] (map vector (numshrink x)))

(deftest shrinking-gives-counterexample
  (testing "if shrinking gets an counterexample it returns an counterexample"
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [2]) (partial = 0) 20)))
    (is (is-counterexample (shrinking ["y"] (tree/unfold numshrinkv [7]) (partial > 3) 20)))
    (is (is-counterexample (shrinking ["z"] (tree/unfold numshrinkv [3]) (partial < 5) 15)))
    (is (is-counterexample (shrinking ["v"] (tree/unfold numshrinkv [4]) (partial = 5) 11)))
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [4]) (partial = 3) 11)))))

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

(deftest combine-generators-works
  (testing "compining of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (integrated numshrink (monad/return 5))))))
    (is (tree/valid-tree? (generate (combine-generators (fn [a] (fn [b] (fn [c] (vector a b c))))
                                                        (integrated numshrink (monad/return 1))
                                                        (integrated numshrink (monad/return 3))
                                                        (integrated numshrink (monad/return 4))))))
    (is (tree/valid-tree? (generate (combine-generators (curry vector 2)
                                                        (monad/return (tree/make-Tree 1 []))
                                                        (monad/return (tree/make-Tree 2 []))))))
    (is (tree/valid-tree? (generate (combine-generators (curry vector 2)
                                                        (monad/return (tree/make-Tree 'a []))
                                                        (monad/return (tree/make-Tree 'b []))))))))
(def my-tree (integrated numshrink (monad/return 5)))

((curry vector 4) [1 2 3 4])

(generate (apply (partial combine-generators (curry vector (count [my-tree]))) [my-tree]))

(deftest for-all-with-shrink-with-name-counterexample
   (testing "for-all-with-shrink-with-name shrinks an counterexample"
     (is (= [1] (get-counterexample (for-all-with-shrink-with-name (partial = 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0] (get-counterexample (for-all-with-shrink-with-name (partial > 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [1] (get-counterexample (for-all-with-shrink-with-name (partial = 0)
                                                                     "x"
                                                                     [(integrated numshrink (monad/return 5))]))))
     (is (= [3] (get-counterexample (for-all-with-shrink-with-name (partial > 3)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0 1] (get-counterexample (for-all-with-shrink-with-name (partial =)
                                                                     ["x" "y"]
                                                                     [(integrated numshrink (monad/return 5))
                                                                      (integrated numshrink (monad/return 6))]))))))
