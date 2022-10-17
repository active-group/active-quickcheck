(ns active.spec-test
  (:require [active.quickcheck :refer :all]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]))

(stest/instrument)
(s/check-asserts true)

;; This is a regression test for a problem we hit in one of our projects

(s/def ::not-nan (s/and number? #(not (Double/isNaN %))))

(s/def ::+-not-nan
  (s/fspec
   :args (s/cat :a ::not-nan
                :b ::not-nan)
   :ret ::not-nan))

(s/fdef my-higher-order-+
  :args (s/cat
         :f ::+-not-nan
         :a ::not-nan
         :b ::not-nan)
  :ret ::not-nan)

(defn my-higher-order-+
  [f a b]
  (f a b))

(deftest t-my-higher-order-+
  (is (let [a 23
            b 65]
        (= (+ a b) (my-higher-order-+ + a b))))
  (is (quickcheck
       (property [a (spec ::not-nan)
                  b (spec ::not-nan)]
                 (is (= (+ a b) (my-higher-order-+ + a b)))))))

(s/fdef my-+
  :args (s/cat
          :a ::not-nan
          :b ::not-nan)
  :ret ::not-nan)

(defn my-+
  [a b]
  (my-higher-order-+ + a b))

(deftest t-my-+
  (is (let [a 23
            b 65]
        (= (+ a b) (my-+ a b))))
  (is (quickcheck
       (property [a (spec ::not-nan)
                  b (spec ::not-nan)]
                 (is (= (+ a b) (my-+ a b)))))))
