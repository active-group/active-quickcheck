(ns active.generator-applicative
  (:require [active.clojure.record :refer [define-record-type]])
  (:require [active.random :as random])
  (:require  [active.clojure.monad :as monad])
  (:require  [active.tree :as tree]))

(define-record-type With-tree-type
  ^{:doc "Use full tree in monadic bind"}
  (make-with-tree tree)
  with-tree?
  [tree get-tree])
(defn with-tree [tree] (make-with-tree tree))

;
(defn generator-pure
  "pure with generators which contains trees"
  [x]
  (monad/return (tree/pure x)))

(defn generator-map
  [f mtree]
  (monad/monadic
   [tree (with-tree mtree)]
   (monad/return (tree/map-tree f tree))))

(defn generator-apply
  "applicative with generators which contains trees"
  [generator-f generator]
  (monad/monadic
   [f (with-tree generator-f)
    tree (with-tree generator)]
   (monad/return (tree/apply-tree f tree))))

(defn curry
  "takes a function f for a arbitrary number of arguments and returns a curried function for num-args arguments"
  [f num-args]
  (letfn [(curry-helper
            [f num-args args]
              (cond
                (= num-args 0) (apply f (reverse args))
                :else (fn [x] (curry-helper f (- num-args 1) (cons x args)))))]
    (cond
      (zero? num-args) (fn [] (f))
      :else (fn [x] (curry-helper f (- num-args 1) [x])))))

(defn combine-generators-curry
  "
  combines n generators with trees applicative with function f
  f has to be curried for n arguments
  "
  ([f] (generator-pure (f)))
  ([f & [a & rest]]
  (reduce generator-apply (generator-map f a) rest)))

(defn combine-generators
  "
  applies a n-aritrary function applicative to generators
  "
  [f & arg-generators]
  (apply combine-generators-curry (curry f (count arg-generators)) arg-generators))

(defn apply-curry
  [f [gen-a & gen-rest]] (apply-curry (f gen-a) gen-rest))

(defn integrated
  "applies a shrinker to a generator"
  [shrink generator-without-shrink]
  (monad/monadic
   [arg generator-without-shrink]
   (monad/return (tree/unfold shrink arg))))
