(ns active.integrated-shrink
  (:require [active.quickcheck :as qc])
  (:require [active.random :as random])
  (:require  [active.clojure.monad :as monad])
  (:require  [active.tree :as tree])
  (:use [active.manual-shrink :only [result-mapped result-add-argument-if-empty]]))

(defn find-failing
  [smaller func]
  (monad/monadic
   ; TODO use apply
   [results (monad/sequ (mapv qc/coerce->result-generator (mapv (partial apply func)
                                                                (mapv tree/tree-outcome smaller))))]
   (let [failingResults (filter (fn [[_ result]] (not (qc/check-result-ok result)))
                                (mapv vector smaller results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults)))))

(defn shrinking
  "
  get shrinks of args and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result.
  "
  [arg-names args func fuel]
  (assert (tree/tree? args) "args has to be a tree")
  (let [children (tree/tree-shrinks args)]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc qc/nothing :ok true))
       :else (monad/monadic
              (let [[shrunk, failure] maybeFailingResult])
              [result (shrinking arg-names shrunk func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       (mapv vector arg-names (tree/tree-outcome shrunk)))))))))

(defn generator-pure
  "pure with generators which contains trees"
  [x]
  monad/return (tree/pure x))

(defn generator-map
  [f mtree]
  (monad/monadic
   [tree mtree]
   (monad/return (tree/map-tree f tree))))

(defn generator-apply
  "applicative with generators which contains trees"
  [generator-f generator]
  (monad/monadic
   [f generator-f
    tree generator]
   (monad/return (tree/apply-tree f tree))))

(defn curry
  "takes a function f for a arbitrary number of arguments and returns a curried function for num-args arguments"
  [f num-args]
  (letfn [(curry-helper
            [f num-args args]
              (cond
                (= num-args 0) (apply f (reverse args))
                :else (fn [x] (curry-helper f (- num-args 1) (cons x args)))))]
    (fn [x] (curry-helper f (- num-args 1) [x]))))

(defn combine-generators
  "
  combines n generators with trees applicative with function f
  f has to be curried for n arguments
  "
  [f & [a & rest]]
  (reduce generator-apply (generator-map f a) rest))

(defn apply-curry
  [f collection]
  (cond
    (empty? collection) f
    :else (apply-curry (f (first collection)) (rest collection))))

(defn for-all-with-shrink-with-name
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-names arg-trees]
  (assert (= (count arg-names) (count arg-trees))
          "Number of arg-names does not match number of arguments")
  (let [args-tree (apply (partial combine-generators (curry vector (count arg-trees))) arg-trees)]
  (monad/monadic
   [args-tree (qc/coerce->generator args-tree)
    res (qc/coerce->result-generator (apply func (tree/tree-outcome args-tree)))
    shrunken (shrinking arg-names args-tree func 20)]
   (let [result (qc/result-add-arguments res [(vector arg-names (tree/tree-outcome args-tree))])])
                                        ; TODO is this lazy. Check efficiency
   (cond
     (qc/check-result-ok result) (monad/return result)
     (qc/check-result-ok shrunken) (monad/return result)
     :else (monad/return shrunken)))))

(defn integrated
  "applies a shrinker to a generator"
  [shrink generator-without-shrink]
  (monad/monadic
   [arg generator-without-shrink]
   (monad/return (tree/unfold shrink arg))))
