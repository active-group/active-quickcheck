(ns active.manual-shrink
  (:require [active.quickcheck :as qc])
  (:require [active.clojure.monad :as monad]))

(defn result-mapped
  "Monoidal plus of result."
  [result1 result2]
  (assert (qc/check-result? result1))
  (assert (qc/check-result? result2))
  (cond
    (qc/check-result-ok result1) result2
    :else result1))

(defn- result-add-argument-if-empty
  [res arg]
  (assert (qc/check-result? res))
  (cond
    (empty? (qc/check-result-arguments-list res)) (assoc res :arguments-list arg)
    :else res))

(defn cartesian-product [colls]
  (if (empty? colls)
    [[]]
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (cons x more))))

(defn find-failing
  [smaller func]
  (let [args (cartesian-product smaller)]
  (monad/monadic
   [results (monad/sequ (mapv qc/coerce->result-generator (mapv (partial apply func) args)))]
   (let [failingResults (filter (fn [[_ result]] (not (qc/check-result-ok result)))
                                (mapv vector args results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults))))))

(defn shrinking
  "
  Apply shrinks to args and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result.
  For every arg there has to be a shrink function
  "
  [shrinks arg-names args func fuel]
  (assert (= (count shrinks) (count args))
          "Number of shrink functions does not match number of arguments")
  (assert (= (count arg-names) (count args))
          (concat "Number of arg-name("
                  (pr-str (count shrinks))
                  ") does not match number of arguments("
                  (pr-str (count shrinks))
                  ")"))
  (let [children (mapv (fn [shrink arg] (shrink arg)) shrinks args)]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc qc/nothing :ok true))
       :else (monad/monadic
              (let [[shrunks, failure] maybeFailingResult])
              [result (shrinking shrinks arg-names shrunks func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       (mapv vector arg-names shrunks))))))))

(defn for-all-with-shrink-with-name
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-names args shrink]
  (assert (= (count arg-names) (count args))
          "Number of arg-names does not match number of arguments")
  (monad/monadic
   [args (monad/sequ (map qc/coerce->generator args))
    res (qc/coerce->result-generator (apply func args))
    shrunken (shrinking shrink arg-names args func 20)]
   (let [result (qc/result-add-arguments res [(vector arg-names args)])])
   ; TODO is this lazy. Check efficiency
   (cond
      (qc/check-result-ok result) (monad/return result)
      (qc/check-result-ok shrunken) (monad/return result)
      :else (monad/return shrunken))))
