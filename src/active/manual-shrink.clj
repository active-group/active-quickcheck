(ns active.manual-shrink
  (:require [active.quickcheck :as qc])
  (:require [active.clojure.monad :as monad]))

(defn result-mapped
  "Monoidal plus of result."
  [result1 result2]
  (println "result-mapped result1" (pr-str result1))
  (assert (qc/check-result? result1))
  (println "result-mapped result2" (pr-str result2))
  (assert (qc/check-result? result2))
  (cond
    (qc/check-result-ok result1) result2
    :else result1))

(defn- result-add-argument-if-empty
  [res arg]
  (assert (qc/check-result? res))
  (cond
    (empty? (qc/check-result-arguments-list res)) (assoc res :arguments-list [arg])
    :else res))

(defn find-failing
  [smaller func]
  (let [_ (println (pr-str "find-failing smaller:" smaller))])
  (let [_ (println (pr-str "find-failing func smaller:" (mapv func smaller)))])
  (let [_ (println (pr-str "find-failing res-gen func smaller:" (mapv qc/coerce->result-generator (mapv func smaller))))])
  (monad/monadic
   [results (monad/sequ (mapv qc/coerce->result-generator (mapv func smaller)))]
   (let [_ (println (pr-str "find-failing results:" results))
         failingResults (filter (fn [[_ result]] (println "result:" (pr-str result)) (not (qc/check-result-ok result)))
                                (mapv vector smaller results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults)))))

(defn shrinking
  "
  Apply shrink to arg and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result
  "
  [shrink arg-name arg func fuel]
  (let [_ (println "shrinking arg-name:" arg-name)])
  (let [_ (println (pr-str "shrinking arg:" arg))])
  (let [_ (println (pr-str "shrinking fuel:" fuel))])
  (let [children (shrink arg)
        _ (println "shrinking children: " (pr-str children))]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (let [_ (println "shrinking maybeFailingResult:" (pr-str maybeFailingResult))])
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc qc/nothing :ok true))
       :else (monad/monadic
              (let [[shrunk, failure] maybeFailingResult])
              [result (shrinking shrink arg-name shrunk func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       (vector arg-name shrunk))))))))


(defn for-all-with-shrink-with-name
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-name arg shrink]
  (let [_ (println (pr-str "for-all-with-shrink arg-name:" arg-name))])
  (let [_ (println (pr-str "for-all-with-shrink arg:" arg))])
  (monad/monadic
   [arg (qc/coerce->generator arg)
    res (qc/coerce->result-generator (func arg))]
   (let [_ (println (pr-str "for-all-with-shrink arg:" arg))])
   (let [_ (println (pr-str "for-all-with-shrink res:" res))])
   (cond
      (qc/check-result-ok res) (monad/return (qc/result-add-arguments res [(vector arg-name arg)]))
      :else (shrinking shrink arg-name arg func 20))))

;(generate 5 my-gen (for-all-with-shrink-with-name (fn [x] (or (= x 0) (not= x 0)))
;                                                  "x"
;                                                  arbitrary-int
;                                                  (fn [x] [(- x 1)])))
;(generate 5 my-gen (for-all-with-shrink-with-name (partial = 0)
;                                                  "x"
;                                                  arbitrary-int
;                                                  (fn [x] [(- x 1000000)])))

