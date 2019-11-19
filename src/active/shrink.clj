(ns active.shrink
    (:require [active.tree :refer :all]))

(defn dont-shrink
  "takes the root of an shrinking tree as tree"
  [tree-with-shrink]
  (make-Tree (tree-outcome tree-with-shrink) []))

(defn cons-nub
  "puts a at the front of the vector, if it isn't already the head"
  [a vector]
  (cond
    (empty? vector) [a]
    (= (first vector) a) vector
    :else (vec (cons a vector))))

(defn halves
  "Produces a vector containing the results of halving a number x over and over again."
  [x]
  (take-while (partial not= 0)
              (iterate (fn [i] (quot i 2)) x)))

(defn shrink-towards
  "Shrink an number x by edging towards a destination number."
  [destination x]
  (if (= destination x)
    []
    (let [diff (- (quot x 2) (quot destination 2))]
          (cons-nub destination
                   (mapv (partial - x)
                         (halves diff))))))

(defn shrink-one
  "Shrink each of the elements in input-vector using the supplied shrinking-function."
  [shrinking-f input-vector]
  (cond
    (empty? input-vector) []
    :else (let [x0 (first input-vector)
                xs0 (rest input-vector)]
            (concat (mapv (fn [head] (cons head xs0))
                          (shrinking-f x0))
                    (mapv (partial cons x0)
                          (shrink-one shrinking-f xs0))))))

(defn sequence-shrink
  "Turn a vector of trees in to a tree of vectors, using the supplied function to merge shrinking options."
  [merge-shrinking-options-f vector-of-trees]
  (make-Tree (mapv tree-outcome vector-of-trees)
             (mapv (partial sequence-shrink merge-shrinking-options-f)
                   (merge-shrinking-options-f vector-of-trees))))

(defn sequence-shrink-one
  "Turn a list of trees in to a tree of lists, opting to shrink only the elements of the list (i.e. the size of the list will always be the same)."
  [list-of-trees]
  (sequence-shrink (fn [list-of-merge-tree-options]
                     (shrink-one tree-shrinks list-of-merge-tree-options))
                   list-of-trees))

(defn removes
  "Permutes a vector by removing 'num-of-elements' consecutive elements from it:"
  [initial-num-of-elements input-vector]
  (letfn [(do-loop [num-of-elments length-of-vector current-vector]
            (let [head (take num-of-elments current-vector)
                  tail (drop num-of-elments current-vector)]
              (cond
               (> num-of-elments length-of-vector) []
               (empty? tail) [[]]
               :else (cons tail (mapv (partial concat head)
                                      (do-loop num-of-elments
                                               (- length-of-vector num-of-elments) tail))))))]
    (do-loop initial-num-of-elements (count input-vector) input-vector)))

(defn shrink-list
  "Produce a smaller permutation of the input vector."
  [input-vector]
  (apply concat (mapv (fn [number-to-remove]
                        (removes number-to-remove input-vector))
                      (halves (count input-vector)))))

(defn sequence-shrink-list
  "Turn a vector of trees in to a tree of vectors, opting to shrink both the vector itself and the elements in the vector during traversal."
  [vector-of-trees]
  (sequence-shrink (fn [list-of-merge-tree-options]
                    (concat (shrink-list list-of-merge-tree-options)
                            (shrink-one tree-shrinks list-of-merge-tree-options)))
                   vector-of-trees))
