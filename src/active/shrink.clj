(ns active.shrink
    (:require [active.tree :refer :all]))

(defn dont-shrink
  "takes the root of an shrinking tree as tree"
  [tree-with-shrink]
  (make-Tree (tree-outcome tree-with-shrink) []))

(defn cons-nub
  "puts a at the front of the seq, if it isn't already the head"
  [a seq]
  (cond
    (empty? seq) [a]
    (= (first seq) a) seq
    :else (vec (cons a seq))))

(defn halves
  "Produces a seq containing the results of halving a number x over and over again."
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
                   (map (partial - x)
                         (halves diff))))))

(defn shrink-one
  "Shrink each of the elements in input-seq using the supplied shrinking-function."
  [shrinking-f input-seq]
  (cond
    (empty? input-seq) []
    :else (let [x0 (first input-seq)
                xs0 (rest input-seq)]
            (concat (map (fn [head] (cons head xs0))
                          (shrinking-f x0))
                    (map (partial cons x0)
                          (shrink-one shrinking-f xs0))))))

(defn sequence-shrink
  "Turn a seq of trees in to a tree of seqs, using the supplied function to merge shrinking options."
  [merge-shrinking-options-f seq-of-trees]
  (make-Tree (map tree-outcome seq-of-trees)
             (map (partial sequence-shrink merge-shrinking-options-f)
                   (merge-shrinking-options-f seq-of-trees))))

(defn sequence-shrink-one
  "Turn a list of trees in to a tree of lists, opting to shrink only the elements of the list (i.e. the size of the list will always be the same)."
  [list-of-trees]
  (sequence-shrink (fn [list-of-merge-tree-options]
                     (shrink-one tree-shrinks list-of-merge-tree-options))
                   list-of-trees))

(defn removes
  "Permutes a seq by removing 'num-of-elements' consecutive elements from it:"
  [initial-num-of-elements input-seq]
  (letfn [(do-loop [num-of-elments length-of-seq current-seq]
            (let [head (take num-of-elments current-seq)
                  tail (drop num-of-elments current-seq)]
              (cond
               (> num-of-elments length-of-seq) []
               (empty? tail) [[]]
               :else (cons tail (map (partial concat head)
                                      (do-loop num-of-elments
                                               (- length-of-seq num-of-elments) tail))))))]
    (do-loop initial-num-of-elements (count input-seq) input-seq)))

(defn shrink-list
  "Produce a smaller permutation of the input seq."
  [input-seq]
  (apply concat (map (fn [number-to-remove]
                        (removes number-to-remove input-seq))
                      (halves (count input-seq)))))

(defn sequence-shrink-list
  "Turn a seq of trees in to a tree of seqs, opting to shrink both the seq itself and the elements in the seq during traversal."
  [seq-of-trees]
  (sequence-shrink (fn [list-of-merge-tree-options]
                    (concat (shrink-list list-of-merge-tree-options)
                            (shrink-one tree-shrinks list-of-merge-tree-options)))
                   seq-of-trees))
