;; reimplementation of haskell disorder jack

(ns active.tree
    (:require [active.clojure.record :refer [define-record-type]]))

; TODO maybe make this tree lazy
(define-record-type Tree
  (make-Tree outcome shrinks)
  tree?
  [outcome tree-outcome
   shrinks tree-shrinks])

(defn valid-tree?
  "checks if tree-shrink contains only trees"
  [tree]
  (and (tree? tree)
       (every? valid-tree? (tree-shrinks tree))))


(defn map-tree
  [f tree]
  (make-Tree (f (tree-outcome tree))
         (mapv (partial map-tree f) (tree-shrinks tree))))

(defn map-outcome
  [f tree]
  (make-Tree (f (tree-outcome tree))
             (tree-shrinks tree)))

(defn pure
  [x]
  (make-Tree x []))

(defn apply-tree
  [ftree tree]
  (let [f (tree-outcome ftree)
        f-tree-list (tree-shrinks ftree)
        y (tree-outcome tree)
        y-tree-list (tree-shrinks tree)]
    (make-Tree (f y)
               (vec (concat (map (fn [x] (apply-tree x tree)) f-tree-list)
                            (map (fn [y] (apply-tree ftree y)) y-tree-list))))))

(declare unfold-forest)
(defn unfold
  "Build a 'Tree' from an unfolding function and a seed value."
  [unfolding-f seed]
  (make-Tree seed (unfold-forest unfolding-f seed)))

(defn unfold-forest
  "Build a list of trees from an unfolding function and a seed value."
  [unfolding-f seed]
  (mapv (partial unfold unfolding-f )
        (unfolding-f seed)))

(defn expand
  " Apply an additional unfolding function to an existing tree.

     The root outcome remains intact, only the shrinks are affected, this
     applies recursively, so shrinks can only ever be added using this
     function.

     If you want to replace the shrinks altogether, try:

     > unfold-tree f (outcome old-tree)"
  [unfolding-f tree]
  (let [node (tree-outcome tree)
        leafs (tree-shrinks tree)]
    (make-Tree node
               (concat (mapv (partial expand unfolding-f)
                             leafs)
                       (unfold-forest unfolding-f tree)))))
