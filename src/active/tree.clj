;; reimplementation of haskell disorder jack

(ns active.tree
    (:require [active.clojure.record :refer [define-record-type]]))

; TODO maybe make this tree lazy
(define-record-type Tree
  (make-Tree outcome shrinks)
  tree?
  [outcome tree-outcome
   shrinks tree-shrinks])

(defn lazy-tree
  [strict-outcome strict-shrinks]
  (make-Tree strict-outcome (lazy-seq strict-shrinks)))

(defn valid-tree?
  "checks if tree-shrink contains only trees until (don't use this on big trees)"
  [tree]
  (and (tree? tree) (every? valid-tree? (tree-shrinks tree))))

(defn approx-valid-tree?
  "checks if tree-shrink contains only trees until depth and width n (trees get to big)"
  [n tree]
  (or (<= n 0)
  (and (tree? tree)
       (every? (partial approx-valid-tree? (- n 1)) (take n (tree-shrinks tree))))))

(defn to-list
  "turns a tree into list containing all the elments fo the tree"
  [tree]
  (cons (tree-outcome tree) (lazy-seq (apply concat (map to-list (tree-shrinks tree))))))

(defn map-tree
  [f tree]
  (lazy-tree (f (tree-outcome tree))
             (map (partial map-tree f) (tree-shrinks tree))))

(defn map-outcome
  [f tree]
  (lazy-tree (f (tree-outcome tree))
             (tree-shrinks tree)))

(defn pure
  [x]
  (lazy-tree x []))

(defn apply-tree
  [ftree tree]
  (let [f (tree-outcome ftree)
        f-tree-list (tree-shrinks ftree)
        y (tree-outcome tree)
        y-tree-list (tree-shrinks tree)]
    (lazy-tree (f y)
               (concat (map (fn [x] (apply-tree x tree)) f-tree-list)
                       (map (fn [y] (apply-tree ftree y)) y-tree-list)))))

(declare unfold-forest)
(defn unfold
  "Build a 'Tree' from an unfolding function and a seed value."
  [unfolding-f seed]
  (lazy-tree seed (unfold-forest unfolding-f seed)))

(defn unfold-forest
  "Build a list of trees from an unfolding function and a seed value."
  [unfolding-f seed]
  (map (partial unfold unfolding-f )
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
    (lazy-tree node
               (concat (map (partial expand unfolding-f)
                             leafs)
                       (unfold-forest unfolding-f tree)))))

(defn lazy-cat' [colls]
  (lazy-seq
   (if (seq colls)
     (concat (first colls) (lazy-cat' (next colls))))))

; (a -> Bool) -> Tree a -> [Tree a]
(defn filter-tree
  "remove all elements wich doesn't statisfy the predicate.
  Retrurns a list of trees"
  [predicate tree]
  (defn go
    [tree]
    (let [outcome (tree-outcome tree)
          shrinks (tree-shrinks tree)]
      (if (predicate outcome)
        [(lazy-tree outcome (lazy-cat' (map go shrinks)))]
        (lazy-cat' (map go shrinks)))))
  (go tree))
