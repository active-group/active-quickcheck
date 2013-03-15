# clojure-duh

A Clojure port of the original Haskell QuickCheck.

## Usage


### Use directly

	:::clojure
	(quickcheck
	  (property [xs (list integer)
				 ys (list integer)]
				(= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs)))))


### Use from `clojure.test`

	:::clojure
	(deftest reverse-distributes-over-concat
	  (testing "reverse distributes over concat"
		(is
		 (quickcheck
		  (property [xs (list integer)
					 ys (list integer)]
					(= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

## License

Copyright © 2013 Michael Sperber, David Frese, Active Group GmbH

Distributed under the Eclipse Public License, the same as Clojure.
