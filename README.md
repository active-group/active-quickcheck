# clojure-duh

A Clojure port of the original Haskell QuickCheck.

## Usage

	:::clojure
	(deftest reverse-distributes-over-concat
	  (testing "reverse distributes over concat"
		(is
		 (quickcheck
		  (property [xs (arbitrary-list arbitrary-integer)
					 ys (arbitrary-list arbitrary-integer)]
					(= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

## License

Copyright © 2013 Michael Sperber

Distributed under the Eclipse Public License, the same as Clojure.
