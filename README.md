# Active QuickCheck

A Clojure port of the original Haskell QuickCheck.

(We looked at
[ClojureCheck](https://bitbucket.org/kotarak/clojurecheck),
[`clojure.test.generative`](https://github.com/clojure/test.generative),
but neither seems faithful to the original, particularly concerning
the reproducibility of test runs, and a set of generator combinators
that includes random generation of functions.
[`test.check`](https://github.com/clojure/test.check) is going down
the right path, but it's lacking some features we want, and is moving
too slow for our purposes.

This library, however, is a straighforward port of the Haskell code
from John Hughes's original paper.

## Releases and Dependency Information

Latest stable release: 0.1.2

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

    [active-quickcheck "0.1.0"]

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

## Documentation

- [Introduction](https://github.com/active-group/active-quickcheck/blob/master/doc/intro.md)

## License

Copyright © 2013 Active Group GmbH

Distributed under the Eclipse Public License, the same as Clojure.
