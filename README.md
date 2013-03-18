# deinprogramm.quickcheck

A Clojure port of the original Haskell QuickCheck.

(We looked at
[ClojureCheck](https://bitbucket.org/kotarak/clojurecheck) and
[`clojure.test.generative`](https://github.com/clojure/test.generative),
but neither seems faithful to the original, particularly concerning the
reproducibility of test runs, and a set of generator combinators that
includes random generation of functions.)

This library, however, is a straighforward port of the Haskell code
from John Hughes's original paper.

## Releases and Dependency Information

Latest stable release: 0.1.2

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

    [de.deinprogramm/quickcheck "0.1.2"]

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

- [Introduction](https://bitbucket.org/sperber/deinprogramm.quickcheck/src/tip/doc/intro.md?at=default)

## License

Copyright © 2013 Michael Sperber, David Frese, Active Group GmbH

Distributed under the Eclipse Public License, the same as Clojure.
