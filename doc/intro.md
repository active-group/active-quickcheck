# Introduction to `deinprogramm.quickcheck`

The `deinprogramm.quickcheck` library is a straightforward port of the
original version of
[QuickCheck](http://en.wikipedia.org/wiki/Quickcheck) to Clojure.
It allows a programmer to formulate specifications of properties of
her code, and QuickCheck then automatically generates and runs random
test cases that check if the property indeed holds.

Here's a simple example that checks the commutativity of addition:

    :::clojure
    (use 'deinprogramm.quickcheck)
    (quickcheck (property [a integer
                           b integer]
                   (= (+ a b) (+ b a))))
    OK, passed 100 tests.

The `property` macro evaluates to a *property*.  The `quickcheck`
function checks that the property and prints a report of its findings.

Sometimes, of course, the property in question is not fulfilled.  In
this case, QuickCheck will try to find a counterexample and report it:
    
    :::clojure
    (quickcheck (property [a float
                           b float
                           c float]
      (= (* a (* b c)) (* (* a b) c))))
    Falsifiable, after 41 tests:
    a = 19.0 b = -10.850000023841858 c = 11.578947365283966

(Yes, floating-point multiplication is [not
associative](http://en.wikipedia.org/wiki/Floating_point).)

## The `property` macro

The most important from in the QuickCheck library is the `property`
macro, which is similar to `let` in that it binds variables to
values.  The body should evaluate to a boolean saying whether the
property is satisfied, given appropriate bindings for the variables.

The "right-hand sides" of the property bindings (`integer` in this
case) are not expressions that are evaluated, but instead
specifications of so-called *arbitraries*.  Arbitraries are objects
that allow generating random values of a certain type.  In this case,
`integer` is the specification of an arbitrary that generates random
integers.

Similarly, `boolean`, `integer`, `natural`, `rational`, `float`,
`char`, `ascii-char`, `printable-ascii-char`, `string`, `ascii-string`,
`printable-ascii-string, symbol`, `keyword` are all specifications of
arbitraries of the corresponding types.

## Abitraries

Arbitraries are not restricted to atomic, primitive types.  Here is an
example:

    :::clojure
    (quickcheck
      (property [xs (list integer)
                 ys (list integer)]
        (= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs)))))

In this case, `(list integer)` is the specification of an arbitrary
that generates lists of integers.

Similarly, `(vector <arb>)`, `(set <arb>)` are specifications of
arbitraries that generate vectors and sets of the specified element
type.

Moreover, `(map <arb1> <arb2>)` generates maps from `<arb1>` to
`<arb2>`, and `(tuple <arb> ...)` generates fixed-size vectors with
heterogenous elements specified by the operand arbitraries.

The `(one-of <equality> <expr> ...)` specification generates an
arbitrary that will generate the value of one of the `<expr>`s.
`<equality>` should be an equality predicate that works for those
values; it's typically `=`.

The `(mixed <pred> <arb> <pred> <arb> ...)` specification generates a
value selects one of the arbitraries `<arb> ...` randomly and
generates a value from *that*.  The `<pred>` must be predicates that
distinguish values of the associated arbitrary from those of the other
arbitraries.

The `(record <constructor> [<accessor> <arb> ...])` is for generating
record and other compound values.  The `<constructor>` should be a
constructor function (typically `->Foo` for record type `Foo`), and
the brackets contain pairs of an accessor (typically `:bar` for field
`bar`) and the arbitrary associated with that field.

Example:

    :::clojure
    (defrecord Foo [bar baz])
    (property [x (record ->Foo [:bar integer :baz string])]
      ...)

Arbitrary specifications can also be used outside of `property` with
the `arbitrary` macro:

    :::clojure
    (def list-of-integer (arbitrary (list integer)))

Finally, `~<expr>` is an arbitrary specification that evaluates to
`<expr>`'s value.  For example

    :::clojure
    (arbitrary ~list-of-integer)

The set of arbitrary specifications is extensible; check the
`expand-arbitrary` multimethod.

## Properties

Most properties are simply boolean expressions.  Sometimes, properties
hold only for a subset of the values that QuickChdeck generates.  In
this case, the `==>` macro says that a property should only be
required to hold when a condition holds.  Here's an example:

    :::clojure
    (property [x integer]
      (==> (even? x)
           (integer? (/ x 2))))

## Using QuickCheck directly

The `quickcheck` function, applied to a property, attempts to generate
100 examples, checks the properties for those examples, and reports on
the result.

The `quickcheck-results` function is like `quickcheck`, but doesn't
print results.  Instead, it returns a description of the result.

## Using QuickCheck with `clojure.test`

QuickCheck augments `clojure.test`'s `is` macro so that it handles
`quickcheck` forms specially.  Exampel:

    :::clojure
    (deftest ok
      (testing "trivial property"
        (is
         (quickcheck
          (property [x integer]
                    (= x x))))))
