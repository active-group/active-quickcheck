A big advantage of Clojure's official QuickCheck implementation [`test.check`](https://github.com/clojure/test.check) is its integration with [`Clojure spec`](https://clojure.org/guides/spec). With spec you can specify the structure of your data in any detail you want. Spec allows you to generate sample data that conforms to such a specification and which you can use in your property-based tests. We currently try to make `active-quickcheck` work with Clojure spec.

You can enter the spec world with `spec`. You need to provide something that is a valid spec such as in the following example.

    (ns a
      (:require [clojure.spec.alpha :as s])
      (use active.quickcheck))

    (quickcheck (property [x (spec (s/and integer? even?))]
                          (some-predicate x)))

## Supported spec forms

We are currently supporting a subset of spec constructs that you can work with:

Base generators: `integer?`, `string?`, `keyword?`

    (property [x (spec integer?)]
              ...)


Defined specs

    (s/def ::deffed string?)
    (property [x (spec ::deffed)]
              ...)

`map-of`

    (property [x (s/map-of integer? string?)]
              ...)

`coll-of`, along with all kwargs except `:unique`

    (property [x (s/coll-of integer? :kind vector? :min-count 23 :max-count 25)
               y (s/coll-of string? :count 32)]
              ...)

`and` and `or`

    (property [x (s/and integer? even? #(> % 100))
               y (s/or string? keyword?)]
              ...)
   
Sets as enums

    (property [x #{:a :b :c}]
              ...)

## To Do

* You can currently use `spec` only to generate data but not to generate functions that take a spec as domain or range.
* We need to support more base generators such as `symbol?` etc.
* A lot of the combinators such as `map-of` take kwargs that modify the specs in some regards. We already implemented some of these kwargs with `coll-of`. We should make this implementation work with the other combinators as well.