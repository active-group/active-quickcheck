(defproject de.active-group/active-quickcheck "0.9.0-SNAPSHOT"
  :description "QuickCheck clone for Clojure"
  :url "http://github.com/active-group/active-quickcheck"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/test.check "0.9.0"]
                 [de.active-group/active-clojure "0.29.0"]]
  :plugins [[codox "0.6.4"]]
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :scm {:name "git"
        :url "https://github.com/active-group/active-quickcheck.git"}

  :global-vars {*warn-on-reflection* true})
