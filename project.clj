(defproject project-euler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.395"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [spyscope "0.1.5"]]
  :injections [(require 'spyscope.core)]
  :main ^:skip-aot project-euler.core
  :jvm-opts ["-Xmx2G"])
