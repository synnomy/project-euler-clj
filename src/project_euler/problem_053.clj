(ns project-euler.problem-053
  (:require [clojure.math.combinatorics :as combo]))

(defn solution-053 []
  (count
   (filter #(< 1000000 %)
           (for [n (range 1 101)
                 r (range 1 (inc n))]
             (combo/count-combinations (range 1 (inc n)) r)))))
