(ns project-euler.problem-056
  (:require [clojure.math.numeric-tower :as math]))

(defn- num-to-numseq [x]
  (map #(- (int %) (int \0)) (into [] (str x))))

(defn- digital-sum [a b]
  (apply + (num-to-numseq (math/expt a b))))

;; easy
(defn solution-056 []
  (apply max (for [a (range 1 100)
                  b (range 1 100)]
              (digital-sum a b))))


















