(ns project-euler.problem-048
  (:require [clojure.math.numeric-tower :as math]))

(defn- trunc-left [x n]
  "Truncate decimal number's left to n-digit number"
  (- x
     (* (quot x (math/expt 10 n))
        (math/expt 10 n))))

(defn- n-digit-expt [x y n]
  "Calculate x to yth power in last n-digit number"
  (loop [i 1 acc x]
    (if (= i y)
      acc
      (recur (inc i)
             (trunc-left (* acc x) n)))))

;; easy
(defn solution-048 []
  (trunc-left (apply + (map #(n-digit-expt % % 10) (range 1 1001))) 10))
