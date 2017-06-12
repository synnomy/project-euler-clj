(ns project-euler.problem-021
  (:require [clojure.string :as str]))

(defn proper-divisors [dividend]
  (loop [i 2 divisors [] another-divisors []]
    (if (< (Math/floor (Math/sqrt dividend)) i)
      (cons 1 (concat divisors another-divisors))
      (let [r (rem dividend i)]
        (if (zero? r)
          (recur (inc i) (conj divisors i) (cons (/ dividend i) another-divisors))
          (recur (inc i) divisors another-divisors))))))

(defn sum-of-proper-divisors [dividend]
  (apply + (proper-divisors dividend)))


(defn find-amicable-numbers []
  (filter #(let [b (sum-of-proper-divisors %)]
          (and (= % (sum-of-proper-divisors b))
               (not= % b))) (range 1 10000)))

(defn solution-021 []
  (apply + (find-amicable-numbers)))
