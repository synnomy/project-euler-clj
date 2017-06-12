(ns project-euler.problem-007)
(require '[clojure.string :as str])

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn solver [nth]
  (loop [n 3 primal-numbers [2] count 1]
    (if (< (dec nth) count)
      (take-last 1 primal-numbers)
      (if (not-divisible-all? n primal-numbers)
             (recur (inc n) (conj primal-numbers n) (inc count))
             (recur (inc n) primal-numbers count)))))

(defn solution-007 []
  (solver 10001))
