(ns project-euler.problem-006)
(require '[clojure.string :as str])

(defn sum-of-squares [range-max]
  (apply + (for [x (range 1 (inc range-max))] (* x x))))

(defn square-of-sum [range-max]
  (int (Math/pow (apply + (for [x (range 1 (inc range-max))] x)) 2)))

(defn sum-square-diff [range-max]
  (- (square-of-sum range-max) (sum-of-squares range-max)))

(defn solution-006 []
  (sum-square-diff 100))
