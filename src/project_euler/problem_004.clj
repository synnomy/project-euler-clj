(ns project-euler.problem-004)
(require '[clojure.string :as str])

(defn- palindromic? [n]
  (let [l (Math/floor (/ (count (str n)) 2))]
    (let [x (take l (str n)) xs (reverse (take-last l (str n)))]
      (if (zero? (compare (vec x) (vec xs)))
        true
        false))))

(defn- palindromic_product [min max]
  (into []
        (filter
         #(palindromic? %)
         (for [x (range min max) y (range min max)] (* x y)))))

(defn solver [min max]
  (apply max (palindromic_product min (+ max 1))))

;; this solves the problem-4 successfully.
(defn solution-004 []
  (apply max (palindromic_product 100 1000)))
