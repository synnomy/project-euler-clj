(ns project-euler.problem-038
  (:require [clojure.string :as str]))

(defn pandigital? [num]
  (= (count (set (str num)))
     (count (str num))))

(defn pandigital-1-to-9? [string]
  (let [s (set (into [] string))]
    (and (= (count s) (count string))
         (= (count s) 9)
         ;; 0 should not be allowed
         ((complement contains?) s \0))))

(defn- get-concatenated-product [n s]
  (str/join "" (map #(str (* n %)) s)))

(defn- find-largest-concatenated-product [a b]
  (filter #(pandigital-1-to-9? %)
          (for [a (range 1 a)
                b (range 2 (inc b))]
            (get-concatenated-product a (range 1 (inc b))))))


;; a-digits, up to b-numbers
;; a x b
;; 4   2
;; 3   3
;; 2   4
;; 1   9
(def a-b-vec [[10000 2] [1000 3] [100 4] [10 9]])

(defn solver [vec]
  (apply max (flatten
              (for [v vec]
                (map #(read-string %)
                     (find-largest-concatenated-product (first v) (second v)))))))

(defn solution-038 []
  (solver a-b-vec))

