(ns project-euler.problem-032
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn pandigital? [num]
  (= (count (set (str num)))
     (count (str num))))

(defn pandigital-1-to-9? [a b c]
  (let [s (set (concat (str a) (str b) (str c)))]
    (and (= (count s) (count (concat (str a) (str b) (str c))))
         (= (count s) 9)
         ;; 0 should be allowed
         ((complement contains?) s \0))))

(defn seq-to-long [seq]
  (let [len (- (count seq) 1)]
    (loop [i 0 sum 0]
      (if (< len i)
        (long sum)
        (recur (inc i) (+ sum (* (nth seq (- len i)) (Math/pow 10 i))))))))

;; 1-digit x 4-digits = 4- or 5-digits
;; 2-digits x 3-digits = 4- or 5-digits
(defn gen-pandigital-number [n s]
  (map #(:product %)
       (filter #(true? (:pandigital %))
               (flatten
                (for [a (combo/combinations s n)]
                  ;; (combo/partitions a :min 2 :max 2)
                  (for [b (combo/permutations a)]
                    (for [c (combo/partitions b :min 2 :max 2)]
                      (let [x (seq-to-long (first c))
                            y (seq-to-long (second c))]
                        {:pandigital (pandigital-1-to-9? x y (* x y))
                         :product (* x y)}))))))))

(defn solution-032 []
  (apply + (distinct (gen-pandigital-number 5 #{1 2 3 4 5 6 7 8 9}))))
