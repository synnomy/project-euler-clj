(ns project-euler.problem-034
  (:require [clojure.string :as str]))

(defn char-to-int [c]
  (- (int c) (int \0)))

(defn sum-digit-factorials [num]
  (let [digits (str num)
        len (count digits)]
    (loop [i 0 sum 0]
      (if (< (- len 1) i)
        sum
        (recur (inc i)
               (+ sum
                  (apply * (range 1 (inc (char-to-int (nth digits i)))))))))))

(defn solver [n]
  (apply + (filter #(= % (sum-digit-factorials %)) (range 3 n))))

(sum-digit-factorials 99999999)
;; '999999' -> 2177280
;; '9999999'-> 2540160
;; '99999999' -> 2903040
;; searching numbers below 2903041 is enough
(defn solution-034 []
  (solver 2903041))

