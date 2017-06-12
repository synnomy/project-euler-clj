(ns project-euler.problem-063
  (:require [clojure.math.numeric-tower :as math]))

(defn- n-digit-nth-power-numbers [n]
  (drop-while #(< % (apply *' (repeat (dec n) 10)))
              (take-while #(< % (apply *' (repeat n 10)))
                          (map #(apply *' (repeat n %)) (range 1 10)))))

;; straightforward, but there is another simple answer.
;; (apply + (map #(count (n-digit-nth-power-numbers %)) (range 0 22)))

;; 別解
;; 底nが10以上のとき、n^nのdigitsはnより大きくなるため、調べる必要がない
;; また、nが22以上のとき、9^nはn-digitな数にはならないため、これ以上調べる必要がない
;; 以下はlog10を用いて桁数を調べることで、演算量を削減している
(defn solution-063 []
  (apply +
         (map (fn [x] (count (filter #(and (< % x) (<= (dec x) %))
                                     (map #(* x  (Math/log10 %))
                                          (range 1 10)))))
              (range 1 22))))

