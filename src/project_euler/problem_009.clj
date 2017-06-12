(ns project-euler.problem-009)
(require '[clojure.string :as str])

;; a < b < c
;; 1 < a < 333

;; triplet
;; 1 4 

(defn- integer-convertible-float? [f]
  (zero? (- f (Math/floor f))))

(defn- hypotenuse [a b]
  (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2))))

(defn solution-009 []
  (int
   (apply *
          (first
           (take 1
                 (filter #(= 1000.0 (apply + %))
                         (filter #(and (integer-convertible-float? (last %)))
                                 (for [y (range 1 501) x (range 1 334) :while (< x y)]
                                   [x y (hypotenuse x y)]))))))))
