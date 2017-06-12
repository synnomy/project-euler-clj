(ns project-euler.problem-005)
(require '[clojure.string :as string])

(def primal-numbers-one-to-ten [1 2 3 5 7])
(def primal-numbers-one-to-twenty [1 2 3 5 7 11 13 17 19])
(def non-primal-numbers-one-to-twenty [4 6 8 9 10 12 14 15 16 18 20])
(def numbers-one-to-twenty (vec (range 1 21)))

(def required-primal-numbers-for-non-primal-numbers {2 4, 3 2, 5 1, 7 1})

(defn- divisible? [n m] (zero? (mod n m)))

(defn factorize [dividend divisor]
  (loop [n dividend m 2 l ()]
    (if (< (Math/sqrt  dividend) m)
      (first l)
      (if (divisible? n m)
             (recur (/ n m) m (cons m l))
             (recur n (inc m) l)))))

(defn- divisible-by-all? [num coll]
  (every? #(zero? (rem num %)) coll))

;; solved by dishonest way,
;; i counted # of required primal numbers (2, 3, 5, and 7)
(defn solution-005 []
  (* 24 (apply * primal-numbers-one-to-twenty)))
