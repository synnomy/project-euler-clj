(ns project-euler.problem-023)

(defn proper-divisors [dividend]
  (let [sqrt-of-dividend (Math/sqrt dividend)]
    (loop [i 2 divisors [] another-divisors []]
      (if (< sqrt-of-dividend i)
        (cons 1 (concat divisors another-divisors))
        (let [r (rem dividend i)]
          (if (zero? r)
            (recur (inc i)
                   (conj divisors i)
                   (if (= sqrt-of-dividend (float i))
                     another-divisors
                     (cons (/ dividend i) another-divisors)))
            (recur (inc i) divisors another-divisors)))))))

(defn sum-of-proper-divisors [dividend]
  (apply + (proper-divisors dividend)))

(defn abundant-number? [num]
  (< num (sum-of-proper-divisors num)))

(defn abundant-numbers [m]
  (filter #(abundant-number? %) (range 1 m)))


;; stupid! summing up two abundant numbers is simple
;; i <= j
;; (defn sum-of-two-abundant-numbers? [num]
;;   (loop [i 0 j 0]
;;     (let [sum (+ (nth abundant-numbers i) (nth abundant-numbers j))]
;;       (cond
;;         (< num (* (nth abundant-numbers i) 2)) false
;;         (= num sum) true
;;         :else (if (< num sum)
;;                 (recur (+ i 1) (+ i 1))
;;                 (recur i (+ j 1)))))))


;; i forgot to use 'distinct'
;; this function produced same values before bug fixed.
(defn two-abundant-numbers-sums [m]
  (let [a (abundant-numbers m)]
    (distinct (map #(+ (first %) (second %))
                   (for [x a
                         y (filter #(< (+ x %) m) a)] [x y])))))

;; x <= y
(defn sum-of-non-abundant [m]
  (- (apply + (range 1 m))
     (apply + (two-abundant-numbers-sums m))))

(defn solution-023 []
  (sum-of-non-abundant 28124))


