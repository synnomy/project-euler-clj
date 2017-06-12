(ns project-euler.problem-033)

(defn gen-fraction [v]
  (/ (+ (* 10 (nth v 0)) (nth v 1)) (+ (* 10 (nth v 2)) (nth v 3))))

(defn gen-candidates [n]
  (let [range-without-n (concat (range 1 n) (range (inc n) 10))]
    (for [a range-without-n
          b (filter #(< a %) range-without-n)
          r (list [n a n b]
                  [n a b n]
                  [a n n b]
                  [a n b n])]
      [r (/ a b)])))

(defn find-digit-cancelling-fractions []
  (loop [i 1 candidates []]
    (if (< 9 i)
      candidates
      (recur (inc i)
             (concat candidates
                     (filter #(= (gen-fraction (first %)) (second %))
                             (gen-candidates i)))))))


(defn solution-033 []
  (denominator
   (apply *
          (map #(second %)
               (find-digit-cancelling-fractions)))))
