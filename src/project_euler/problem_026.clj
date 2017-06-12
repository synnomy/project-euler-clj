(ns project-euler.problem-026)

(defn decimal-left-shift [n m] )

(defn find-reciprocal-cycles* [dividend divisor quot-digits dividend-histry]
  (if (zero? dividend)
    nil
    (if (< dividend divisor)
      (find-reciprocal-cycles* (* dividend 10) divisor quot-digits dividend-histry)
      (let [idx (.indexOf dividend-histry dividend)]
        (cond
          ;; recurring some sequences
          (= 0 idx) dividend-histry
          ;; recurring same digits
          (< 0 idx) [dividend]
          ;; finding recursion
          (< idx 0) (let [q (quot dividend divisor)
                          r (rem dividend divisor)]
                      (find-reciprocal-cycles* (* r 10)
                                               divisor
                                               (conj quot-digits q)
                                               (conj dividend-histry dividend))))))))

(defn find-reciprocal-cycles [n]
  (map #(quot % n) (find-reciprocal-cycles* 1 n [] [])))


(defn solver [n]
  (let [cycles (map #(count (find-reciprocal-cycles %)) (range 1 n))
        max-cycle (apply max cycles)]
    (+ 1 (.indexOf cycles max-cycle))))

(defn solution-026 []
  (solver 1000))




