(ns project-euler.problem-039)

(defn- solver* [p]
  (filter #(let [a (first %)
                 b (second %)
                 c (Math/sqrt (+ (* a a) (* b b)))]
             (and (zero? (- c (Math/floor c)))
                  (= p (+ a b (long c)))))
          (for [a (range 1 p) b (range a p)] [a b])))

(defn solver [n]
  (inc (first (apply max-key
                     second
                     (map-indexed vector
                                  (map #(count (solver* %)) (range 1 n)))))))

(defn solution-039 []
  (solver 1001))
