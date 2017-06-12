(ns project-euler.problem-003)

(require '[clojure.string :as str])

(defn- divisible? [n m] (zero? (mod n m)))

(defn solver [dividend]
  (loop [n dividend m 2 l ()]
    (if (< (Math/sqrt  dividend) m)
      (first l)
      (if (divisible? n m)
             (recur (/ n m) m (cons m l))
             (recur n (inc m) l)))))

(defn solution-003 []
  (solver 600851475143))
