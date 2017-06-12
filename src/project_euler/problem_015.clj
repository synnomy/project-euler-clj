(ns project-euler.problem-015)

(defn solution-015 []
  (/ (apply * (range 21N 41N)) (apply * (range 1N 21N))))
