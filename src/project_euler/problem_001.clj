(ns project-euler.problem-001)

(defn- multiple-list
  ""
  [n a]
  (range a n a))

(defn- solver*
  ""
  [n]
  (+ (reduce + (multiple-list n 5))
     (reduce + (multiple-list n 3))))

(defn solver
  ""
  [n]
  (let [l (distinct (concat (multiple-list n 5) (multiple-list n 3)))]
    (reduce + l)))

(defn solution-001 []
  (solver 1000))
