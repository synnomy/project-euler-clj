(ns project-euler.problem-029)

(defn solution-029 []
  (count (distinct (for [a (range 2 101)
                         b (range 2 101)] (apply *' (repeat a b))))))
