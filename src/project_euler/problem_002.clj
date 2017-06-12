(ns project-euler.problem-002)

(def m-fib (memoize
            (fn [n]
              (if (< 1 n)
                (+ (m-fib (- n 2)) (m-fib (- n 1)))
                1N))))

(defn solver
  [limit]
  (loop [x 1 acc 0]
    (let [fibx (m-fib x)]
      (if (> fibx limit)
        acc
        (recur (+ x 1) (+ acc (if (even? fibx) fibx 0)))))))

(defn solution-002 []
  (time (solver 4000000)))
