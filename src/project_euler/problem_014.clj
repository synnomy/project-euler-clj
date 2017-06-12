(ns project-euler.problem-014)

(defn collatz [start]
  (loop [n start len 1]
    (if (= 1 n)
      len
      (if (even? n)
        (recur (/ n 2) (inc len))
        (recur (+ 1 (* 3 n)) (inc len))))))

(defn solver [upper-limit]
  (loop [i 1 curr-max 0 start 0]
    (if (< (dec upper-limit) i)
      start
      (let [len (collatz i)]
        (if (< len curr-max)
          (recur (inc i) curr-max start)
          (recur (inc i) len i))))))

(defn solution-014 []
  (solver 1000000))


















