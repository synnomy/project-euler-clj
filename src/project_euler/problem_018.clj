(ns project-euler.problem-018)

(def triangle
  [[75]
   [95 64]
   [17 47 82]
   [18 35 87 10]
   [20  4 82 47 65]
   [19  1 23 75  3 34]
   [88  2 77 73  7 63 67]
   [99 65  4 28  6 16 70 92]
   [41 41 26 56 83 40 80 70 33]
   [41 48 72 33 47 32 37 16 94 29]
   [53 71 44 65 25 43 91 52 97 51 14]
   [70 11 33 28 77 73 17 78 39 68 17 57]
   [91 71 52 38 17 14 91 43 58 50 27 29 48]
   [63 66  4 68 89 53 67 30 73 16 69 87 40 31]
   [ 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]])

(defn take-left-child [x y]
  (nth (nth triangle (inc y)) x))

(defn take-right-child [x y]
  (nth (nth triangle (inc y)) (inc x)))

(defn take-left-parent [x y]
  (nth (nth triangle (dec y) x)))

(defn take-right-parent [x y]
  (nth (nth triangle (dec y) (inc x))))

(defn find-maximum-path [depth prev-sum]
  (if (<= (count triangle) depth)
    prev-sum
    (let [curr-nodes (first (drop depth triangle))]
      (loop [i 0 curr-sum []]
        (cond
          (= i 0) (recur (inc i) (conj
                                  curr-sum
                                  (+ (nth curr-nodes 0)
                                     (nth prev-sum 0))))
          (= i (- (count curr-nodes) 1)) (find-maximum-path
                                          (inc depth)
                                          (conj curr-sum
                                                (+ (nth curr-nodes i)
                                                   (nth prev-sum (dec i)))))
          :else (recur (inc i) (conj curr-sum
                                     (+ (nth curr-nodes i)
                                        (max (nth prev-sum (dec i))
                                             (nth prev-sum i))))))))))

(defn solution-018 []
  (apply max (find-maximum-path 1 [75])))


