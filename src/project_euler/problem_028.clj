(ns project-euler.problem-028)

;; distances of two numbers are as follows
;; [ 2 2 2 2 4 4 4 4 6 6 6 6 8 8 8 8 ... ]

;; distances from '1'
(defn gen-distances
  ([]
   (gen-distances 1 0))
  ([n prev-sum]
   (lazy-seq
    (concat (map #(+ prev-sum %)
                 (range (* n 2) (inc (* n 2 4)) (* n 2)))
            (gen-distances (inc n) (+ prev-sum (* n 2 4)))))))

;; 3x3 -> 4 distances
;; 5x5 -> 8 distances
;; nxn -> (n-1)/2 * 4
(defn count-spiral [n]
  (apply + (cons 1 (map #(+ 1 %) (take (* 4 (/ (- n 1) 2)) (gen-distances))))))

(defn solution-028 []
  (count-spiral 1001))
