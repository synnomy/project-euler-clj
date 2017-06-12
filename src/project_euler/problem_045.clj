(ns project-euler.problem-045)

(defn- triangle-numbers
  ([]
   (cons 1 (triangle-numbers 2)))
  ([n]
   (lazy-seq
    (cons (/ (* n (+ n 1)) 2)
          (triangle-numbers (inc n))))))

(defn- pentagonal-numbers
  ([]
   (cons 1 (pentagonal-numbers 2)))
  ([n]
   (lazy-seq
    (cons (/ (* n (- (* 3 n) 1)) 2)
          (pentagonal-numbers (inc n))))))

(defn- hexagonal-numbers
  ([]
   (cons 1 (hexagonal-numbers 2)))
  ([n]
   (lazy-seq
    (cons (* n (- (* 2 n) 1))
          (hexagonal-numbers (inc n))))))

(defn- triangle? [n]
  (if (< n 1)
    false
    (zero? (- n (last (take-while #(<= % n) (triangle-numbers)))))))

(defn- pentagonal? [n]
  (if (< n 1)
    false
    (zero? (- n (last (take-while #(<= % n) (pentagonal-numbers)))))))

(defn- hexagonal? [n]
  (if (< n 1)
    false
    (zero? (- n (last (take-while #(<= % n) (hexagonal-numbers)))))))

;; easy, but slow
(defn solution-045 []
  (last
   (take 3
         (filter hexagonal?
                 (filter pentagonal? (triangle-numbers))))))
