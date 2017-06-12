(ns project-euler.problem-057)

(defn- count-digits [x]
  (count (str x)))

(defn- continued-fraction-root-two
  ([]
   (let [f (/ 1 (+ 2 (/ 1 2)))]
     (cons (+ 1 (/ 1 2)) (cons (+ 1 f) (continued-fraction-root-two f)))))
  ([prev]
   (lazy-seq
    (let [f (/ 1 (+ 2 prev))]
     (cons (+ 1 f) (continued-fraction-root-two f))))))

;; easy, beautiful
(defn solution-057 []
  (count
   (filter #(< 1 %) (map #(/ (count-digits (numerator %))
                             (count-digits (denominator %)))
                         (take 1000 (continued-fraction-root-two))))))
