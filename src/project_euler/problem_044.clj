(ns project-euler.problem-044)

(defn- nth-pentagon-number [n] (/ (* n (- (* 3 n) 1)) 2))

(defn- pentagon-numbers
  ([]
   (cons 1 (pentagon-numbers 2)))
  ([n]
   (lazy-seq (cons (nth-pentagon-number n) (pentagon-numbers (inc n))))))

;; slow...
;; (defn- pentagonal? [n]
;;   (if (< n 1)
;;     false
;;     (zero? (- n (last (take-while #(<= % n) (pentagon-numbers)))))))

(defn- pentagonal? [x]
  "Returns true if x is pentagonal, else false."
  (= 5.0 (mod (Math/sqrt (+ 1 (* 24 x))) 6)))

(defn- diff-pentagon-numbers [j k]
  (- (nth (pentagon-numbers) k)
     (nth (pentagon-numbers) j)))

(defn- sum-pentagon-numbers [j k]
  (+ (nth (pentagon-numbers) k)
     (nth (pentagon-numbers) j)))

(defn- n-slided-sums [n]
  (map #(+ %1 %2)
       (drop n (pentagon-numbers))
       (pentagon-numbers)))

(defn- n-slided-diffs [n]
  (map #(- %1 %2)
       (drop n (pentagon-numbers))
       (pentagon-numbers)))

(defn- sum-two-pentagonal-numbers [k j]
  (nth (n-slided-sums (- k j)) (dec j)))

(defn find-index-of-sum-pentagonal [n]
  (filter #(pentagonal? (second %))
          (map-indexed vector
                       (n-slided-sums n))))


(defn find-index-of-diff-pentagonal [n]
  (filter #(pentagonal? (second %))
          (map-indexed vector
                       (n-slided-diffs n))))

(defn solver* [i m]
  (let [diff-candidates (->> (find-index-of-diff-pentagonal i)
                             (take-while #(< (second %) m)))
        sum-candidates (map #(conj [] (first %) (nth (n-slided-sums i) (first %))) diff-candidates)
        found (->> sum-candidates
                   (filter #(pentagonal? (second %))))
        num-candidates (count sum-candidates)
        num-found (count found)]
    (map #(nth (n-slided-diffs i) (first %)) found)))

(defn solver [a b m]
  (map #(solver* % m) (range a b)))

;; this range is too slow
;; (flatten (solver 1 2000) 10000000)

(defn solution-044 []
  (first (flatten (solver 100 2000 10000000))))
