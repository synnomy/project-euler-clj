(ns project-euler.problem-027)

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime? [num]
  (cond
    (< num 0) false
    (= num 0) false
    (= num 1) false
    :else (not-divisible-all? num (range 2 (+ 1 (Math/floor (Math/sqrt num)))))))

(defn quadratic-formula
  ([n a b] (+ (* n n) (* a n) b)))

(defn count-consective-primes [a b]
  (count (take-while true?
                     (map #(prime? (quadratic-formula % a b)) (range)))))

(defn solver [m]
  (apply max-key :num-primes
         (for [a (range (- m) (+ 1 m))
               b (range (- m) (+ 1 m))]
           {:num-primes (count-consective-primes a b) :prod (* a b)})))

(defn solution-027 []
  (solver 999))
