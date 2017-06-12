(ns project-euler.problem-012)

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- square? [f]
  (= 0.0 (let [sr (Math/sqrt f)]
           (- sr (Math/floor sr)))))

(defn count-divisors [dividend]
  (loop [i 2 counter 0]
    (if (<= (Math/sqrt dividend) i)
      (+ (* 2 counter) 2 (#(if (square? %) 1 0) dividend))
      (if (divisible? dividend i)
        (recur (inc i) (inc counter))
        (recur (inc i) counter)))))

(defn triangle-numbers [n last-number]
  (lazy-seq
   (let [new-number (+ n last-number)]
     (cons new-number
           (triangle-numbers (inc n) new-number)))))

(defn solver [m]
  (take 1 (filter #(< m (count-divisors %)) (triangle-numbers 1 0))))

(defn solution-012 []
  (first (solver 500)))
