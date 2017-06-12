(ns project-euler.problem-049)

(defn- prime? [^Long n]
  (not-any? zero?
            (map #(rem n %)
                 (range 2 (inc (Math/ceil (Math/sqrt n)))))))

(defn- permutation? [xs]
  (= 1 (count (distinct (map #(sort (into [] (str %))) xs)))))

(def four-digit-primes (filter prime? (range 1000 10000)))

;; easy
(defn solver []
  (filter #(and (prime? (second %))
                (prime? (last %))
                (permutation? %))
          (for [i four-digit-primes
                j (range 1 (/ (- 9999 i) 2))]
            (let [d1 i, d2 (+ d1 j), d3 (+ d2 j)]
              [d1 d2 d3]))))

(defn solution-049 []
  (apply str (map #(str %) (last (solver)))))
