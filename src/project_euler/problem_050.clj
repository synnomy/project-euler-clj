(ns project-euler.problem-050)

(defn- prime? [^Long n]
  (cond
    (< n 0) false
    (= n 0) false
    (= n 1) false
    (= n 2) true
    :else (not-any? zero?
                    (map #(rem n %)
                         (range 2 (inc (Math/ceil (Math/sqrt n))))))))

(def primes (filter prime? (range 2 1000000)))

(defn- add-consective-primes [n]
  (->> primes
       (partition n 1)
       (map #(apply + %))
       ))

(defn solver []
  (loop [i 2 xs []]
    (let [adds (take-while #(< % 1000000) (add-consective-primes i))
          adds-primes (filter prime? adds)]
      (if (< (count adds) 1)
        xs
        (let [pick (if (= 0 (count adds-primes))
                     nil
                     (rand-nth adds-primes))]
          (recur (inc i) (conj xs pick)))))))

(defn solution-050 []
  (last (filter (complement nil?) (solver))))













