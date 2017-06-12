(ns project-euler.problem-010)

;; from problem-7
(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime-with-upper-limit [upper-limit]
  (loop [n 3 primal-numbers [2] count 1]
    (if (< upper-limit (last primal-numbers))
      (drop-last primal-numbers)
      (if (not-divisible-all? n (filter #(< % (Math/sqrt n)) primal-numbers))
             (recur (inc n) (conj primal-numbers n) (inc count))
             (recur (inc n) primal-numbers count)))))

;; BAD: this is too slow.
;; (time (apply + (prime-with-upper-limit 2000001)))

;; code below runs fast.
(def divisors-below-two-million (into [] (prime-with-upper-limit (Math/ceil (Math/sqrt 2000000)))))

(defn- prime? [num divisors]
  (not-divisible-all? num (filter #(<= % (Math/sqrt num)) divisors)))

(defn solver [upper-limit divisors]
  (apply + (filter #(prime? % divisors) (range 2 upper-limit))))

(defn solution-010 []
  (solver 2000000 divisors-below-two-million))
