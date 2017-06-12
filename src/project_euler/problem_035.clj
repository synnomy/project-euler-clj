(ns project-euler.problem-035
  (:require [clojure.string :as str]))

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime? [num]
  (cond
    (< num 0) false
    (= num 0) false
    (= num 1) false
    :else (not-divisible-all? num (range 2 (+ 1 (Math/floor (Math/sqrt num)))))))

(defn char-seq-to-long [seq]
  (let [len (- (count seq) 1)]
    (loop [i 0 sum 0]
      (if (< len i)
        (long sum)
        (recur (inc i) (+ sum (* (- (int (nth seq (- len i))) (int \0)) (Math/pow 10 i))))))))

(defn- rotate-number [num]
  (loop [i 0 s (into [] (str num)) r []]
    (let [rot-str (concat (drop 1 s) (take 1 s))]
      (if (< (dec (count s)) i)
        r
        (recur (inc i) rot-str (conj r (char-seq-to-long rot-str)))))))

(defn- circular-prime? [num]
  (every? prime? (rotate-number num)))

(defn count-circular-primes [n]
  (count (filter circular-prime? (filter prime? (range 2 n)))))

(defn solution-035 []
  (count-circular-primes 1000000))
