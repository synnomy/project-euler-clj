(ns project-euler.problem-041
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn int-seq-to-int [s]
  (read-string (apply str
                      (map #(char (+ % (int \0)))
                           s))))

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime? [num]
  (cond
    (< num 0) false
    (= num 0) false
    (= num 1) false
    :else (not-divisible-all? num (range 2 (+ 1 (Math/floor (Math/sqrt num)))))))

;; not smart. 
;; (filter #(and (prime? %) (n-digit-pandigital? % 9)) (range 100000000 1000000000))

(defn- gen-n-digit-pandigital [n]
  (combo/permutations (range 1 (inc n))))

(defn solution-041 []
  (apply max
         (flatten
          (for [n (range 1 10)]
            (filter #(prime? %)
                    (map #(int-seq-to-int %) (gen-n-digit-pandigital n)))))))


