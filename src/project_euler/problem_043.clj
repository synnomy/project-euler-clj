(ns project-euler.problem-043
  (:require [clojure.math.combinatorics :as combo]))

(def zero-to-nine-pandigital-numbers
  (filter #(not= (first %) 0) (combo/permutations (range 0 10))))

(defn- int-seq-to-int [s]
  (if (empty? s)
    0
    (if (= 0 (first s))
      (int-seq-to-int (next s))
      (read-string (apply str
                          (map #(char (+ % (int \0)))
                               s))))))

(defn- int-subseq-to-int [s a b]
  (int-seq-to-int (drop-last (- (count s) b 1) (drop a s))))

(defn- divisible? [n m] (zero? (mod n m)))

(defn solver []
  (for [n zero-to-nine-pandigital-numbers]
    (let [p [(divisible? (int-subseq-to-int n 1 3) 2)
             (divisible? (int-subseq-to-int n 2 4) 3)
             (divisible? (int-subseq-to-int n 3 5) 5)
             (divisible? (int-subseq-to-int n 4 6) 7)
             (divisible? (int-subseq-to-int n 5 7) 11)
             (divisible? (int-subseq-to-int n 6 8) 13)
             (divisible? (int-subseq-to-int n 7 9) 17)]]
      (if (every? true? p) n nil))))

;; too slow...
(defn solution-043 []
  (apply + (map #(int-seq-to-int %) (filter #((complement nil?) %) (solver)))))
