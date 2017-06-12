(ns project-euler.problem-062
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- cubic-numbers
  ([]
   (map #(apply * (repeat 3 %)) (range)))
  ([start]
   (drop-while #(< % start) (cubic-numbers)))
  ([start end]
   (take-while #(< % end) (cubic-numbers start))))

(defn- cubic? [x]
  (= x (last (take-while #(<= % x) (cubic-numbers)))))

(defn- cubic-permutated-numbers [x]
  (filter #(cubic? %)
          (map #(read-string (apply str (into [] %)))
               (filter #(not= (first %) \0)
                       (combo/permutations (into [] (str x)))))))

(defn- n-digit-cubic-numbers [n]
  (cubic-numbers (apply * (repeat (dec n) 10)) (apply * (repeat n 10))))

(defn- vector-add [xs1 xs2]
  (into []
        (let [len (max (count xs1) (count xs2))]
          (map #(+ %1 %2) xs1 xs2))))

(defn- vector-recursive-count-search [xs]
  (loop [ys xs searched []]
    (if (empty? ys)
      searched
      (let [head (first ys) rest (drop 1 ys)]
        (recur rest
               (conj searched (inc (count (filter #(= % head) rest)))))))))

(defn- vector-recursive-search [xs]
  (loop [ys xs searched []]
    (if (empty? ys)
      searched
      (let [head (first ys) rest (drop 1 ys)]
        (recur rest
               (conj searched
                     (if (= 5
                            (inc
                             (count
                              (filter #(= (:count %) (:count head))
                                      rest)))) (:num head))))))))

(defn- count-digits [x]
  (reduce vector-add
          (map #(assoc [0 0 0 0 0 0 0 0 0 0] % 1)
               (map #(- (int %) (int \0))
                    (into [] (str x))))))

;; two 5-permutated cubic numbers appear in 12-digit.
(filter #(= 5 %)
        (vector-recursive-count-search
         (map count-digits
              (n-digit-cubic-numbers 12))))

;; get that two numbers
(defn solution-062 []
  (first
   (filter #((complement nil?) %)
           (vector-recursive-search
            (map #(sorted-map :count (count-digits %) :num %)
                 (n-digit-cubic-numbers 12))))))
