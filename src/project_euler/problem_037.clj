(ns project-euler.problem-037
  (:require [clojure.string :as str]))

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime? [num]
  (cond
    (< num 0) false
    (= num 0) false
    (= num 1) false
    :else (not-divisible-all? num (range 2 (+ 1 (Math/floor (Math/sqrt num)))))))

(defn- read-string-skipping-leading-zeros [string]
  (let [s (into [] string)]
    (read-string (apply str
                        (drop (min (count (take-while #(= \0 %) s))
                                   (- (count s) 1)) s)))))

(defn- trunc-left [num i]
  (read-string-skipping-leading-zeros (apply str (into [] (drop i (str num))))))

(defn- trunc-right [num i]
  (read-string (apply str (into [] (drop-last i (str num))))))

(defn- gen-truncated [num]
  (let [len (count (str num))]
    (loop [i 1 s [num]]
      (if (< (dec len) i)
        (distinct s)
        (recur (inc i)
               (conj (conj s (trunc-right num i))
                     (trunc-left num i)))))))

(defn- truncatable-both? [num]
  (every? true? (map #(prime? %) (gen-truncated num))))

(defn solution-037 []
  (- (apply + (take 15 (filter truncatable-both? (filter prime? (range)))))
     (apply + [2 3 5 7])))
