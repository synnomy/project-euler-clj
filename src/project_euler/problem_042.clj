(ns project-euler.problem-042
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; easy problem

(def english-words
  (with-open [fin (io/reader "./resources/input/p042_words.txt")]
    (sort (str/split (str/replace (.readLine fin) #"\"" "") #","))))

(defn- word-to-value [w]
  (apply + (map #(+ 1 (- (int %) (int \A))) (into [] w))))

(defn- gen-triangle-numbers
  ([]
   (cons 1 (gen-triangle-numbers 2)))
  ([n]
   (lazy-seq (cons (/ (* n (+ n 1)) 2) (gen-triangle-numbers (inc n))))))

(defn- triangle-word? [w]
  (= 0 (last (take-while #(<= 0 %) (map #(- (word-to-value w) %) (gen-triangle-numbers))))))

(defn solution-042 []
  (count (filter true? (for [w english-words]
                         (triangle-word? w)))))
