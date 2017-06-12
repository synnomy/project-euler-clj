(ns project-euler.problem-022
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sorted-names
  (with-open [fin (io/reader "resources/input/p022_names.txt")]
    (sort (str/split (str/replace (.readLine fin) #"\"" "") #","))))

(defn char-score [c] (inc (- (int c) (int \A))))

(defn name-score [s rank]
  (* rank (apply + (map #(char-score (first %)) (str/split s #"")))))

(defn sum-name-scores [names]
  (apply + (map #(name-score (second %) (inc (first %)))
                (map-indexed list names))))

(defn solution-022 []
  (sum-name-scores sorted-names))
