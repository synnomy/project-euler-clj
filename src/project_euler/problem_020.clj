(ns project-euler.problem-020
  (:require [clojure.string :as str]))

(defn solution-020 []
  (apply + (map #(- (int %) (int \0))
                (map first
                     (str/split (str (apply * (range 1N 100N))) #"")))))



