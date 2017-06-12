(ns project-euler.problem-016
  (:require [clojure.string :as str]))

(defn solution-016 []
  (apply +
         (map #(- (int (first %))
                  (int \0))
              (str/split (str (apply * (repeat 1000 2N))) #""))))
