(ns project-euler.problem-040)

(defn- number-to-seq [num]
  (map #(- (int %) (int \0)) (into [] (str num))))

(defn get-champernownes-constant
  ([]
   (cons '(1) (get-champernownes-constant 2)))
  ([n]
   (lazy-seq
    (cons (number-to-seq n) (get-champernownes-constant (inc n))))))

(defn solver [v]
  (apply *
         (let [champernownes-constant (flatten (take (apply max v)
                                                     (get-champernownes-constant)))]
           (for [n v]
             (nth champernownes-constant (dec n))))))

(defn solution-040 []
  (solver [1 10 100 1000 10000 100000 1000000]))
