(ns project-euler.problem-025)

;; using Math/log cause corruption?
;; i use 'count' and 'str' to count digits instead
(defn indexed-fib
  ([]
   (indexed-fib (biginteger 1) (biginteger 1) 1))
  ([a b idx]
   (lazy-seq (cons (list a idx) (indexed-fib b (+' a b) (inc idx))))))

(defn solver [num-digits]
  (first (filter #(<= num-digits (count (str (first %)))) (indexed-fib))))

(defn solution-025 []
  (solver 1000))
