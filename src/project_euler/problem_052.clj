(ns project-euler.problem-052)

(defn- num-to-numseq [^Long x]
  (map #(- (int %) (int \0)) (into [] (str x))))

(defn- count-digit [xs n]
  (count (filter #(= % n) xs)))

(defn- count-digits [^Long x]
  "Returns list of numbers of each digits, most left number indicates # of '0' and most right number indicates '9'."
  (let [nxs (num-to-numseq x)]
    (loop [i 0 xs [0 0 0 0 0 0 0 0 0 0]]
      (if (< 9 i)
        xs
        (recur (inc i) (assoc xs i (count-digit nxs i)))))))

(defn- contains-same-digits? [xs]
  "Search integer list 'xs', and returns true if all the numbers containes exactly same digits, else false."
  (= 1 (count (distinct (map #(count-digits %) xs)))))

;; easy
(defn solver []
  (loop [x 1]
    (let [xs (map #(* x %) (range 1 7))]
      (if (contains-same-digits? xs)
        xs
        (recur (inc x))))))

(defn solution-052 []
  (first (solver)))



