(ns project-euler.problem-036)

(defn- gen-binary-numbers []
  (lazy-seq (map #(apply * (repeat % 2)) (range))))

(defn- count-divisors [num]
  (count (take-while #(<= 0 %) (map #(- num %) (gen-binary-numbers)))))

(defn- decimal-to-binary [num]
  (let [c (count-divisors num)
        bin (take c (gen-binary-numbers))]
    (loop [i (dec c) n num seq []]
      (if (< i 0)
        seq
        (let [q (quot n (nth bin i))
              r (rem n (nth bin i))]
          (if (< 0 q)
            (recur (dec i) r (conj seq q))
            (recur (dec i) n (conj seq 0))))))))

(defn- palindromic-seq? [s]
  (let [l (Math/floor (/ (count s) 2))]
    (let [x (take l s)
          xs (reverse (take-last l s))]
      (if (zero? (compare (vec x) (vec xs)))
        true
        false))))

(defn- palindromic-number? [n]
  (palindromic-seq? (str n)))

(defn- double-base-palindromic? [n]
  (and (palindromic-number? n)
       (palindromic-seq? (decimal-to-binary n))))

(defn solution-036 []
  (apply +  (filter #(double-base-palindromic? %) (range 1 1000001))))
