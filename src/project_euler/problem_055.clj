(ns project-euler.problem-055)

(def lychrel-iteration 50)

(defn- palindromic-seq? [s]
  (let [l (Math/floor (/ (count s) 2))]
    (let [x (take l s)
          xs (reverse (take-last l s))]
      (if (zero? (compare (vec x) (vec xs)))
        true
        false))))

(defn char-seq-to-long [sq]
  (let [len (- (count sq) 1)]
    (loop [i 0 sum 0]
      (if (< len i)
        (bigint sum)
        (recur (inc i) (+ sum (* (- (int (nth sq (- len i))) (int \0)) (Math/pow 10 i))))))))

(defn- palindromic-number? [n]
  (palindromic-seq? (str n)))

(defn- reversed-number [x]
  (char-seq-to-long (reverse (into [] (str x)))))

(defn- lychrel-number? [x]
  (loop [i 0 prev-sum x]
    (if (= i lychrel-iteration)
      true
      (let [new-sum (+ prev-sum (reversed-number prev-sum))]
        (if (palindromic-number? new-sum)
          false
          (recur (inc i) new-sum))))))

;; easy
(defn solution-055 []
  (count (filter true? (map #(lychrel-number? %) (range 1 10001)))))
