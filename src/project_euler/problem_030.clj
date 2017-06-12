(ns project-euler.problem-030)

(defn calc-fifth-power [n]
  (apply * (repeat 5 n)))

(def fifth-power {0 (calc-fifth-power 0)
                  1 (calc-fifth-power 1)
                  2 (calc-fifth-power 2)
                  3 (calc-fifth-power 3)
                  4 (calc-fifth-power 4)
                  5 (calc-fifth-power 5)
                  6 (calc-fifth-power 6)
                  7 (calc-fifth-power 7)
                  8 (calc-fifth-power 8)
                  9 (calc-fifth-power 9)})

(defn char-to-int [c] (- (int c) (int \0)))

(defn sum-of-fifth-powers [n]
  (apply + (map #(fifth-power (char-to-int %)) (seq (str n)))))

(defn solver [m]
  (apply + (map #(first %)
                (filter #(= (first %) (second %))
                        (map #(vector % (sum-of-fifth-powers %)) (range 2 m))))))

;; 5-digits (sum-of-fifth-powers   99999) => 295245
;; 6-digits (sum-of-fifth-powers  999999) => 354294
;; 7-digits (sum-of-fifth-powers 9999999) => 413343
;; up to 6-digits numbers is enough
(defn solution-030 []
  (solver 1000000))
