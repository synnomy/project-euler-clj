(ns project-euler.problem-017)

;; maybe this code is written in stupid way.
;; whole integer value should be converted to letters first, then count # of chars.

;; one 3 two 3 three 5 four 4 five 4 six 3 seven 5 eight 5 nine 4 ten 3
;; eleven 6 twelve 6 thirteen 8 fourteen 8 fifteen 7 sixteen 7 seventeen 9 eighteen 8 nineteen 8
;;  twenty 6 thirty 6 forty 5 fifty 5 sixty 5 seventy 7 eighty 6 ninety 6
;; hundred 7 thousand 8
(def num-of-letters
  {:0 0 ;; special value
   :1 3 :2 3 :3 5 :4 4 :5 4 :6 3 :7 5 :8 5 :9 4 :10 3
   :11 6 :12 6 :13 8 :14 8 :15 7 :16 7 :17 9 :18 8 :19 8
   :20 6 :30 6 :40 5 :50 5 :60 5 :70 7 :80 6 :90 6
   :100 10 :200 10 :300 12 :400 11 :500 11 :600 10 :700 12 :800 12 :900 11
   :1000 11 })

(defn int-to-digits [num] (map #(- (int %) (int \0)) (into [] (str num))))

(defn digits-to-int [digits] )

;; exp: exponent of ten
(defn char-to-int [c exp]
  (* (apply * (repeat exp 10)) (- (int c) (int \0))))

(defn int-to-keyword [num] (keyword (str num)))

(defn count-letters-below-twenty [num]
  (num-of-letters (int-to-keyword num)))

(defn count-letters-below-one-hundred [num]
  (let [d1 (nth (str num) 0)
        d0 (nth (str num) 1)]
    (+ (num-of-letters (int-to-keyword (char-to-int d0 0)))
       (num-of-letters (int-to-keyword (char-to-int d1 1))))))

(defn count-letters-below-one-thousand [num]
  (let [d2 (nth (str num) 0)]
    (let [hundred (char-to-int d2 2)
          ten-and-one (- num hundred)]
      (+ (num-of-letters (int-to-keyword hundred))
         (cond
           (= 0 ten-and-one) 0 ;; for 100
           (and
            (< 0 ten-and-one)
            (< ten-and-one 20)) (+ 3 (count-letters-below-twenty ten-and-one)) ;; need 'and'
           (and
            (<= 20 ten-and-one)
            (< ten-and-one 100)) (+ 3 (count-letters-below-one-hundred ten-and-one))
           ;; need 'and
           )))))

(defn count-letters-one-thousand [] (num-of-letters :1000))

(defn count-letters [num]
  (cond
    (and (< 0 num) (< num 20)) (count-letters-below-twenty num)
    (and (<= 20 num) (< num 100)) (count-letters-below-one-hundred num)
    (and (<= 100 num) (< num 1000)) (count-letters-below-one-thousand num)
    (= 1000 num) (count-letters-one-thousand)
    :else ()))

(defn solver [maxi]
  (apply + (map #(count-letters %) (range 1 (inc maxi)))))

(defn solution-017 []
  (solver 1000))
