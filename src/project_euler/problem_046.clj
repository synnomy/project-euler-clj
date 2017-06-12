(ns project-euler.problem-046)

(defn- divisible? [n m] (zero? (mod n m)))

(defn- not-divisible-all? [n coll] (nil? (some #(divisible? n %) coll)))

(defn- prime? [num]
  (cond
    (< num 0) false
    (= num 0) false
    (= num 1) false
    :else (not-divisible-all? num (range 2 (+ 1 (Math/floor (Math/sqrt num)))))))

(def odd-composite-numbers
  (filter #(and ((complement prime?) %)
                (odd? %)) (iterate inc 2)))

(def prime-numbers
  (filter prime? (iterate inc 2)))

(def twiced-square-numbers
  (map #(* 2 (* % %)) (iterate inc 1)))

(defn- prime-numbers-below [n]
  (take-while #(< % n) prime-numbers))

(defn- twiced-square-numbers-below [n]
  (take-while #(< % n) twiced-square-numbers))

(defn solver* [n]
  (some #(if (= n (+ (first %) (second %))) n)
        (for [a (prime-numbers-below n)
              b (twiced-square-numbers-below (inc (- n a)))]
          [a b])))

(defn solver []
  (for [n odd-composite-numbers]
    (solver* n)))

(defn solver-new []
  (let [n odd-composite-numbers]
    (loop [i 0]
      )))

;; too slow!
;; takes more than 1 hour
(defn solution-046 []
  (first (filter #(nil? (second %)) (map-indexed vector (solver)))))
;; => (2131 nil)

;;(nth odd-composite-numbers 2131)
;; => 5777




