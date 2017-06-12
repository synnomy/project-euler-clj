(ns project-euler.problem-058)

(defn- prime? [^Long n]
  (cond
    (< n 0) false
    (= n 0) false
    (= n 1) false
    (= n 2) true
    :else (not-any? zero?
                    (map #(rem n %)
                         (range 2 (inc (Math/ceil (Math/sqrt n))))))))
(defn solver []
  (loop [i 3                            ; i is side-length
         acc-diagonal-num-primes 0
         last-number 1]
    (let [curr-diagonal-distances (map #(* %1 %2)
                                       (repeat 4 (- i 1)) (range 1 5))
          curr-diagonal-numbers (map #(+ last-number %)
                                     curr-diagonal-distances)
          curr-diagonal-num-primes (count (filter prime? curr-diagonal-numbers))
          diagonal-ratio (/ (+ acc-diagonal-num-primes
                               curr-diagonal-num-primes)
                            (+ 1 (* 4 (/ (- i 1) 2))))]
      (if (<= diagonal-ratio (/ 1 10))
        i
        (recur (+ i 2)
               (+ acc-diagonal-num-primes curr-diagonal-num-primes)
               (last curr-diagonal-numbers))))))

;; really unreadable code
;; (i-side-lengthまでのdiagonalなライン上にある素数の数)/(i-side-lengthまでの素数の数)だと勘違いしていた
;; 正しくは、(i-side-lengthまでのdiagonalなライン上にある素数の数)/(i-side-lengthまでのdiagonalなライン上にある整数の数)
(defn solution-058 []
  (solver))















