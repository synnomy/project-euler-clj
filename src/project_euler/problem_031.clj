(ns project-euler.problem-031)

(def coin-values '(1 2 5 10 20 50 100 200))


(defn sum-coins [c]
  (apply + (map * coin-values c)))

(defn butlast-vec [vec]
  (into [] (butlast vec)))

(defn increment-largest-coin [coins]
  (conj (butlast-vec coins) (inc (last coins))))

;; i think this is really ugly code.
(defn find-combinations [goal curr-coins]
  (if (<= (count curr-coins) 1)
    ;; when a combination is found
    1
    ;; finding combinations
    (let [subset-coins (butlast-vec curr-coins)]
      (loop [i 0 result 0 next-coins curr-coins]
        (let [sum (sum-coins next-coins)]
          (cond
            ;; no more search in current coins
            (< goal sum) result
            ;; using no 1p, just one combination.
            (= goal sum) (+ result 1)
            :else (recur (inc i)
                         ;; result
                         (+ result
                            (find-combinations (- goal sum)
                                               (into [] (drop-last next-coins))))
                         ;; next-coins
                         (increment-largest-coin next-coins))))))))

(defn solution-031 []
  (find-combinations 200 [0 0 0 0 0 0 0 0]))

















