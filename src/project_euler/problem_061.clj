(ns project-euler.problem-061)

(defn- triangle? [x]
  "Returns true if x is triangle, else false."
  (= 1.0 (mod (Math/sqrt (+ 1 (* 8 x))) 2)))

(defn- square? [x]
  (= 0.0 (mod (Math/sqrt x) 1)))

(defn- pentagonal? [x]
  "Returns true if x is pentagonal, else false."
  (= 5.0 (mod (Math/sqrt (+ 1 (* 24 x))) 6)))

(defn- hexagonal? [x]
  (= 3.0 (mod (Math/sqrt (+ 1 (* 8 x))) 4)))

(defn- heptagonal? [x]
  (= 7.0 (mod (Math/sqrt (+ 9 (* 40 x))) 10)))

(defn- octagonal? [x]
  (= 4.0 (mod (Math/sqrt (+ 4 (* 12 x))) 6)))

(defn- polygonal? [x]
  (or (triangle? x)
      (square? x)
      (pentagonal? x)
      (hexagonal? x)
      (heptagonal? x)
      (octagonal? x)))

(defn- count-polygonal-types [xs]
  (reduce #(conj []
                 (+ (nth %1 0) (nth %2 0))
                 (+ (nth %1 1) (nth %2 1))
                 (+ (nth %1 2) (nth %2 2))
                 (+ (nth %1 3) (nth %2 3))
                 (+ (nth %1 4) (nth %2 4))
                 (+ (nth %1 5) (nth %2 5)))
         (map #(conj []
                     (if (triangle? %) 1 0)
                     (if (square? %) 1 0)
                     (if (pentagonal? %) 1 0)
                     (if (hexagonal? %) 1 0)
                     (if (heptagonal? %) 1 0)
                     (if (octagonal? %) 1 0)) (map #(read-string %) xs))))

(defn- two-digits-cyclic-number [x n xs]
  (if (= n 1)
    (let [head x tail (subs (first xs) 0 2)]
      (conj xs (str head tail)))
    (let [head x tail (str (rand-nth (range 10 100)))]
      (two-digits-cyclic-number tail
                                     (dec n)
                                     (conj xs (str head tail))))))

(two-digits-cyclic-number "12" 6 [])

(defn- two-digits-cyclic-number [x n xs]
  (if (= n 1)
    (let [head x tail (subs (first xs) 0 2)]
      (if (polygonal? (read-string (str head tail)))
        (conj xs (str head tail))
        nil))
    (filter #((complement nil?) %) (map (fn [a] (let [head x tail (str a) curr-digits (str head tail)]
                                          (if (polygonal? (read-string curr-digits))
                                            (two-digits-cyclic-number tail
                                                                      (dec n)
                                                                      (conj xs curr-digits))
                                            nil)))
                                        (range 10 100)))))

(def candidates
  (filter #(let [v (count-polygonal-types %)]
             (not (or (zero? (nth v 0))
                      (zero? (nth v 1))
                      (zero? (nth v 2))
                      (zero? (nth v 3))
                      (zero? (nth v 4))
                      (zero? (nth v 5)))))
          (filter #(= 6 (count (distinct %)))
                  (loop [i 11 candidates []]
                    (if (= i 100)
                      candidates
                      (recur (inc i)
                             (concat candidates
                                     (->> (filter #((complement nil?) %) (two-digits-cyclic-number (str i) 6 []))
                                          (mapcat identity)
                                          (mapcat identity)
                                          (mapcat identity)
                                          (mapcat identity)))))))))



(defn- count-polygonal-types-breakdown [xs]
  (map #(conj []
              (if (triangle? %) 1 0)
              (if (square? %) 1 0)
              (if (pentagonal? %) 1 0)
              (if (hexagonal? %) 1 0)
              (if (heptagonal? %) 1 0)
              (if (octagonal? %) 1 0)) (map #(read-string %) xs)))

(defn- mask-required-type [xs sum]
  (map #(if %2 %1 0) xs (map #(if (< 1 %) false true) sum)))

(defn- types-assignable? [xs sum]
  (< (apply + (mask-required-type xs sum)) 2))

;; difficult, but interesting problem. ugly code below
;; every 6 vectors is same for each other, so use 'first'
(defn solution-061 []
  (apply +
         (map #(read-string %)
              (first
               (filter #(let [b (count-polygonal-types-breakdown %)
                              sum (count-polygonal-types %)]
                          (= 6 (count (filter true?
                                              (for [bb b]
                                                (types-assignable? bb sum))))))
                       (for [c candidates]
                         c))))))


;; type count
;; 個々のpolygonalタイプに必須なタイプが２つ以上含まれるとだめ
;; 必須なタイプはsumで1となるところ
;;
;;     100100 OK
;;     110000 NG
;; sum 111211
