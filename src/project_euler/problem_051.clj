(ns project-euler.problem-051)

;; n-digit prime number (n=1...)
;; number of replacement mark '*' (m=1...n)
;;

(defn- prime? [^Long x]
  (cond
    (< x 0) false
    (= x 0) false
    (= x 1) false
    (= x 2) true
    :else (not-any? zero?
                    (map #(rem x %)
                         (range 2 (inc (Math/ceil (Math/sqrt x))))))))

(defn- num-to-numseq [^Long x]
  (map #(- (int %) (int \0)) (into [] (str x))))

(defn- numseq-to-num [xs]
  (apply + (map #(* %1 %2) xs (->> (range 0 (count xs))
                                   (map #(repeat % 10))
                                   (map #(apply * %))
                                   (reverse)))))

(defn- count-digit [xs n]
  (count (filter #(= % n) xs)))

(defn- count-digits [^Long x]
  "Returns list of numbers of each digit, most left number indicates # of '0' and most right number indicates '9'."
  (let [nxs (num-to-numseq x)]
    (loop [i 0 xs [0 0 0 0 0 0 0 0 0 0]]
      (if (< 9 i)
        xs
        (recur (inc i) (assoc xs i (count-digit nxs i)))))))

(defn- replace-digit* [v n i]
  (map #(if (= % n) i %) v))

(defn- replace-digit [v n]
  (->> (if (= (first v) n) (range 1 10) (range 0 10)) ; fixed (previous: (range 0 10))
       (map #(replace-digit* v n %))
       (map #(numseq-to-num %))))

(defn- count-family-primes [xs i]
  (->> (replace-digit xs i)
       (filter prime?)
       (count)))

(defn- solver* [xs]
  (hash-map :count (apply max
                     (->> (distinct xs)
                          (map #(count-family-primes xs %))))
            :value (numseq-to-num xs)))

;; really fun
(defn solution-051 []
  (some #(if (= (:count %) 8) (:value %)) (->> (range 56004 1000000)
                                               (filter prime?)
                                               (map #(num-to-numseq %))
                                               (map #(solver* %)))))


;; I got 111857 first, which is incorrect because 857 cannot be accepted.
;; I noticed the bug and fixed in 'replace-digit'

