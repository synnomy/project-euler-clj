(ns project-euler.problem-047)

(defn- gen-factor-candidates [n]
  (if (even? n)
    (conj (into [] (range 2 (inc (/ n 2)))) n)
    (conj (into [] (range 3 (/ (inc n) 2))) n)))

(defn- prime? [^Long n]
  (not-any? zero?
            (map #(rem n %)
                 (range 2 (inc (Math/ceil (Math/sqrt n)))))))

(defn- factorize
  "n: a number to be factorized
   c: candidates of factors"
  ([n]
   (factorize n (gen-factor-candidates n)))
  ([n c]
   (if (= n 1)
     (empty '())
     (let [factor (some #(if (zero? (rem n %)) %) c)]
       (cons factor
             (factorize (/ n factor)
                        (filter #(<= factor %) c)))))))

(defn- filter-factorized-numbers [n]
  "get numbers which factorized to n-prime factors."
  (filter #(= n (count (set (factorize %)))) (iterate inc 1)))

(defn- consective-numbers [n]
  (map #(first %)
       (filter #(= 1 (second %))
               (map #(conj [] (first %1) (- (second %1) %2))
                    (map-indexed vector (drop 1 (filter-factorized-numbers n)))
                    (filter-factorized-numbers n)))))

(defn- diff-seq [xs n]
  "Return difference list of n-slided seq and original seq"
  (map #(- %1 %2) (drop n xs) xs))

(defn- find-subseq [xs sub]
  "Return a position of subseq 'sub' in sequence 'xs' when found, else -1"
  (->> (partition (count sub) 1 xs)
       (map-indexed vector)
       (filter #(= (second %) sub))
       (ffirst)))

(defn- find-consective-numbers [n]
  "find a position of n-consective numbers"
  (-> (filter-factorized-numbers n)
      (diff-seq 1)
      (find-subseq (repeat (dec n) 1))))

(defn solver [n]
  (nth (filter-factorized-numbers n) (find-consective-numbers n)))

;; slow
(defn solution-047 []
  (solver 4))

