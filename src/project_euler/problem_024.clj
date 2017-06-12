(ns project-euler.problem-024)

(defn number-of-permutation [rank]
  (apply * (range 1 (+ rank 1))))

(defn lexi-permutation [at in-list out-list]
  (let [rank (count in-list)
        sub (number-of-permutation (dec rank))]
    (if (= rank 1)
      (conj out-list (first in-list))
      (let [p (map #(- at (* sub %)) (range 0 rank))
            idx (- (count (filter pos? p)) 1)]
        (lexi-permutation (- at (* sub idx))
                          (concat (take idx in-list) (drop (+ idx 1) in-list))
                          (conj out-list (nth in-list idx)))))))

(defn solution-024 []
  (reverse (lexi-permutation 1000000 (list 0 1 2 3 4 5 6 7 8 9) (list))))
