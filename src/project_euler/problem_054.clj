(ns project-euler.problem-054
  (:require [clojure.string :as str]))

(def base-hand-value
  {:high-card 10
   :one-pair 30
   :two-pairs 50
   :three-of-a-kind 70
   :straight 90
   :flush 110
   :full-house 130
   :four-of-a-kind 150
   :straight-flush 170
   :royal-flush 190})

(def card-value
  {:2 0
   :3 1
   :4 2
   :5 3
   :6 4
   :7 5
   :8 6
   :9 7
   :T 8
   :J 9
   :Q 10
   :K 11
   :A 12
   })

(def index-to-number {0 :2 1 :3 2 :4 3 :5 4 :6 5 :7 6 :8 7 :9 8 :T 9 :J 10 :Q 11 :K 12 :A})

(def numbers [:2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A])

;; suit keywords
;; :spades :clubs :diamonds :hearts

(defn count-numbers [xs]
  (map (fn [k] (count (filter #(= k %) xs))) numbers))

(defn high-card? [xs]
  (let [comp (->> xs
                  (map #(card-value %))
                  (sort)
                  (reverse))]
    (cons (:high-card base-hand-value) comp)))

(defn flush? [nxs sxs]
  (if (= 1 (count (distinct sxs)))
    (let [c-nums (count-numbers nxs)
          comp (->> nxs
                    (map #(card-value %))
                    (sort)
                    (reverse))]
      (cons (:flush base-hand-value) comp))
    false))

(defn one-pair? [xs]
  (let [c-nums (count-numbers xs)]
    (if (= 1 (count (filter #(= 2 %) c-nums)))
      (let [comp1 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 2 (second %)))
                       (map #(first %))
                       (map #(repeat 2 %))
                       (flatten))
            comp2 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 1 (second %)))
                       (map #(first %)))]
        (cons (:one-pair base-hand-value)
              (concat (reverse (sort comp1))
                      (reverse (sort comp2)))))
      false)))

(defn two-pairs? [xs]
  (let [c-nums (count-numbers xs)]
    (if (= 2 (count (filter #(= 2 %) c-nums)))
      (let [comp1 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 2 (second %)))
                       (map #(first %))
                       (map #(repeat 2 %))
                       (flatten))
            comp2 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 1 (second %)))
                       (map #(first %)))]
        (cons (:two-pairs base-hand-value)
              (concat (reverse (sort comp1))
                      (reverse (sort comp2)))))
      false)))

(defn three-of-a-kind? [xs]
  (let [c-nums (count-numbers xs)]
    (if (= 1 (count (filter #(= 3 %) c-nums)))
      (let [comp1 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 3 (second %)))
                       (map #(first %))
                       (map #(repeat 3 %))
                       (flatten))
            comp2 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 1 (second %)))
                       (map #(first %)))]
        (cons (:three-of-a-kind base-hand-value)
              (concat (reverse (sort comp1))
                      (reverse (sort comp2)))))
      false)))

(defn four-of-a-kind? [xs]
  (let [c-nums (count-numbers xs)]
    (if (= 1 (count (filter #(= 4 %) c-nums)))
      (let [comp1 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 4 (second %)))
                       (map #(first %))
                       (map #(repeat 4 %))
                       (flatten))
            comp2 (->> c-nums
                       (map-indexed vector)
                       (filter #(= 1 (second %)))
                       (map #(first %)))]
        (cons (:four-of-a-kind base-hand-value)
              (concat (reverse (sort comp1))
                      (reverse (sort comp2)))))
      false)))

(defn full-house? [xs]
  (let [p1 (one-pair? xs)
        p2 (three-of-a-kind? xs)]
    (if (and (not= false p1) (not= false p2))
      (cons (:full-house base-hand-value)
            (concat (drop 1 p2) (drop 1 p1)))
      false)))

;;(full-house? [:A :A :T :T :T])

(defn straight? [xs]
  (if (= 5 (apply max (->> (count-numbers xs)
                           (partition 5 1)
                           (map (fn [x] (filter #(= 1 %) x)))
                           (map count))))
    (cons (:straight base-hand-value)
          (reverse (sort (map #(card-value %) xs))))
    false))



(defn straight-flush? [nxs sxs]
  (let [p1 (flush? nxs sxs)
        p2 (straight? nxs)]
    (if (and (not= false p1) (not= false p2))
      (cons (:straight-flush base-hand-value) (drop 1 p2))
      false)))

(defn royal-flush? [nxs sxs]
  (if (and (flush? nxs sxs)
        (= '(1 1 1 1 1)
           (drop 8 (count-numbers nxs))))
    (list (:royal-flush base-hand-value))
    false))

(defn parse-card [s]
  (let [number (subs s 0 1)
        suit (subs s 1 2)]
    [(keyword number) (keyword suit)]))

(defn parse-hand [xs]
  (let [cards (map parse-card xs)
        numbers (for [c cards] (first c))
        suits (for [c cards] (second c))]
    (first (filter #(not= false %)
                   [(royal-flush? numbers suits)
                    (straight-flush? numbers suits)
                    (four-of-a-kind? numbers)
                    (full-house? numbers)
                    (flush? numbers suits)
                    (straight? numbers)
                    (three-of-a-kind? numbers)
                    (two-pairs? numbers)
                    (one-pair? numbers)
                    (high-card? numbers)]))))

(defn solver []
  (let [result1 (atom 0)
        result2 (atom 0)]
    (with-open [rdr (clojure.java.io/reader "./resources/input/p054_poker.txt")]
      (doseq [line (line-seq rdr)]
        (let [l (str/split line #" ")
              player1 (drop-last 5 l)
              player2 (drop 5 l)
              hand1 (parse-hand player1)
              hand2 (parse-hand player2)
              hand-diff (map #(- %2 %1) hand1 hand2)
              comp (some #(if (not= 0 %) %) hand-diff)]
          (if (neg? comp)
            (do (comment (println "player1 won: (" comp ")" player1 hand1 hand2 player2))
                (swap! result1 + 1))
            (do (comment (println "player2 won: (" comp ")" player1 hand1 hand2 player2))
                (swap! result2 + 1)))
          )))
    @result1))

(defn solution-054 []
  (solver))
