(ns project-euler.problem-059
  (:require [clojure.string :as str]))

(defn- encryption-keys [start end]
  (for [x (range (int start) (inc (int end)))
        y (range (int start) (inc (int end)))
        z (range (int start) (inc (int end)))]
    [x y z]))

(defn- string-to-keycode [s]
  (map #(int %) (into [] s)))

(defn- charcode-seq-to-string [xs]
  "char-code seq to string"
  (apply str (map #(char %) xs)))

(defn- find-string [xs s]
  (comment ((complement not-any?) true? (map #(= %1 %2) (partition (count s) 1 xs) (string-to-keycode s))))
  ((complement not-any?) true? (map #(= % (string-to-keycode s)) (partition (count s) 1 xs))))

(defn solver []
  (with-open [rdr (clojure.java.io/reader "./resources/input/p059_cipher.txt")]
    (let [line (first (line-seq rdr))
          message (map read-string (str/split line #","))
          len (count message)]
      (for [pwd (encryption-keys \a \z)]
        (let [key (flatten (repeat (Math/ceil (/ len 3)) pwd))
              decoded (map #(bit-xor %1 %2) message key)]
          (if (< 2 (count (filter true? [(find-string decoded "is")
                                         (find-string decoded "this")
                                         (find-string decoded "that")
                                         (find-string decoded "are")
                                         (find-string decoded "you")])))
            {:key (apply str pwd)
             :message (charcode-seq-to-string decoded)
             :sum (apply + decoded)}))))))

(defn solution-059 []
  (:sum (first (filter (complement nil?) (solver)))))















