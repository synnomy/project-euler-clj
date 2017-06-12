(ns project-euler.problem-019)

(def days-of-month
  {1 31, 2 nil, 3 31, 4 30, 5 31, 6 30, 7 31, 8 31, 9 30, 10 31, 11 30, 12 31})

(def day-of-week
  {0 :Monday
   1 :Tuesday
   2 :Wednesday
   3 :Thirsday
   4 :Friday
   5 :Saturday
   6 :Sunday})

(defn leap-year? [year]
  (if (zero? (rem year 100))
    (zero? (rem year 400))
    (zero? (rem year 4))))

(defn number-of-days [year month]
  (if (= month 2)
    (if (leap-year? year) 29 28)
    (days-of-month month)))

(defn what-day [acc-days]
  (day-of-week (mod acc-days 7)))

(defn days-previous-month [year month acc-days]
  (if (= month 1)
    (+ acc-days (number-of-days (dec year) 12))
    (+ acc-days (number-of-days year (dec month)))))

(defn traverse-months [from-year from-month
                       to-year to-month]
  (loop [curr-year from-year
         curr-month (inc from-month)
         acc-days 1
         first-day-list [:Monday]]
    (if
        (if (= to-month 12)
          (and (= curr-year (inc to-year))
               (= curr-month 1))
          (and (= curr-year to-year)
                 (= curr-month (inc to-month))))
      first-day-list
      (let [new-acc-days (days-previous-month curr-year curr-month acc-days)]
        (if (= curr-month 12)
          (recur (inc curr-year)
                 1
                 new-acc-days (conj first-day-list
                                    (what-day (dec new-acc-days))))
          (recur curr-year
                 (inc curr-month)
                 new-acc-days (conj first-day-list
                                    (what-day (dec new-acc-days)))))))))

(defn solution-019 []
  (- (count (filter #(= % :Sunday) (traverse-months 1900 1 2000 12)))
     (count (filter #(= % :Sunday) (traverse-months 1900 1 1900 12)))))
