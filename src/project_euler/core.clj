(ns project-euler.core)

(defn run-problem
  ""
  [n]
  (let [number (.format (new java.text.DecimalFormat "000") n)
        solution-function (symbol (str "solution-" number))]
    (use (vec (list (symbol (str "project-euler.problem-" number))
                    :only (list solution-function))))
    (eval (list solution-function))))


(defn -main
  ""
  [& args]
  (if (and args (= 1 (count args)))
    (println (time (run-problem (Integer/parseInt (first args)))))
    (println "Usage: lein run [3-digit-number]")))

