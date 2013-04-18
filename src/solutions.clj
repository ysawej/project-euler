

(defn mysolutions [] "My solutions to project euler in clojure")

;; Problem # 1 what is the sum of all multiples of 3 & 5 <1000?
(defn sum-multiples-3-5<1000 [] 
  (letfn [(multiples [n] 
            (map #(* n %) (range (/ 1000 n))))]
  (reduce + 
    (filter #(> 1000 %) (distinct (concat (multiples 3) (multiples 5)))))))





(defn main []
  (println (sum-multiples-3-5<1000)))

(main)


