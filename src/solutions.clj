

(defn mysolutions [] "My solutions to project euler in clojure")

;; Problem # 1 what is the sum of all multiples of 3 & 5 <1000?
(defn sum-multiples-3-5<1000 [] 
  (letfn [(multiples [n] 
            (map #(* n %) (range (/ 1000 n))))]
  (reduce + 
    (filter #(> 1000 %) (distinct (concat (multiples 3) (multiples 5)))))))

(defn sum-3&5*<1000 []
(loop [x3 3 x5 5 sigma 0 ] (if-not (or (< x3 1000) (< x5 1000)) sigma (cond (> x3 x5)  (recur x3 (+ x5 5) (+ sigma x5)) (= x3 x5) (recur (+ x3 3) (+ x5 5) (+ sigma x3)) (< x3 x5)
(recur (+ x3 3) x5 (+ sigma x3))))))

(defn fibnacci-sum-even-num<n [n] 
  (loop [[a b c] [0 1 0]] (if (> b n) c (recur [b (+ a b) (if (even? b) (+ c b) c)]))))

(defn prime? [n] (not (reduce #(or %1 (zero? (mod n %2))) (concat '(false) (range (int (Math/sqrt n)) 1 -1)))))

(defn largest-prime-factor-bad [n] (loop [x (int (Math/sqrt n))] (if (and (zero? (mod n x)) (prime? x)) x (recur (dec x))))) 
;(int  (Math/sqrt 600851475143))




(defn main []
	(println (sum-multiples-3-5<1000) " " (sum-3&5*<1000)))
;	(println (map #(do (println % " " (prime? %))) (range 100))))
; (println (largest-prime-factor-bad 600851475143)))

(main)


