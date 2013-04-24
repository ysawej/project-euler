

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

;; Problem # 2
(defn fibnacci-sum-even-num<n [n] 
  (loop [[a b c] [0 1 0]] (if (> b n) c (recur [b (+ a b) (if (even? b) (+ c b) c)]))))

;; Problem # 3
(defn prime? [n] (not (reduce #(or %1 (zero? (mod n %2))) (concat '(false) (range (int (Math/sqrt n)) 1 -1)))))

(defn largest-prime-factor-bad [n] (loop [x (int (Math/sqrt n))] (if (and (zero? (mod n x)) (prime? x)) x (recur (dec x))))) 
;(int  (Math/sqrt 600851475143))

;; Problem # 4
(defn palindrome? [number] (let [ind-digits (fn [number] (loop [x [] number number] (if (zero? number) x (recur (cons (mod number 10) x) (int (/ number 10)))))) digits (ind-digits number)]  (= (reverse digits) digits)))

(defn three? [number x] (and (zero? (mod number x)) (>= 999 x 100)))

(defn three-dig-factors? [number] (loop [x (int (Math/sqrt number)) y (int (/ number x)) ](if (> x 999) false (if (and (three? number x) (three? number y)) true (recur (inc x) (int (/ number (inc x))))))))


(defn find-largest-palin [] (loop [x (* 999 999)] (if-not (and (palindrome? x) (three-dig-factors? x)) (if (> x 10000) (recur (dec x)) 0) x)))


;; Problem # 5
(defn gcd [a b] (if (< a 1) b (recur (mod b a) a)))
(defn lcm [a b] (int (/ (* a b) (gcd a b))))
(defn lcm-1->20 [] (reduce lcm (range 20 1 -1)))







(defn main []
	(println (sum-multiples-3-5<1000) " " (sum-3&5*<1000)))
;	(println (map #(do (println % " " (prime? %))) (range 100))))
; (println (largest-prime-factor-bad 600851475143)))

(main)


