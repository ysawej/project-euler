

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


; Problem # 6
(- (apply * (repeat 2 (reduce + (range 101))))
(reduce + (map #(* % %) (range 101))))

; Problem # 7
(defn sieve-primes [n] 
(let [arr (vec (repeat n true)) ];mymap (java.util.HashMap. {:N 1 :prime 2}) ] 
(loop [x 2 sieve arr i 0]
(if-not (< (* x x) n) sieve (if-not (sieve i) (recur (inc x) sieve (inc i)) (recur (inc x) (apply assoc sieve (interleave (range (+ i x) n x) (repeat false))) (inc i)))))))

(defn last-prime [n] (let [sieve (sieve-primes n)] 
(loop [cnt 0 x 0 prime 2] (if (>= cnt n) [x prime] (if (sieve cnt) (recur (inc cnt) (inc x) (+ cnt 2)) (recur (inc cnt) x prime) )))))

(defn mth-prime [n m] (let [sieve (sieve-primes n)] 
(loop [cnt 0 x 0 prime 1] (if (or (>= x m) (>= cnt n)) [x prime] (if (sieve cnt) (recur (inc cnt) (inc x) (+ cnt 2)) (recur (inc cnt) x prime) )))))

(defn main []
	(println "1000000 primes" (last-prime 1000000))
	(println "1000000 primes, 10001 prime " (mth-prime 1000000 10001))
	(println "100000 primes, 10001 prime " (mth-prime 100000 10001))
	(println "110000 primes, 10001 prime " (mth-prime 110000 10001))
	(println "110000 primes, 10001 prime " (mth-prime 110000 6)))
;	(println (sum-multiples-3-5<1000) " " (sum-3&5*<1000)))
;	(println (map #(do (println % " " (prime? %))) (range 100))))
; (println (largest-prime-factor-bad 600851475143)))

(main)


