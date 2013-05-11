

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

;; Problem # 8

(def hel "<sequence of digits>")
 (require '[clojure.string])
(apply max (map (comp #(apply * %) (partial map #(- (int %) (int \0)))) (partition 5 1 (clojure.string/replace hel #"\n" ""))))

;; Problem # 9
(defn pyth-1k-sum? [b] (let [ b b a (int (/ (- 500000 (* 1000 b)) (- 1000 b))) c (- 1000 a b)] (and (every? pos? [a b c]) (= (* c c) (+ (* a a) (* b b))))))

( let [doublet (filter pyth-1k-sum? (range 1 667)) triplet (conj doublet (- 1000 (apply + doublet)))] (apply * triplet))

;; Problem # 10
(defn sum-primes [n] (let [sieve (sieve-primes n)] (loop [sum 0 index 0] (if (>= index (- n 2)) sum (if (sieve index) (recur (+ sum index 2) (inc index)) (recur sum (inc index)))))))

;(sum-primes 2000000)

;; Problem # 11

(require '[clojure.string :as string])
;(def t20 (vec (map #(vec (map (fn [n] (Integer/parseInt n)) (string/split % #" "))) (string/split t20str #"\n"))))

(defn find-max-4-prod-in-grid [t20] (let [n (count t20)] (reduce max (for [x (range n) y (range n)] 
  (max 
    (apply * (take 4 (drop x (t20 y)))) 
    (apply * (map #(nth % x 1) (take 4 (drop y t20))))
    (apply * (map #(nth (nth t20 (+ y %) []) (+ x %) 1) (range 4)))
    (apply * (map #(nth (nth t20 (- y %) []) (+ x %) 1) (range 4)))
)))))

;; Problem # 12 
;;
;;
;;
;;
;;
;;
;
(defn int-sqrt [x] (int (Math/sqrt x)))
(defn power-exact-divide [n x] (if (some zero? [n x]) 0 (if (zero? (mod n x)) (+ 1 (power-exact-divide (/ n x) x)) 0)))

(defn prime-factors-orig [n] (loop [sieve (sieve-primes (int-sqrt n)) x 2 res (sorted-map) res2 (sorted-map n 1)] 
                          (do (println sieve)
                          (if (empty? sieve) 
                            (merge res (apply dissoc res2
                                              (filter 
                                                #(some zero? (map mod (repeat %) (keys res))) 
                                                      (keys res2))))
                            (if (and (first sieve) (zero? (mod n x))) 
                              (recur (rest sieve) (inc x) (assoc res x (power-exact-divide n x)) (assoc res2 (/ n x) (power-exact-divide n (/ n x))))
                              (recur (rest sieve) (inc x) res res2))))))
(defn prime-factors [n] (loop [sieve (butlast (sieve-primes n)) x 2 res (sorted-map)]
                          (if (empty? sieve) res
                            (if (and (first sieve) (zero? (mod n x))) (recur (rest sieve) (inc x) (assoc res x (power-exact-divide n x))) (recur (rest sieve) (inc x) res)))))

(defn power-set-enumerate [myset] (let [cnts (map-indexed vector (map #(reduce * (repeat % 2)) (range (count myset))))]
                                    (for [x (range (reduce * (repeat (count myset) 2)))] (map #(nth myset (first %)) (filter #(pos? (bit-and x (second %))) cnts)))))

(defn nth-trinum-factors-count [n]  (let [prime-facs (apply merge-with + (map prime-factors (if (odd? n) [(-> n inc (/ 2)) n] [(/ n 2) (inc n)])))]
                                      (let [the-sum
                                      (loop [sum 0 pow-set (power-set-enumerate (keys prime-facs))] 
                                        (if-let [fst (first pow-set)] 
                                          (if (empty? fst) (recur (inc sum) (rest pow-set)) 
                                            (recur (+ sum (reduce * (map #(prime-facs %) fst))) (rest pow-set))) 
                                          sum))] the-sum)))
;                                      (do (println n " and its prime factors " prime-facs " and the total-num of factors " the-sum) the-sum))))




(defn main []
;        (nth-trinum-factors-count 12375))
;(comment
        (time (loop [x 10 y (nth-trinum-factors-count x)] 
          (if (< 500 y) 
            (println x " with " y " factors") 
            (recur (inc x) (nth-trinum-factors-count (inc x))))) ))
;  (println "prime factors of 170 " (prime-factors 170)))
(comment
	(time (println "110000 primes, 10001 prime " (mth-prime 110000 10001)))
	(println "1000000 primes" (last-prime 1000000))
	(println "1000000 primes, 10001 prime " (mth-prime 1000000 10001))
	(println "100000 primes, 10001 prime " (mth-prime 100000 10001))
	(time (println "110000 primes, 10001 prime " (mth-prime 110000 10001)))
	(println "110000 primes, 10001 prime " (mth-prime 110000 6))
        (println (sieve-primes 25))
	(println (sum-multiples-3-5<1000) " " (sum-3&5*<1000))
	(println (map #(do (println % " " (prime? %))) (range 100)))
 (println (largest-prime-factor-bad 600851475143)))

(main)


