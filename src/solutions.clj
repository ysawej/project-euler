

(defn mysolutions [] "My solutions to project euler in clojure")

;; Problem # 1 what is the sum of all multiples of 3 & 5 <1000?
(defn sum-multiples-of-3-and-5-lt-1000 [] 
  (letfn [ (multiples [n] (map #(* n %) (range (/ 1000 n))))] 
    (reduce + (filter #(> 1000 %) (distinct (concat (multiples 3) (multiples 5)))))))

(defn sum-multiples-of-3-and-5-lt-1000-better [] 
(loop [x3 3 x5 5 sigma 0 ] 
  (if (or (< x3 1000) (< x5 1000)) 
    (cond 
      (> x3 x5) (recur x3 (+ x5 5) (+ sigma x5)) 
      (= x3 x5) (recur (+ x3 3) (+ x5 5) (+ sigma x3)) 
      (< x3 x5) (recur (+ x3 3) x5 (+ sigma x3)))
    sigma)))

;; Problem # 2
(defn sum-even-valued-fibonacci-terms-lt-n [n] 
  (loop [[a b c] [0 1 0]] (if (> b n) c (recur [b (+ a b) (if (even? b) (+ c b) c)]))))

;; Problem # 3
(defn prime? [n] (not (reduce #(or %1 (zero? (mod n %2))) (concat '(false) (range (int (Math/sqrt n)) 1 -1)))))

(defn largest-prime-factor-bad [n] 
  (loop [x (int (Math/sqrt n))] (if (and (zero? (mod n x)) (prime? x)) x (recur (dec x))))) 
;(int  (Math/sqrt 600851475143))

;; Problem # 4
(defn palindrome? [number] 
  (let [ind-digits (fn [number] 
                     (loop [x [] number number] 
                       (if (zero? number) 
                         x 
                         (recur (cons (mod number 10) x) (int (/ number 10))))))
        digits (ind-digits number)]
    (= (reverse digits) digits)))

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
(let [arr (vec (repeat (dec n) true)) ];mymap (java.util.HashMap. {:N 1 :prime 2}) ] 
(loop [x 2 sieve arr i 0]
(if-not (<= (* x x) n) sieve (if-not (sieve i) (recur (inc x) sieve (inc i)) (recur (inc x) (apply assoc sieve (interleave (range (+ i x) n x) (repeat false))) (inc i)))))))

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
(defn sum-primes [n] 
  (let [sieve (sieve-primes n)] 
    (loop [sum 0 index 0] 
      (if (>= index (- n 2)) 
        sum 
        (if (sieve index) 
          (recur (+ sum index 2) (inc index)) 
          (recur sum (inc index)))))))

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
(defn int-sqrt [x] (int (Math/sqrt x)))

(defn power-exact-divide [n x] 
  (if (some zero? [n x]) 0 (if (zero? (mod n x)) (+ 1 (power-exact-divide (/ n x) x)) 0)))

; Assumes primes contain only primes and they are all < x
(defn make-lean-int [x primes]
  ;(do (println "x & primes are " x " " primes)
  (let [x2 (reduce #(/ %1 %2) (concat [x] (filter #(zero? (mod x %)) primes)))] 
    (if (= x2 x) x (make-lean-int x2 primes))));)

(println (make-lean-int 170 [2 5]))

; Good one, goes till sqrt n,  
(defn prime-factors-orig [n] 
  (loop [sieve (sieve-primes (int-sqrt n)) x 2 res (sorted-map) res2 (sorted-map n 1)] 
    (do (println sieve)
        (if (empty? sieve) 
          (merge res (apply dissoc res2
                            (filter 
                              #(some zero? (map mod (repeat %) (keys res))) 
                              (keys res2))))
          (if (and (first sieve) (zero? (mod n x))) 
            (recur (rest sieve) (inc x) (assoc res x (power-exact-divide n x)) (assoc res2 (/ n x) 1))
            (recur (rest sieve) (inc x) res res2))))))

(defn prime-factors-orig2 [n] 
  (loop [sieve (butlast (sieve-primes n)) x 2 res (sorted-map)]
    (if (empty? sieve) 
      res
      (if (and (first sieve) (zero? (mod n x))) 
        (recur (rest sieve) (inc x) (assoc res x (power-exact-divide n x))) 
        (recur (rest sieve) (inc x) res)))))

(defn prime-factors [n] 
  (loop [sieve (sieve-primes (int-sqrt n)) x 2 res (sorted-map) res2 (sorted-map n 1)] 
    ;(do (println "so far prime factors " res " for n" n)
    (if-not (empty? sieve) 
      (if (and (first sieve) (zero? (mod n x)))
        (recur (rest sieve) (inc x) (assoc res x (power-exact-divide n x)) (assoc res2 (/ n x) 1))
        (recur (rest sieve) (inc x) res res2))
      (dissoc 
        (merge 
          (reduce #(apply assoc %1 %2) {} (map #(vec [ (make-lean-int % (keys res)) 1 ]) (keys res2))) 
          res) 
        1))));)


(defn power-set-enumerate [myset] 
  (let [myvec (vec myset) 
        cnts (map-indexed vector (map #(reduce * (repeat % 2)) (range (count myvec))))]
    (for [x (range (reduce * (repeat (count myvec) 2)))] 
      (map #(nth myvec (first %)) (filter #(pos? (bit-and x (second %))) cnts)))))

(defn nth-trinum-factors-count [n]  
  (let [prime-facs (apply merge-with + 
                          (map prime-factors (if (odd? n) [(-> n inc (/ 2)) n] [(/ n 2) (inc n)])))]
    ;(let [the-sum
          (loop [sum 0 pow-set (power-set-enumerate (keys prime-facs))] 
            (if-let [fst (first pow-set)] 
              (if (empty? fst) (recur (inc sum) (rest pow-set)) 
                (recur (+ sum (reduce * (map #(prime-facs %) fst))) (rest pow-set))) 
              sum))
            ))
    ;      ] the-sum)))
;                                      (do (println n " and its prime factors " prime-facs " and the total-num of factors " the-sum) the-sum))))
;

; Problem # 13
;
(def nums (slurp "data/problem13"))
(defn rshift-10-digs [n] (long (/ n 10000000000)))
(defn str-arr-to-num [coll] (reduce #(+ (* %1 10) %2) (map #(- (int %) (int \0)) coll)))
(println "The answer to problem#13 is first 10 digits of "
         (+ (reduce + (map (comp str-arr-to-num first) (map #(partition 10 %) (clojure.string/split nums #"\s"))))
            (rshift-10-digs (reduce + (map (comp str-arr-to-num second) (map #(partition 10 %) (clojure.string/split nums #"\s")))))))

; Problem # 14
(defn Collatz-fn [n] (if (odd? n) (inc (* 3 n)) (/ n 2)))
(defn collatz-series-len [n] (loop [size 1 n n] (if (= n 1) size (recur (inc size) (Collatz-fn n)))))

(time (println "The answer to problem 14 is" (reduce #(if (> (second %1) (second %2)) %1 %2) (map #(vec [ % (collatz-series-len %)]) (range 1 1000001)))))

; Problem # 15
(time (println "The answer to problem 15 is " (reduce #(* %1 (first %2) (/ 1 (second %2))) 1 (partition 2 (interleave (range 40 20 -1) (range 20 0 -1))))))


; Problem # 16


(defn digit-arr-times [mydigseq times]
  (loop [myseq [] carry 0 toprocess mydigseq]
    (if (and (empty? toprocess) (zero? carry))
      myseq
      (let [dig (nth toprocess 0 0) newdig (+ (* dig times) carry)]
        (recur
          (conj myseq (mod newdig 10))
          (int (/ newdig 10))
          (rest toprocess))))))

(defn doubles-digit-arr [mydigseq] (digit-arr-times mydigseq 2))

;(println (double-dig-arr '(1)))
;(println (double-dig-arr '(8)))
;(println (double-dig-arr (double-dig-arr '(8))))

;(println (loop [init-seq '(1) iter 0] (if (< iter 10) (recur (double-dig-arr init-seq) (inc iter)) init-seq)))
;(println (loop [init-seq '(1) iter 0] (if (< iter 1000) (recur (double-dig-arr init-seq) (inc iter)) init-seq)))
(println "The answer to problem 16 is " 
         (reduce + (loop [init-seq '(1) iter 0] (if (< iter 1000) (recur (doubles-digit-arr init-seq) (inc iter)) init-seq))))


;; Problem # 17

(def single-digs '(3 3 5 4 4 3 5 5 4))
(def ten-to-teens '(3 6 6 8 8 7 7 9 8 8))
(def twenty-90 '(6 6 5 5 5 7 6 6))

(def total 
    (+
       (* 10 (reduce + (map #(reduce + %) [(map #(* 9 %) single-digs) ten-to-teens (map #(* 10 %) twenty-90)])))
       (+ (* 99 (reduce + (map #(+ 7 3 %) single-digs))) (reduce + (map #(+ 7 %) single-digs)))
       (+ 3 8)))

(println "The answer to problem 17 is  " total)


;; Problem # 18

(defn parseTriangleNumbers [filename] (map #(map (fn [a] (Integer/parseInt a)) %) (map #(clojure.string/split % #"\s") (clojure.string/split (slurp filename) #"\n"))))
(defn max-sum-triangle [triangle] 
  (let [toadd (fn [myseq] (partition 2 1 (concat [0] myseq [0])))]
    (loop [tolist (second triangle) fromlst (toadd (first triangle)) moretogo (rest (rest triangle))]
      (if (empty? moretogo)
      (map #(+ %1 (apply max %2)) tolist fromlst)
      (recur 
        (first moretogo) 
        (toadd (map #(+ %1 (apply max %2)) tolist fromlst))
        (rest moretogo))))))

(println "The answer to problem 18 is " (apply max (max-sum-triangle (parseTriangleNumbers "data/problem18"))))

;; Problem # 19

(defn days-in-month
      ([indx] (days-in-month indx 1)) 
      ([indx year] 
              (cond 
                                 (#{4 6 9 11} indx) 30  
                             (#{1 3 5 7 8 10 12} indx) 31  
                             (and (zero? (mod year 4)) (or (pos? (mod year 100)) (zero? (mod year 400)))) 29  
                             :default 28)))
(println "The answer to problem 19 is " 
         (loop [sun-months 0 mon 1 year 1901 weekday 2] 
           (if (and (>= mon 1) (>= year 2001)) 
             sun-months 
             (recur 
               (if (zero? weekday) (inc sun-months) sun-months) 
               (-> mon (mod 12) inc) 
               (if (= mon 12) (inc year) year) 
               (mod (+ weekday (days-in-month mon year)) 7)))))


;; Problem # 20

(println "The answer to problem 20 is " 
         (reduce + (loop [init-seq '(1) tocompute (range 1 101)] 
                     (if-not (empty? tocompute) 
                       (recur (digit-arr-times init-seq (first tocompute)) (rest tocompute)) 
                       init-seq))))



;; Problem # 21

(defn modfact [x y] ; x ('() sum) y dividend 
  (let [x1 (vec (first x)) x2 (second x)]
    [(conj x1 (mod x2 y) ) (int (/ x2 y))]))

(comment 
(defn factors-sum [n]  
  (let [prime-facs (prime-factors n)]
    (loop [sum 0 pow-set (power-set-enumerate (keys prime-facs))] 
      (if-let [fst (first pow-set)] 
        (if (empty? fst) (recur (inc sum) (rest pow-set)) 
          (recur (+ sum (reduce * (map #(prime-facs %) fst))) (rest pow-set))) 
        sum))))
)

;; Convenience function for use with reduce takes an list of empty-list-and-a-number, and dividend, 
;; (adds the remainder to the list, and sets the number to the quotient)
;; (modfact ['() 23] 5]) will return ['(3) 4]
;; So a reduce like: (reduce modfact ['() 29] [2 3 5]) will return the the individual components from [2 3 5] for a count 29 (assuming < 2*3*5)
;(defn modfact [x y] ; x ('() sum) y dividend 
;    [(cons (mod (second x) y) (first x)) (int (/ (second x) y))])
(defn atleast-1-from-each [factors repeats] 
    (map #(reduce * (mapcat repeat %1 %2)) (map  #(map inc (first (reduce modfact ['() %] repeats))) (range (reduce * repeats))) (repeat factors)))

(defn all-factors [n]
  (let [primes-fac (prime-factors n)
        factorss (power-set-enumerate (keys primes-fac))]
    (mapcat #(atleast-1-from-each % (map primes-fac %)) factorss)))

(defn amicable? [n] (let [m (reduce + (cons (- n) (all-factors n))) m2 (reduce + (cons (- m) (all-factors m)))] (and (= m2 n) (not (= m m2)) (<= m2 10000))))
(println "The answer to problem 21 is " (dec (reduce + (filter amicable? (range 1 10000)))))

;; Problem # 22

(println "The answer to problem 22 is " (reduce + (map #(* (first %) (reduce + (map (fn [a] (inc (- (int a) (int \A)))) (second %)))) (map-indexed #(vec [(inc %1) %2]) (sort (clojure.string/split (clojure.string/replace (slurp "data/names.txt") #"\"" "") #","))))) )

;; Problem # 67
(println "The answer to problem 67 is " (apply max (max-sum-triangle (parseTriangleNumbers "data/problem67"))))


;; Problem # 30
(range 10)
(defn to-digits [n] (cons (mod n 10) (let [k (int (/ n 10))] (if-not (zero? k) (lazy-seq (to-digits k))))))

(def numbers-with-sum-of-fifth-powers-of-individual-digits (filter (fn [a1] (= (reduce + (map #(nth (map (fn [x] (apply * (repeat 5 x))) (range 10)) %) (take 10 (to-digits a1)))) a1)) (range 2 1000000)))


;;;;;

(defn main []
;        (nth-trinum-factors-count 12375))
;(comment
        (time (loop [x 10 y (nth-trinum-factors-count x)]
          (if (< 500 y) 
            (println x " with " y " factors") 
            (recur (inc x) (nth-trinum-factors-count (inc x))))) ))
  ;(println "prime factors of 170 " (prime-factors 170)))
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


(println "hello world")
