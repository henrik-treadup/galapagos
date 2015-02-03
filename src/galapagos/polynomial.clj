(ns galapagos.polynomial
  (:use [galapagos.util])
  (:gen-class))

;
; The polynomial part.
;

;
; Polynomials are represented by vectors of coefficients. 
; The lowest coefficient  i.e. the constant coefficient 
; is stored in the first element of the vector.
;

(defn deg 
  "Calculates the degree of the polynomial."
  [poly]
  (dec (count poly)))

(defn eval-poly 
  "Evaluates the polynomial for the given point."
  [poly x]
   (cond
      (= (deg poly) 0) 
         (first poly)
      :else
         (+ (first poly) (* x (eval-poly (rest poly) x)))))

(defn poly-to-function 
  "Converts a polynomial to a normal clojure function."
  [poly]
  (fn [x] (eval-poly poly x)))

(defn add-poly 
  "Adds two polynomials."
  [p1 p2]
   (cond
      (empty? p1) p2
      (empty? p2) p1
      :else (cons (+ (first p1) (first p2)) (add-poly (rest p1) (rest p2)))))

; Multiplies the polynomials p1 and p2. Returns a list where each element is a pair. The first part of the pair is
; the coefficent. The second part of the pair is the exponent. There can (and will be in most of the cases) pairs that
; have the same exponent.
(defn mul-poly-expanded 
  "Multiplies the polynomials p1 and p2. Returns a list of pairs of coefficients and exponents.
   There can be multiple pairs with the same exponent."
  [p1 p2]
  (for [i (range (inc (deg p1))) j (range (inc (deg p2)))] (list (* (p1 i) (p2 j)) (+ i j))))

; Calculates the nth term of the polynomial. 
(defn nth-term 
  "Takes a list of coefficient and exponent pairs and calculates the nth term
   of the resulting polynomial."
  [result n]
  (apply + (for [r result :when (= n (second r))] (first r))))

(defn mul-poly 
  "Multiplies the polynomials p1 and p2."
  [p1 p2]
   (let [new-degree (+ (deg p1) (deg p2))
         result (mul-poly-expanded p1 p2)]
     (into [] (for [k (range (inc new-degree))] (nth-term result k)))))

(defn abs 
  "Calculates the absolute value of x"
  [x] (max x (- x)))

(defn poly-difference 
  "Calculates the difference between the polynomials p1 and p2 at the point x."
  [p1 p2 x]
  (- (eval-poly p1 x) (eval-poly p2 x)))
  
(defn poly-difference-multi 
  "Calculates the difference between the polynomials p1 and p2 at multiple points."
  [p1 p2 points]
  (if (= 0 (count points))
    '()
    (conj 
     (poly-difference-multi p1 p2 (rest points))
     (poly-difference p1 p2 (first points)))))

(defn linear-polynomial 
  "Constructs a linear polynomial given the root of the linear polynomial."
  [root]
  [(- root) 1.0])

(defn polynomial-from-roots
  "Generates a polynomial with the given roots. The highest coefficient is 1.0"
  [roots]
  (if (= 1 (count roots))
    (linear-polynomial (first roots))
    (mul-poly (linear-polynomial (first roots)) 
              (polynomial-from-roots (rest roots)))))

