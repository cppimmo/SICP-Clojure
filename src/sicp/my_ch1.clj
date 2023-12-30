(ns sicp.my-ch1)

(declare accumulate)

;;; Exercise 1.30

;;; Exercise 1.31

(defn product
  [term a next b]
  (accumulate * 1 term a next b))

;;; Exercise 1.32

(defn accumulate
  [combiner nil-value term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) (combiner result (term a)))))]
    (iter a nil-value)))

(defn sum-cubes
  [a b]
  (accumulate + 0 #(* % % %) a inc b))

;;; Exercise 1.34

(defn f [g] (g 2))

;;; If you try to call (f f), then it will try to use 2 as a function call operator.

;;; Exercise 1.35

(def ^:dynamic *tolerance* 0.00001)

(defn fixed-point
  [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) *tolerance*))
          (try-iter [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try-iter first-guess)))

(def phi (fixed-point #(+ 1 (/ 1 %)) 1.0))

;;; Exercise 1.36

(defn fixed-point
  [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) *tolerance*))
          (try-iter [guess]
            (let [next (f guess)]
              (println "Approx:" next)
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try-iter first-guess)))

;;; (fixed-point #(/ (Math/log 1000) (Math/log %)) 1.1)

;;; Exercise 1.37

(defn cont-frac
  [n d k]
  (letfn []))

;;; Exercise 1.40

(def ^:dynamic *dx* 0.00001)

(defn deriv
  [g]
  (fn [x]
    (/ (- (g (+ x *dx*)) (g x))
       *dx*)))

(defn newton-transform
  [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method
  [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic
  [a b c]
  (fn [x]
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;;; Exercise 1.41

(defn double
  [unary-op]
  (fn [arg]
    (unary-op (unary-op arg))))

;;; Exercise 1.42

(defn compose
  [f g]
  (fn [x]
    (f (g x))))

;;; Exercise 1.43

(defn repeated
  [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

;;; Exercise 1.44

(defn smooth
  [f]
  (fn [x]
    (/ (+ (f (- x *dx*))
          (f x)
          (f (+ x *dx*)))
       3)))

(defn n-fold-smooth
  [f n]
  (repeated (smooth f) n))

