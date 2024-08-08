(ns sicp.my-ch2)

;;; Exercise 2.1

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (rem a b))))

(defn make-rat [n d]
  (let [g (Math/abs (gcd n d))
        m (if (neg? d) (* -1 g) g)]
    [(/ n m) (/ d m)]))

;;; Exercise 2.2

(defn make-segment
  [p1 p2]
  [p1 p2])

(def start-segment first)
(def end-segment second)

(defn make-point
  [x y]
  [x y])

(def x-point first)
(def y-point second)

(defn midpoint-segment
  [s]
  (make-point
   (/ (+ (x-point (start-segment s))
         (x-point (end-segment s)))
      2)
   (/ (+ (y-point (start-segment s))
         (y-point (end-segment s)))
      2)))

(defn print-point
  [p]
  (printf "(%d, %d)\n" (x-point p) (y-point p)))

;;; Exercise 2.3

(defn make-rect
  [ul ur dl dr] ; Upper-left, upper-right, down-left, down-right
  )

;;; Exercise 2.4

(defn my-cons [x y]
  (fn [m] (m x y)))

(defn my-car [lst]
  (lst (fn [x y] x)))

(defn my-cdr [lst]
  (lst (fn [x y] y)))

;;; Exercise 2.5

(defn my-cons [a b]
  (* (Math/pow 2 a)
     (Math/pow 3 b)))

(defn divides-count [n d]
  (loop [n (int n)
         cnt 0]
    (if (not= (mod n d) 0)
      cnt
      (recur (/ n d) (inc cnt)))))

(defn my-car [pair]
  (divides-count pair 2))

(defn my-cdr [pair]
  (divides-count pair 3))

;;; Exercise 2.6

(defn zero
  [f]
  (fn []))

;;; Exercise 2.7

(defn make-interval
  [a b]
  [a b])

(defn upper-bound
  [interval]
  (second interval))

(defn lower-bound
  [interval]
  (first interval))

;;; Exercise 2.8

(defn sub-interval
  [a b]
  (make-interval (max (- (upper-bound a)
                         (lower-bound b))
                      (- (upper-bound b)
                         (lower-bound a)))
                 (min (- (lower-bound a)
                         (upper-bound b))
                      (- (lower-bound b)
                         (upper-bound a)))))

;;; Exercise 2.9



;;; Exercise 2.10



;;; Exercise 2.11



;;; Exercise 2.12



;;; Exercise 2.17

(defn last-pair [coll]
  (if (next coll)
    (recur (rest coll))
    coll))

;;; Exercise 2.18

(defn reverse [coll]
  (loop [c coll, acc '()]
    (if c
      (recur (next c)
             (conj acc (first c)))
      acc)))

;;; Exercise 2.19

(def first-denomination first)
(def except-first-denomination next)
(def no-more? nil?)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values))))

;;;

(defn append [coll-a coll-b]
  (if (empty? coll-a)
    coll-b
    (cons (first coll-a) (append (rest coll-a) coll-b))))

;;; Exercise 2.20

(defn same-parity [x & xs]
  (cons x
        (filter (if (even? x) even? odd?) xs)))

;;; Exercise 2.21

(defn square [x] (* x x))
(defn square-list [coll]
  (if (nil? coll)
    nil
    (cons (square (first coll))
          (square-list (next coll)))))
(defn square-list [coll]
  (map square coll))

;;; Exercise 2.22

;;; Exercise 2.23

(defn for-each [f coll]
  (if (nil? coll)
    nil
    (do (f (first coll))
        (for-each f (next coll)))))
;;; (for-each println (range 1 (inc 5)))

;;; Exercise 2.24

; '(1 (2 (3 4)))
;  (1     (2 (3 4)))
;   1     2
;        / \
;       3  4

;;; Exercise 2.25

(comment
  (let [lst '(1 3 (5 7) 9)]
               (rest (first (rest (rest lst)))))
  (let [lst '((7))]
               (first (first lst)))
  (let [lst '(1 (2 (3 (4 (5 (6 7))))))]
    lst))

;;; Exercise 2.26

(comment
  (let [x (list 1 2 3)
        y (list 4 5 6)]
    (println "(append x y):" (= (append x y) (list 1 2 3 4 5 6)))
    (println "(cons x y):" (= (cons x y) '((1 2 3) 4 5 6)))
    (println "(list x y):" (= (list x y) '((1 2 3) (4 5 6))))))

;;; Exercise 2.27

(defn deep-reverse [coll]
  (if (seq? coll)
    (reverse (map deep-reverse coll))
    coll))

;;; Exercise 2.28

(defn fringe [tree]
  (reduce (fn [acc elem]
            (if (seq? elem)
              (concat acc (fringe elem))
              (conj acc elem)))
          []
          tree))

(comment
  (let [x '((1 2) (3 4))]
    (println "(fringe x):" (fringe x))
    (println "(fringe x x):" (fringe (list x x)))))

;;; Exercise 2.29

(defn make-mobile [left right] (list left right))
(defn make-branch [length structure] (list length structure))
(defn left-branch [mobile] (first mobile))
(defn right-branch [mobile] (second mobile))
(defn branch-length [branch] (first branch))
(defn branch-structure [branch] (second branch))

