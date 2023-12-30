(ns sicp.ex-1-22)

(declare divides?)

(defn smallest-divisor
  [n]
  (loop [n n, test-divisor 2]
    (cond (> (* test-divisor test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (recur n (inc test-divisor)))))

(defn prime?
  [n]
  (= n (smallest-divisor n)))

(defn search-for-primes
  [start end]
  (filter (fn [n]
            (and (odd? n) (prime? n)))
          (range start end)))

(defn divides?
  [a b]
  (= (rem b a) 0))

