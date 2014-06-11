(ns playground.numeric.factorial)

;; http://mishadoff.github.io/blog/fast-factorial/
(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
      (fn primes-from [n [f & r]]
        (if (some #(zero? (rem n %))
              (take-while #(<= (* % %) n) primes))
          (recur (+ n f) r)
          (lazy-seq (cons n (primes-from (+ n f) r)))))
      wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
            6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
            2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn power [x n]
  (cond (= 0 n) 1
        (= 1 n) x
        (even? n) (power (*' x x) (/ n 2))
        (odd? n) (*' x (power (*' x x) (/ (dec n) 2)))))

(defn- find-power [n k]
  (loop [total n sum 0]
    (let [i (int (/ total k))]
      (if (zero? i) sum
          (recur i (+ sum i))))))

(defn ! [n]
  (loop [[h & t]
         (map #(power % (find-power n %))
              (take-while #(<= % n) primes))
         acc 1]
    (if h (recur t (*' h acc)) acc)))
