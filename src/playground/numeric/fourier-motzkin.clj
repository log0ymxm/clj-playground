(ns playground.numeric.fourier-motzkin)

;; http://en.wikipedia.org/wiki/Fourier-Motzkin_elimination
;; http://www.mathworks.com/matlabcentral/fileexchange/7957-fourier-motzkin-elimination

;; elimination in a system of linear inequalities
;; a = [1, -2;1,1;1,0;-2,1;-1,0;0,8]
;; b = [-2;3;2;0;0;15]
;; fourmotz(a,b,1)
;; => [-1.5; 0; 8]
;; fourmotz(a,b,2)
;; => [1, -2;1,1;1,0;-2,1;-1,0;0,8]

;; http://www.ms.uky.edu/~lee/ma515fa03/notes.pdf

(defn start
  "Opposite of rest, returns all elements of a seq except the last entry"
  [coll]
  (take (dec (count coll)) coll))

(defn first-count [f M]
  (count (filter f (map first M))))

(defn reduce-rows [M]
  (println "--- reduce-rows" M)
  (let [sorted (sort-by first M)
        negative-row-count (first-count #(< % 0) sorted)
        zero-row-count (first-count #(zero? %) sorted)
        positive-row-count (first-count #(> % 0) sorted)
        normalized (map normalize-by-first sorted)
        _ (println "rr normalized" normalized)
        combined (map concat normalized sorted)
        _ (println "combined" combined)
        ;; sort by second then first may not be necessary, and
        ;; doesn't seem to fully express what a sort in matlab
        ;; was doing
        normal-sorted (sort-by first (sort-by second normalized))
        _ (println "normal-sorted" normal-sorted)
        ]
    normalized))

(defn size [M]
  [(count M) (count (first M))])

(defn normalize-by-first
  "Will divide all values of a vector according to the first element"
  [a]
  ;;(println "normalize-by-first" a)
  (let [car (Math/abs (float (first a)))]
    (if (not (zero? car))
      (map #(/ % car) a)
      a)))

(defn scalar-matrix [k]
  (fn ! ([m n] (vec (take m (repeat (! n)))))
    ([m] (vec (take m (repeat k))))))

(def zeros (scalar-matrix 0))
(def ones (scalar-matrix 1))

(defn fourmotz-elimination
  [M & k]
  (let [A (map start M)
        b (map last M)
        to-eliminate (or k 1)
        [m n] (size A)]
    (println A b m n)
    (if (> n to-eliminate)
      (let [sorted (sort-by first M)
            _ (println M sorted)

            negative-row-count (first-count #(< % 0) sorted)
            zero-row-count (first-count #(zero? %) sorted)
            positive-row-count (first-count #(> % 0) sorted)
            _ (println "row counts: - " negative-row-count
                       " 0 " zero-row-count
                       " + " positive-row-count)

            negative-rows (filter #(< (first %) 0) sorted)
            zero-rows (filter #(zero? (first %)) sorted)
            positive-rows (filter #(> (first %) 0) sorted)

            pos-neg-zero-temp (concat positive-rows
                                      negative-rows
                                      zero-rows)
            _ (println "pos-neg-zero-temp" pos-neg-zero-temp)

            normalized-temp (map normalize-by-first pos-neg-zero-temp)
            _ (println "normalized-temp" normalized-temp)

            normalized-negative-rows (filter #(< (first %) 0) normalized-temp)
            normalized-zero-rows (filter #(zero? (first %)) normalized-temp)
            normalized-positive-rows (filter #(> (first %) 0) normalized-temp)

            ;; create a result zeros array the size of the reduced result
            ;; zeros ((num-pos*num-neg)+num-zero) n
            result-zeros (zeros (+ (* positive-row-count
                                      negative-row-count)
                                   zero-row-count)
                                n)
            _ (println "result-zeros" result-zeros)
            _ (println "----")
            _ (println (ones negative-row-count 1))
            _ (println (range 2 (inc n)))
            _ (println (nth normalized-temp 0))
            _ (println (map #(map +
                                  (rest (nth normalized-temp 0))
                                  (rest %))
                            normalized-negative-rows))

            calculations (mapcat
                           (fn [i]
                             (map #(map +
                                        (rest (nth normalized-temp i))
                                        (rest %))
                                  normalized-negative-rows))
                           (range 0 positive-row-count))
            _ (println "calculations" calculations)

            with-appended-inequalities (concat calculations
                                               (map rest normalized-zero-rows))

            _ (println "with-appended-inequalities" with-appended-inequalities)

            ;; TODO here
            ;; still need to properly implement reduce-rows
            reduced (reduce-rows with-appended-inequalities)
            _ (println "reduced" reduced)

            temp-A (map start reduced)
            temp-b (map last reduced)
            _ (println "tmp-A" temp-A "tmp-b" temp-b)
            recursive-result (fourmotz-elimination temp-A temp-b to-eliminate)
            _ (println "recursive-result" recursive-result)
            ]
        recursive-result)
      (if (> n 0)
        (reduce-rows (map concat A b))
        [A b]))))
