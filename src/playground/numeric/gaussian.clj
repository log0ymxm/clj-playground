(ns playground.numeric.gaussian)
;; http://en.wikipedia.org/wiki/Gaussian_elimination

;; TODO clean code, test code, make sure it's readable

;; https://github.com/skatenerd/gaussian_elimination
;; https://github.com/mjm/clojure-code/blob/master/math/linalg_rowred.clj

;; TODO the pseudocode on wikipedia has a bug
;;
;; A = Matrix(m x n)
;; T = solved row-echelon matrix
;;
;;
;; for k = 1 ... m:
;;   Find pivot for column k:
;;   i_max  := argmax (i = k ... m, abs(A[i, k]))
;;   if A[i_max, k] = 0
;;     error "Matrix is singular!"
;;   swap rows(k, i_max)
;;   Do for all rows below pivot:
;;     for i = k + 1 ... m:
;;       Do for all remaining elements in current row:
;;         for j = k ... n:
;;           A[i, j]  := A[i, j] - A[k, j] * (A[i, k] / A[k, k])
;;       Fill lower triangular matrix with zeros:
;;       A[i, k]  := 0
;;;;;;
;;
;; A = [[2 3 4 5], [2 4 5 6], [1 3 5 6]]
;;
;; solve(a)
;; m = 3
;; -> for k = 1
;;    -> i_max = argmax(i=[1,2,3], abs(A[i,1])) = 1
;;       -> A[1,1] == 2
;;       -> A[2,1] == 2
;;       -> A[3,1] == 1
;;       -> max(i) = 1
;;    -> swap rows(k=1,i_max=1) ;; no change
;;    -> for i=2
;;       -> for j=1
;;          -> A[i=2,j=1] = A[i=2,j=1] - A[k=1,j=1] * (A[i=2,k=1] / A[k=1,k=1])
;;          -> A[2,1] = 2 - 2 * (2 / 2) = 0
;;       -> for j=2
;;          -> A[i=2,j=2] = A[i=2,j=2] - A[k=1,j=2] * (A[i=2,k=1] / A[k=1,k=1])
;;          -> A[2,2] = 4 - 3 * (0 / 2) = 4
;;       -> for j=3
;;          -> A[i=2,j=3] = A[i=2,j=3] - A[k=1,j=3] * (A[i=2,k=1] / A[k=1,k=1])
;;          -> A[2,3] = 5 - 4 * (0 / 2) = 5
;;       -> for j=4
;;          -> A[i=2,j=4] = A[i=2,j=4] - A[k=1,j=4] * (A[i=2,k=1] / A[k=1,k=1])
;;          -> A[2,4] = 6 - 5 * (0 / 2) = 6
;;    -> A = [[2 3 4 5], [0 4 5 6], [1 3 5 6]]
;;    -> for i=3
;;       -> for j = 1
;;          -> A[i=3,j=1] = A[i=3,j=1] - A[k=1,j=1] * (A[i=3,k=1] / A[k=1,k=1])
;;          -> A[3,1] = 1 - 2 * (1 / 2) = 0
;;       -> for j = 2
;;          -> A[i=3,j=2] = A[i=3,j=2] - A[k=1,j=2] * (A[i=3,k=1] / A[k=1,k=1])
;;          -> A[3,2] = 3 - 3 * (0 / 2) = 3
;;       -> for j = 3
;;          -> A[i=3,j=3] = A[i=3,j=3] - A[k=1,j=3] * (A[i=3,k=1] / A[k=1,k=1])
;;          -> A[3,3] = 5 - 4 * (0 / 2) = 5
;;       -> for j = 4
;;          -> A[i=3,j=4] = A[i=3,j=4] - A[k=1,j=4] * (A[i=3,k=1] / A[k=1,k=1])
;;          -> A[3,4] = 6 - 5 * (0 / 2) = 6
;;    -> A = [[2 3 4 5], [0 4 5 6], [0 3 5 6]]
;; -> A = [[2 3 4 5], [0 4 5 6], [0 3 5 6]]
;; -> for k = 2
;;    -> i_max = argmax(i=[2,3], abs(A[i,2]))
;;       -> A[2,2] == 4
;;       -> A[3,2] == 3
;;       -> max(i) = 2
;;    -> swap rows(k=2,i_max=2) ;; no change
;;    -> for i = 3
;;       -> for j = 2
;;          -> A[i=3,j=2] = A[i=3,j=2] - A[k=2,j=2] * (A[i=3,k=2] / A[k=2,k=2])
;;          -> A[3,2] = 3 - 4 * (3 / 4) = 0
;;       -> for j = 3
;;          -> A[i=3,j=3] = A[i=3,j=3] - A[k=2,j=3] * (A[i=3,k=2] / A[k=2,k=2])
;;          -> A[3,3] = 5 - 5 * (0 / 4) = 5
;;       -> for j = 4
;;          -> A[i=3,j=4] = A[i=3,j=4] - A[k=2,j=4] * (A[i=3,k=2] / A[k=2,k=2])
;;          -> A[3,4] = 6 - 6 * (0 / 4) = 6
;; -> A = [[2 3 4 5], [0 4 5 6], [0 0 5 6]]
;; -> for k = 3
;;
;; I don't think this pseudocode is accurate, it feels way off...

;; http://clojure.roboloco.net/?p=587
(defn argmax
  "Returns the argument in collection that produces the
maximum result of a function f"
  [f coll]
  ;;(println "argmax" coll)
  (reduce (fn [a b] (if (> (f a) (f b)) a b))
          coll))

(defn column
  "Returns the ith column from a matrix as a vector"
  [M i]
  (map (fn [m] (nth m i)) M))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elementary Row Operations

(defn interchange-rows
  "Produce a new matrix `M` with rows `a` & `b` swapped"
  [M a b]
  (assoc M b (M a) a (M b)))

(defn scaler-multiply
  "Return the scalar multiplication `s` of a row `a`
from the matrix `M1"
  [M s a]
  ;;(println "scaler-multiply" s a)
  (map #(* % s) (M a)))

(defn add-row-to-row
  "Adds the values of a row `a` from the matrix `M` to
the row `b` returning the updated matrix. Assumes that
`a` is the values of the row, and `b` is the index of
the row to be operated on."
  [M a b]
  (assoc M b (vec (map (fn [a_i b_i] (+ a_i b_i))
                       a (M b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-pivot
  "Finds the leading coefficient for a column passed in
as a vector"
  [column args]
  ;;(println "find-pivot" column args)
  (if (not (empty? args))
    (argmax #(Math/abs (nth column %)) args)
    args))

(defn partial-pivot
  [M pivot]
  (let [max-pivot (find-pivot (column M pivot)
                              (range pivot (count M)))]
    (interchange-rows M pivot max-pivot)))

(defn scale-factor [M i j]
  (- (/ (get-in M [i j])
        (get-in M [j j]))))

(defn eliminate [M pivot]
  (let [m (count M)]
    (reduce (fn [M i]
              ;;(println "   -- eliminate reduce fn" M i)
              (let [scaler-factor (scale-factor M i pivot)
                    scaled-row (scaler-multiply M scaler-factor pivot)]
                (add-row-to-row M scaled-row i)))
            M (range (inc pivot) m))))

(defn row-echelon-form
  "Computes the row-echelon form of a matrix, `A`,
using gaussian elimination."
  [A]
  (let [m (count A)]
    ;;(println "mxn" m n)
    (reduce (fn [A pivot]
              ;;(println "- pivot reduce fn" A pivot)
              (let [B (partial-pivot A pivot)]
                ;;(println "   - after pivot" B)
                (eliminate B pivot)))
            A (range 0 m))))

;; -> numeric git:(master) ✗ python ng.py
;; start [[2.0, -3.0, -8.0], [3.0, 4.0, 5.0]]
;; m x n 2 3
;; maxrow 1
;; partial-pivot 1 0
;; maxrow 1
;; partial-pivot 1 1
;; -----------
;; A before back-substitute [[3.0, 4.0, 5.0], [0.0, -5.666666666666666, -11.333333333333332]]
;; k range [1, 0]
;; k [[3.0, 4.0, 5.0], [0.0, -5.666666666666666, -11.333333333333332]] 1
;; c -5.66666666667
;; i range [0]
;;    - i [[3.0, 4.0, 5.0], [0.0, -5.666666666666666, -11.333333333333332]] 0
;;    j range [2, 1]
;;       - j [[3.0, 4.0, 5.0], [0.0, -5.666666666666666, -11.333333333333332]] 2
;;         - after [[3.0, 4.0, -3.0], [0.0, -5.666666666666666, -11.333333333333332]] 2
;;       - j [[3.0, 4.0, -3.0], [0.0, -5.666666666666666, -11.333333333333332]] 1
;;         - after [[3.0, 0.0, -3.0], [0.0, -5.666666666666666, -11.333333333333332]] 1
;; - k after [[3.0, 0.0, -3.0], [0.0, 1.0, -11.333333333333332]]
;; j range [2]
;;    - normalize range [[3.0, 0.0, -3.0], [0.0, 1.0, -11.333333333333332]] 2
;;     - normalize after [[3.0, 0.0, -3.0], [0.0, 1.0, 2.0]] 2
;; k [[3.0, 0.0, -3.0], [0.0, 1.0, 2.0]] 0
;; c 3.0
;; i range []
;; - k after [[1.0, 0.0, -3.0], [0.0, 1.0, 2.0]]
;; j range [2]
;;    - normalize range [[1.0, 0.0, -3.0], [0.0, 1.0, 2.0]] 2
;;     - normalize after [[1.0, 0.0, -1.0], [0.0, 1.0, 2.0]] 2
;; True
;; end [[1.0, 0.0, -1.0], [0.0, 1.0, 2.0]]
(defn some-factor [A i k c]
  ;;(println "normalize factor" i k c)
  (let [f
        (- (/ (get-in A [i k])
              c))]
    ;;(println f)
    f))

(defn back-substitute
  "Will use a process of back-substitution to
reduce a row-echelon matrix"
  [A]
  (let [m (count A)
        n (count (first A))
        k-range (reverse (range 0 m))]
    (println "--- back-substitute")
    (println "k range" k-range)
    (reduce (fn [A k]
              (let [scale-factor (get-in A [k k])
                    i-range (range 0 k)]
                (println "k back-sub reduce" A k)
                (println "c" (* 1.0 scale-factor))
                (println "i range" i-range)

                (reduce ; normalize (range m n)
                 (fn [A j]
                   (assoc-in A [k j] (/ (get-in A [k j])
                                        scale-factor)))
                 (assoc-in ; A[k][k] /= c
                  (reduce (fn [A i]
                            (let [j-range (reverse (range k n))]
                              (println "   - i inner reduce" A i)
                              (println "   j range" k n j-range)
                              (reduce (fn [A j]
                                        (println "      -- inner inner reduce" j)
                                        (let [some-factor (some-factor A i k
                                                                       scale-factor)
                                              scaled-row (scaler-multiply A
                                                                          some-factor
                                                                          k)]
                                          (println "       -- factor scaled"
                                                   some-factor
                                                   scaled-row)
                                          (add-row-to-row A scaled-row i)))
                                      A j-range)))
                          A i-range)
                  [k k] (/ (get-in A [k k])
                           scale-factor))
                 (range m n))))
            A k-range)))

(defn reduced-row-echelon-form
  "Computes the reduced row-echelon form of a matrix, `A`, using gaussian elimination and back-substitution."
  [M]
  (back-substitute (row-echelon-form M)))

;;;;;;;;;
;; GaussianElimination[m_?MatrixQ, v_?VectorQ] :=
;;     Last /@ RowReduce[Flatten /@ Transpose[{m, v}]]
;;
;; row reduce is pretty much everything here
;;
;;


;; elementary row and column operations
;; http://mathworld.wolfram.com/ElementaryRowandColumnOperations.html
;;
;; 1. Interchanging two rows or columns,
;; 2. Adding a multiple of one row or column to another,
;; 3. Multiplying any row or column by a nonzero element.



;; 1. find row-echelon matrix

;; 2. use back-substitution to find solution from a row-echelon matrix
;;    finding the solution set of a system of linear equations

;; TODO Finding the determinant of a square matrix
;;   - find the determinant of the row-echelon matrix
;;   - elementary row operations will change the determinant
;;   - reverse elementary row ops
;; TODO Finding the inverse of an invertible square matrix
;;   - A
;;   - augment original matrix with the identity matrix
;;   - [ A | I ]
;;   - if you can find the reduced row matrix, then
;;   - [ I | B ]
;;   - B is the inverse of A
;; TODO Finding the rank of a matrix
;;   - number of non-zero rows in row echelon matrix
