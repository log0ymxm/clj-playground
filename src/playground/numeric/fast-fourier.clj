(ns playground.numeric.fft)
;; http://en.wikipedia.org/wiki/Fast_Fourier_transform
;; http://www.mathworks.com/help/matlab/ref/fft.html

;; http://mathworld.wolfram.com/FastFourierTransform.html
;; http://mathworld.wolfram.com/FourierMatrix.html

;; https://code.google.com/p/clojure-contrib/source/browse/trunk/src/clojure/contrib/complex_numbers.clj?r=756
;; http://sicpinclojure.com/?q=sicp/2-4-1-representations-complex-numbers

;; http://www.altdevblogaday.com/2011/05/17/understanding-the-fourier-transform/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cooley-tukey

;; The Discrete Fourier Transform
;; X_k = \sum^{N - 1}_{n = 0} x_n e^{- \frac{2 \pi i}{N} n k}
;;
;; Radix-2 DIT
;; first compute the DFTs of even indexed inputs, then odd
;; X_k = \sum^{\frac{N}{2} - 1}_{m = 0} x_{2m} e^{- \frac{2 \pi i}{N} (2m) k}
;;       + \sum^{\frac{N}{2} - 1}_{m = 0} x_{2m} e^{- \frac{2 \pi i}{N} (2m+1) k}
;;
;; Can be done recursively

;; X_0,...,N−1 ← ditfft2(x, N, s):             DFT of (x_0, x_s, x_2s, ..., x_(N-1)s):
;;     if N = 1 then
;;         X_0 ← x_0                                      trivial size-1 DFT base case
;;     else
;;         X_0,...,N/2−1 ← ditfft2(x, N/2, 2s)             DFT of (x0, x2s, x4s, ...)
;;         X_N/2,...,N−1 ← ditfft2(x+s, N/2, 2s)           DFT of (xs, xs+2s, xs+4s, ...)
;;         for k = 0 to N/2−1                           combine DFTs of two halves into full DFT:
;;             t ← X_k
;;             X_k ← t + exp(−2πi k/N) X_{k+N/2}
;;             X_{k+N/2} ← t − exp(−2πi k/N) X_{k+N/2}
;;         endfor
;;     endif

(deftype complex [^double real ^double imaginary])

(defn is-power-2? [n]
  (and (not (= n 0))
       (= (bit-and n (dec n)) 0)))

(defn twiddle-factor [k N]
  ;; TODO i is imaginary not 1
  (let [i 1]
    (Math/exp (/ (* -2 Math/PI i k)
                 N))))

;; scaling factor? 1, (sqrt N), N

;; TODO this might be correct, need to try implementing complex numbers
(defn fast-fourier-transform [X]
  {:pre [(is-power-2? (count X))]}
  (let [N (count X)]
    (println "fft" X " -- N" N)
    (if (= N 1)
      X
      (let [first-range (fast-fourier-transform (keep-indexed #(if (even? %1) %2) X))
            second-range (fast-fourier-transform (keep-indexed #(if (odd? %1) %2) X))
            _ (println "--- 1, 2" first-range second-range)
            partitioned (partition 2 X)]
        ;;(println "partitioned" partitioned)
        (flatten
                (map (fn [a b k]
                       (let [factor (twiddle-factor k N)
                             a* (+ a factor)
                             b* (- a factor)]
                         (println "    - " factor "-" a* b*)
                         [a* b*]))
                     first-range
                     second-range
                     (range 0 (count first-range))))))))

;; http://stackoverflow.com/questions/6177744/what-is-correct-result-after-fft-if-input-array-is-0-1-2-3-4-5-6-7
;; (fft [0 1 2 3 4 5 6 7])
;;;;
;; {28.0000 + 0.0000i,
;;  -4.0000 + 9.6569i,
;;  -4.0000 + 4.0000i,
;;  -4.0000 + 1.6569i,
;;  -4.0000 + 0.0000i,
;;  -4.0000 - 1.6569i,
;;  -4.0000 - 4.0000i,
;;  -4.0000 - 9.6569i}

;; -- or -- might be with sqrt(N)
;; {9.89949 + 0.0000i
;;  -1.41421 - 3.41421i,
;;  -1.41421 - 1.41421i,
;;  -1.41421 - 0.585786i,
;;  -1.41421 + 0.0000i,
;;  -1.41421 + 0.585786i,
;;  -1.41421 + 1.41421i,
;;  -1.41421 + 3.41421i}

;; TODO try generating a set of data with mathematica
;; http://www.mathworks.com/help/matlab/ref/fft.html
