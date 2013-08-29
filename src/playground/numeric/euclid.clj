(ns playground.numeric.euclid)

;; http://en.wikipedia.org/wiki/Euclidean_algorithm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gcd-subtraction [a b]
  (if (= a 0)
    b
    (if (= b 0)
      a
      (if (not (= a b))
        (if (> a b)
          (recur (- a b) b)
          (recur a (- b a)))
        a))))

(defn trace-gcd-subtraction [a b]
  (println "gcd-subtraction" a b)
  (if (= a 0)
    b
    (if (= b 0)
      a
      (if (not (= a b))
        (if (> a b)
          (recur (- a b) b)
          (recur a (- b a)))
        a))))
(println "tracing gcd-subtraction")
(trace-gcd-subtraction 1071 462)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gcd-division [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn trace-gcd-division [a b]
  (println "gcd-division" a b)
  (if (= b 0)
    a
    (recur b (mod a b))))
(println "tracing gcd-division")
(trace-gcd-division 1071 462)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-function [fun times a-range b-range]
  (doall
   (for [i (range 0 times)
         a a-range
         b b-range]
     (fun a b))))

(println "timing gcd-subtraction")
(time
 (repeat-function gcd-subtraction 100 (range 0 100) (range 100 200)))
(println "timing gcd-division")
(time
 (repeat-function gcd-division 100 (range 0 100) (range 100 200)))

(def gcd-s-memoized
  (memoize
   (fn [a b]
     (if (= a 0)
       b
       (if (= b 0)
         a
         (if (not (= a b))
           (if (> a b)
             (recur (- a b) b)
             (recur a (- b a)))
           a))))))

(def gcd-d-memoized
  (memoize
   (fn [a b]
     (if (= b 0)
       a
       (recur b (mod a b))))))

(println "timing gcd-s-memoized")
(time
 (repeat-function gcd-s-memoized 100 (range 0 100) (range 100 200)))
(println "timing gcd-d-memoized")
(time
 (repeat-function gcd-d-memoized 100 (range 0 100) (range 100 200)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic arguments length
;; associative law: http://en.wikipedia.org/wiki/Associative_property
;; f(f(x,y),z) = f(x,f(y,z))
;; (1 + 2) + 3 = 1 + (2 + 3)
;; (1 * 2) * 3 = 1 * (2 * 3)

;; Since GCD is associative, GCD(a,b,c,d) is the same as GCD(GCD(GCD(a,b),c),d)
(defn gcd-reduce [& numbers]
  (reduce gcd-division numbers))

(println "timing gcd-reduce")
(time
 (repeat-function gcd-reduce 100 (range 0 100) (range 100 200)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure idiomatic version
;; http://clojuredocs.org/clojure_contrib/clojure.contrib.graph/fixed-point
(defn fixed-point
  "Repeatedly apply fun to data until (equal old-data new-data)
   returns true.  If max iterations occur, it will throw an
   exception.  Set max to nil for unlimited iterations."
  [data fun max equal]
  (let [step (fn step [data idx]
               (when (and idx (= 0 idx))
                 (throw (Exception. "Fixed point overflow")))
               (let [new-data (fun data)]
                 (if (equal data new-data)
                   new-data
                   (recur new-data (and idx (dec idx))))))]
    (step data max)))

;; mathematica version
;; functional version: http://mathworld.wolfram.com/EuclideanAlgorithm.html
;;
;; Remainder[a_, b_] := Mod[a, b]
;; Remainder[a_, 0] := 0
;; EuclideanAlgorithmGCD[a_, b_] := FixedPointList[
;;   {Last[#], Remainder @@ #}&, {a, b}][[-3, 1]]

(defn gcd-idiomatic [a b]
  (let [remainder (fn [[a b]]
                    (if (= b 0)
                      [a 0]
                      [b (mod a b)]))]
    ;; NOTE may be able to use iterate here, would be much cleaner
    (first (fixed-point [a b] remainder nil =))))

(println "timing gcd-idiomatic")
(time
 (repeat-function gcd-idiomatic 100 (range 0 100) (range 100 200)))

(defn gcd-i-reduce [& values]
  (reduce gcd-idiomatic values))

(println "timing gcd-i-reduce")
(time
 (repeat-function gcd-i-reduce 100 (range 0 100) (range 100 200)))

;; Looks like the non-memoized division version of the algorithm is the
;; most efficient for our simple needs, let's accept that as our default
;; and call it gcd.
(def gcd gcd-reduce)
