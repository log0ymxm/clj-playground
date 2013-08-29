(ns playground.numeric.euclid-test
  (:use midje.sweet
        playground.numeric.gaussian))

(fact "Gaussian Elimination produces the row-echelon form of a matrix"
      (let [A [[2 -3 -8]
               [3  4  5]]]

        (row-echelon-form A) => [[3     4     5]
                                 [0 -17/3 -34/3]]

        (reduced-row-echelon-form A) => [[1  0 -1]
                                         [0  1  2]]))
