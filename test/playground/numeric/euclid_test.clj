(ns playground.numeric.euclid-test
  (:use midje.sweet
        playground.numeric.euclid))

(fact "Euclid's Algorithm produces the GCD"
      (gcd-subtraction 20 30) => 10
      (gcd-subtraction 309 4005) => 3

      (gcd-division 20 30) => 10
      (gcd-division 309 4005) => 3

      (gcd-reduce 309 4005) => 3
      (gcd-reduce 20 30 60 70) => 10
      (gcd-reduce 20 30 60 70 25) => 5

      (gcd-idiomatic 20 30) => 10
      (gcd-idiomatic 309 4005) => 3

      (gcd 309 4005) => 3
      (gcd 20 30 60 70) => 10
      (gcd 20 30 60 70 25) => 5)
