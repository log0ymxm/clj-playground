(ns playground.core-test
  (:use midje.sweet
        playground.core))

(fact "math works"
      (+ 2 2) => 4)
