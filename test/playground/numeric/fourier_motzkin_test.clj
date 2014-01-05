(ns playground.numeric.fourier-motzkin-test
  (:use midje.sweet
        playground.numeric.fourier-motzkin))

(fact "fourier-motzkin"
      (let [A [[ 1 -2]
               [ 1  1]
               [ 1  0]
               [-2  1]
               [-1  0]
               [ 0  8]]
            b [-2 3 2 0 0 15]]
        (fourier-motzkin-elimination A b 1) => [-1.5 0 8]
        (fourier-motzkin-elimination A b 2) => [[ 1 -2]
                                                [ 1  1]
                                                [ 1  0]
                                                [-2  1]
                                                [-1  0]
                                                [ 0  8]])
      ;; TODO add a few more known examples
      )
