(ns playground.data-structures.binary-trees-test
  (:use midje.sweet
        playground.data-structures.binary-trees))

(def unbalanced-example
  [2 [7 [2] [6 [5] [11]]]
   [5 [] [9 [4] []]]])

(def balanced-example
  [2 [7 [2] [6 [5] [11]]]
   [5 [8] [9 [4] []]]])

(fact "We can define what a tree is"
      (tree? []) => true
      (tree? [6]) => true
      (tree? 6) => false
      (tree? {}) => false
      (tree? nil) => false
      (tree? balanced-example) => true
      (tree? unbalanced-example) => true
      (tree? [8 [] []]) => true
      (tree? [8 {} []]) => false
      (tree? [8 [] {}]) => false
      )

(fact "We can interact with the root of a tree"
      (root balanced-example) => 2
      (root unbalanced-example) => 2
      (root []) => nil
      (root [6]) => 6
      )

(fact "We can tell if a tree has children"
      (has-children? balanced-example) => true
      (has-children? unbalanced-example) => true
      (has-children? []) => false
      (has-children? [6]) => false
      (has-children? [9 [] []]) => true
      )

(fact "We can interact with the child nodes"
      (left-child []) => nil
      (right-child []) => nil

      (left-child unbalanced-example) => [7 [2] [6 [5] [11]]]
      (left-child balanced-example) => [7 [2] [6 [5] [11]]]

      (right-child unbalanced-example) => [5 [] [9 [4] []]]
      (right-child balanced-example) => [5 [8] [9 [4] []]]
      )

(fact "We can find the height of a tree"
      (height []) => 0
      (height [9]) => 1
      (height [9 [] []]) => 1
      (height [9 [2] []]) => 2

      (height unbalanced-example) => 4
      (height balanced-example) => 4)

(fact "We can tell if a tree is balanced"
      (balanced? []) => true
      (balanced? [9]) => true
      (balanced? [9 [] []]) => true
      (balanced? [9 [2] []]) => true
      (balanced? [9 [2 [] [3]] []]) => false
      (balanced? balanced-example) => true
      (balanced? unbalanced-example) => false)

(fact "We can balance a tree if it's unbalanced"
      (balance []) => []
      (balance [9]) => [9]
      (balance balanced-example) => balanced-example
      ;; TODO
      ;;(balance unbalanced-example) => [2 [7 [2] [6 [5] [11]]] [9 [5] [4]]]
      (balanced? (balance unbalanced-example)) => true
      )

(fact "We can test if a tree is complete"
      (complete? []) => false
      (complete? [9]) => true
      (complete? [9 [] []]) => false
      (complete? [9 [10] [7]]) => true
      (complete? [1 [4] []]) => false
      (complete? balanced-example) => false
      (complete? unbalanced-example) => false)

(fact "insertions create a balanced tree"
      (insert [] 5) => [5]
      (insert [5] 9) => [5 [] [9]]
      (insert [5 [] [9]] 2) => [5 [2] [9]]
      (insert [9 [5] []] 2) => [5 [2] [9 [] []]]
      (insert [5 [2] [9]] 12) => [5 [2] [9 [] [12]]]
      (insert [5 [2] [9 [] [12]]] 10) => [5 [2] [10 [9] [12]]]
      (insert [2 [1] []] 3) => [2 [1] [3]]
      (insert [2 [1] [3]] 10) => [2 [1] [3 [] [10]]]
      )

(fact "we can traverse a tree in various manners"
      (pre-order-map inc balanced-example) => [3 6 12 7 8 9 5 10 6 3]
      (in-order-map inc balanced-example) => [3 8 6 7 12 3 9 6 5 10]
      (post-order-map inc balanced-example) => [3 8 3 7 6 12 6 9 10 5]
      (level-order-map inc balanced-example) => [3 8 6 3 7 9 10 6 12 5]

      ;; http://rosettacode.org/wiki/Tree_traversal#Clojure
      ;;         1
      ;;        / \
      ;;       /   \
      ;;      /     \
      ;;     2       3
      ;;    / \     /
      ;;   4   5   6
      ;;  /       / \
      ;; 7       8   9
      (let [tree [1
                  [2
                   [4 [7] []]
                   [5]]
                  [3
                   [6 [8] [9]]
                   []]]]
        (pre-order-map identity tree) => [1 2 4 7 5 3 6 8 9]
        (in-order-map identity tree) => [7 4 2 5 1 8 6 9 3]
        (post-order-map identity tree) => [7 4 5 2 8 9 6 3 1]
        (level-order-map identity tree) => [1 2 3 4 5 6 7 8 9])

      (let [tree [1 [2 [4] [5]] [3 [6]]]]
        (df tree) => [1 2 4 5 3 6]
        (bf tree) => [1 2 3 4 5 6]))
