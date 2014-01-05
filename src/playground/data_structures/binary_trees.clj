(ns playground.data-structures.binary-trees
  (:require [clojure.core.async :as async])
  (:use clojure.walk))

;; http://en.wikipedia.org/wiki/Binary_tree

(defn root [tree] (first tree))
(def parent root)

(defn has-children?
  "We assume a node without children only contains it's root value"
  [tree]
  (= (count tree) 3))

(defn left-child [tree] (if (has-children? tree) (nth tree 1) nil))
(defn right-child [tree] (if (has-children? tree) (nth tree 2) nil))

(defn tree? [tree]
  (if (and (vector? tree) (<= (count tree) 3))
    (cond
     (has-children? tree) (and (tree? (left-child tree))
                               (tree? (right-child tree)))
     :else true)
    false))

(defn height [tree]
  (cond
   (= 0 (count tree)) 0
   (has-children? tree) (inc (max (height (left-child tree))
                                  (height (right-child tree))))
   :else 1))

(defn balanced? [tree]
  (if (has-children? tree)
    (let [left (left-child tree)
          right (right-child tree)
          difference (Math/abs (- (height left)
                                  (height right)))]
      (and
       (<= difference 1)
       (balanced? left)
       (balanced? right)))
    true))

(defn rotate-left [tree]
  ;;(println "rotate-left" tree)
  ;; right parent becomes this parent
  (let [left (left-child tree)
        right (right-child tree)
        rotated-tree [(root right)
                      ;; left child becomes root and left child
                      [(root tree) left []]
                      ;; right child becomes right of right
                      (right-child right)]]
    ;;(println "rotated" rotated-tree)
    rotated-tree))

(defn rotate-right [tree]
  ;;(println "rotate-right" tree)
  ;; left parent becomes root
  (let [left (left-child tree)
        right (right-child tree)
        rotated-tree [(root left)
                      ;; left child become left child of left child
                      (left-child left)
                      ;; right child becomes root and right child
                      [(root tree) [] right]]]
    ;;(println "rotated" rotated-tree)
    rotated-tree
    ))

(defn replace-left [tree branch]
  ;;(println "replace-left" tree branch)
  (if (= (count tree) 1)
    (conj tree branch [])
    (assoc tree 1 branch)))

(defn replace-right [tree branch]
  ;;(println "replace-right" tree branch)
  (if (= (count tree) 1)
    (conj tree [] branch)
    (assoc tree 2 branch)))

(defn balance [tree]
  ;;(println "balance" tree)
  (if (balanced? tree)
    tree
    (let [left (left-child tree)
          right (right-child tree)
          left-height (height left)
          right-height (height right)
          ;;_ (println "-" left-height right-height)
          difference (- left-height
                        right-height)]
      ;;(println "tree is unbalanced" difference)
      (cond
       (> difference 0) (rotate-right tree)
       (< difference 0) (rotate-left tree)
       :else [(root tree) (balance left) (balance right)]))))

(defn complete? [tree]
  (cond
   (has-children? tree) (and (complete? (left-child tree))
                             (complete? (right-child tree)))
   :else (not (nil? (root tree)))))

(defn insert [tree item]
  (let [new-tree
        (cond
         (empty? tree) [item]
         (< (root tree) item) (replace-right tree
                                             (insert (right-child tree) item))
         (> (root tree) item) (replace-left tree
                                            (insert (left-child tree) item))
         )
        left-height (height (left-child new-tree))
        right-height (height (right-child new-tree))
        ]
    (cond
     (balanced? new-tree) new-tree
     (> left-height right-height) (rotate-right new-tree)
     (< left-height right-height) (rotate-left new-tree))))

(defn insert-node [tree item]
  (if (< (compare item (root tree)) 1)
    (insert (left-child tree) item)
    (insert (right-child tree) item)))

(defn remove-node [tree item]
  ;; TODO
  )

(defn pre-order-map [f tree]
  (cond
   (empty? tree) []
   (has-children? tree) (concat
                         (pre-order-map f (left-child tree))
                         (pre-order-map f (right-child tree))
                         [(f (root tree))])
   :else [(f (root tree))]))

(defn pre-order-map [f tree]
  (prewalk f tree))

(defn in-order-map [f tree]
  (cond
   (empty? tree) []
   (has-children? tree) (concat
                         (in-order-map f (left-child tree))
                         [(f (root tree))]
                         (in-order-map f (right-child tree)))
   :else [(f (root tree))]))

(defn post-order-map [f tree]
  (cond
   (empty? tree) []
   (has-children? tree) (concat
                         [(f (root tree))]
                         (post-order-map f (left-child tree))
                         (post-order-map f (right-child tree)))
   :else [(f (root tree))]))

;; http://martintrojer.github.io/clojure/2013/07/17/non-tailrecursive-functions-in-coreasync/
(defn drain [c]
  (loop [v (async/<!! c) res []]
    (if v
      (recur (async/<!! c) (conj res v))
      res)))

(defn test-walk [tree ch]
  (letfn [(walker [t]
            (async/go
             (when (root t)
               (async/>! ch (root t))
               (async/<! (walker (left-child t)))
               (async/<! (walker (right-child t))))))]
    (async/go
     (async/<! (walker tree))
     (async/close! ch))))

;; 1  procedure BFS(G,v) is
;; 2      create a queue Q
;; 3      create a set V
;; 4      enqueue v onto Q
;; 5      add v to V
;; 6      while Q is not empty loop
;; 7          t ← Q.dequeue()
;; 8          if t is what we are looking for then
;; 9              return t
;; 10         end if
;; 11         for all edges e in G.adjacentEdges(t) loop
;; 12             u ← G.adjacentVertex(t,e)
;; 13             if u is not in V then
;; 14                  add u to V
;; 15                  enqueue u onto Q
;; 16             end if
;; 17         end loop
;; 18     end loop
;; 19     return none
;; 20 end BFS

;; Doesn't work
(defn level-walk [tree c]
  (println "level-walk" tree c)
  (async/go
   (async/>!! c "TEST")
   (println "---" (async/<!! c))
   (async/>!! c tree)
   (loop [n (async/<!! c) res []]
     (println "loop" n res)
     (if n
       (do
         (println "c & next")
         (async/>!! c (left-child n))
         (async/>!! c (right-child n))
         (recur (async/<!! c) (conj res (root tree))))
       (do
         (println "n is nil")
         (async/close! c)
         res)))))

(defn level-order-map [f tree]
  (println "level-order-map" tree)
  (map f
       (cond
        (empty? tree) []
        (has-children? tree)

        (let [
              c (async/chan)
              ;;c (async/timeout 5000)
              ]
          ;;(async/thread (level-walk tree c))
          ;;(drain c)
          )
        :else [(root tree)])))

(def leaf? (comp not has-children?))
(def internal? has-children?)
(def external? leaf?)

;; TODO make use of
;; http://clojuredocs.org/clojure_core/clojure.walk

;; http://stackoverflow.com/questions/11409140/stumped-with-functional-breadth-first-tree-traversal-in-clojure

(defn bfs-eager [tree]
  (loop [ret [], queue (conj clojure.lang.PersistentQueue/EMPTY tree)]
    (if (seq queue)
      (let [[node & children] (peek queue)]
        (recur (conj ret node) (into (pop queue) children)))
      ret)))

(defn bfs-lazy [tree]
  ((fn step [queue]
     (lazy-seq
      (when (seq queue)
        (let [[node & children] (peek queue)]
          (cons node
                (step (into (pop queue) children)))))))
   (conj clojure.lang.PersistentQueue/EMPTY tree)))

(def tree [1 [2 [4] [5]] [3 [6]]])

(defn df "return elements in tree, depth-first"
   [[el left right]] ;; a tree is a seq of one element,
                     ;; followed by left and right child trees
   (if el
     (concat [el] (df left) (df right))))

(df tree)
;; => (1 2 4 5 3 6)

(defn bf [& roots]
   (if (seq roots)
       (concat (map first roots) ;; values in roots
               (apply bf (mapcat rest roots))))) ;; recursively for children

(bf tree)
;; => (1 2 3 4 5 6)
