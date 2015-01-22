(ns galapagos.ast
  (:use [galapagos.config])
  (:gen-class))

;
; Abstract Syntax Tree functions
;

(defn operator
  "Gets the operator from a node."
  [tree]
  (first tree))

(defn right-child
  "Gets the right child from a node."
  [tree]
  (nth tree 2))

(defn left-child
  "Gets the left child from a node."
  [tree]
  (nth tree 1))
  
; Determines if x is a terminal
(defn function? 
  ([x]
     (function? x functions))
  ([x funcs]
     (if (empty? funcs)
        false
        (if (= x (first funcs))
           true
           (function? x (rest funcs))))))

(defn random-function []
  (rand-nth (list '+ '- '* )))

; Returns a random number between +10 and -10.
(defn random-number []
  (- (rand-int 21) 10))

(defn random-terminal []
  (if (> (rand 1.0) 0.5)
    'x
    (random-number)))

(defn grow-random-tree [tree-depth]
  (cond
   (= tree-depth 0) (random-terminal)
   (< (rand 1.0) generation-function-threshold) (random-terminal)
   :else (list (random-function) 
               (grow-random-tree (dec tree-depth)) 
               (grow-random-tree (dec tree-depth)))))

; Generates a full tree with the given depth.
(defn random-full-tree [tree-depth]
  (cond
   (= tree-depth 0) (random-terminal)
   :else (list (random-function)
               (random-full-tree (dec tree-depth))
               (random-full-tree (dec tree-depth)))))

; Creates a random tree with the given tree depth.
; If method is :grow then a tree will be grown.
; If method is :full then a full tree will be generated.
(defn random-tree [tree-depth method]
  (cond
   (= method :grow) (grow-random-tree tree-depth)
   (= method :full) (random-full-tree tree-depth)))

; Creates a function based on a tree. The function takes a number
; as an argument and returns the calculated value of the program
; at that point.
(defn tree-to-function [tree]
  (eval (list 'fn '[x] tree)))

