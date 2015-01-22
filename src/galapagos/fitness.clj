(ns galapagos.fitness
  (:use [galapagos.polynomial]
        [galapagos.ast])
  (:gen-class))


(def target-polynomial (polynomial-from-roots [1 2 3 5 7]))
(def target-function (poly-to-function target-polynomial))




; I need to create a fitness function.

; Eval a function at a list of points

; Samples a function on certain points between 0.0 and 10.0
(defn sample-function [f]
   (map f (range 0.0 10.0 0.1)))

(defn error-function [f]
   (fn [x]
     (- (target-function x) (f x))))

(defn error-squared-function [f]
   (fn [x]
      (let [error-func (error-function f)]
         (let [error (error-func x)]
            (* error error)))))

; This is the fitness function. It calculates the fitness of a program. A lower value means better fitness.
(defn fitness [tree]
  (let [error-squared-func (error-squared-function (tree-to-function tree))]
    (apply +  (sample-function error-squared-func))))

