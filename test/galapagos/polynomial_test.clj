(ns galapagos.polynomial-test
  (:use [clojure.test :refer :all]
        [galapagos.polynomial :refer :all]))

(deftest deg-test
  (is (deg [1]) 0)
  (is (deg [2 1]) 1)
  (is (deg [3 0 1]) 2))

(deftest eval-poly-test
  (is (eval-poly [3] 5) 3)
  (is (eval-poly [4 1] 2) 6)
  (is (eval-poly [1 2 1] 3) 16))

(deftest poly-to-function-test
  (let [poly-func (poly-to-function [1 2 1])]
    (is (poly-func 3) 16)))

(deftest add-poly-test
  (is (add-poly [3] [5]) [8])
  (is (add-poly [3 1] [4 3 7]) [7 4 7]))

(deftest mul-poly-expanded-test
  (is (mul-poly-expanded [3 2] [4 5 6])
      '((12 0) (15 1) (18 2) (8 1) (10 2) (12 3))))

(deftest nth-term-test
  (is (nth-term '((100 0) (3 1) (200 2) (2 1)) 1) 5))

(deftest mul-poly-test
  (is (mul-poly [3] [4 1]) [12 3])
  (is (mul-poly [-1 1] [1 1]) [-1 0 1]))

(deftest abs-test
  (is (abs 3) 3)
  (is (abs 0) 0)
  (is (abs -2) 2))

(deftest linear-polynomial-test
  (is (linear-polynomial 3.0) [-3.0 1.0]))

(deftest polynomial-from-roots-test
  (is (polynomial-from-roots [1 2 3]) [-6 11 -6 1]))
