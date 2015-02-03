(ns galapagos.ast-test
  (:use [clojure.test :refer :all]
        [galapagos.ast :refer :all]))

(deftest operator-test
  (is (operator '(+ 1 2)) +))

(deftest right-child-test
  (is (right-child '(+ 3 5)) 5))

(deftest left-child-test
  (is (left-child '(+ 3 5)) 3))

; For grow random tree we should test that
; the generated trees never have a depth
; greater than tree-depth

; depth is not defined in this context
(comment deftest grow-random-tree-test
  (dotimes [k 1000]
    (let [t (grow-random-tree 3)]
      (is (<= (depth t) 3) true))))
