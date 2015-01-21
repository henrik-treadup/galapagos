(ns galapagos.config
  (:gen-class))

;
; Configuration
;

(def terminals (cons 'x (list 1 2 3 5 7)))
(def functions [+ - *])

; Some control parameters
(def population-size 500)
(def max-generation-count 100)

(def max-tree-depth 10)
(def generation-function-threshold 0.5)
(def max-mutation-tree-depth 4)
(def max-initial-tree-depth 6)

(def mutation-probability      0.08)
(def crossover-probability     0.80)
(def reproduction-probability  0.08)

(def mutation-tournament-size 2)
(def reproduction-tournament-size 2)
(def crossover-tournament-size 2)
(def pruning-tournament-size 2)
