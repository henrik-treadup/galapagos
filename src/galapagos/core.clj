(ns galapagos.core
  (:gen-class))

;
; The polynomial part.
;

;
; Polynomials are represented by vectors of coefficients. 
; The lowest coefficient  i.e. the constant coefficient 
; is stored in the first element of the vector.
;

(defn deg 
  "Calculates the degree of the polynomial."
  [poly]
  (dec (count poly)))

(defn eval-poly 
  "Evaluates the polynomial for the given point."
  [poly x]
   (cond
      (= (deg poly) 0) 
         (first poly)
      :else
         (+ (first poly) (* x (eval-poly (rest poly) x)))))

(defn poly-to-function 
  "Converts a polynomial to a normal clojure function."
  [poly]
  (fn [x] (eval-poly poly x)))

(defn add-poly 
  "Adds two polynomials."
  [p1 p2]
   (cond
      (empty? p1) p2
      (empty? p2) p1
      :else (cons (+ (first p1) (first p2)) (add-poly (rest p1) (rest p2)))))

; Multiplies the polynomials p1 and p2. Returns a list where each element is a pair. The first part of the pair is
; the coefficent. The second part of the pair is the exponent. There can (and will be in most of the cases) pairs that
; have the same exponent.
(defn mul-poly-expanded 
  "Multiplies the polynomials p1 and p2. Returns a list of pairs of coefficients and exponents.
   There can be multiple pairs with the same exponent."
[p1 p2]
  (for [i (range (inc (deg p1))) j (range (inc (deg p2)))] (list (* (p1 i) (p2 j)) (+ i j))))

; Calculates the nth term of the polynomial. 
(defn nth-term 
  "Takes a list of coefficient and exponent pairs and calculates the nth term
   of the resulting polynomial."
  [result n]
  (apply + (for [r result :when (= n (second r))] (first r))))

(defn mul-poly 
  "Multiplies the polynomials p1 and p2."
  [p1 p2]
   (let [new-degree (+ (deg p1) (deg p2))
         result (mul-poly-expanded p1 p2)]
     (into [] (for [k (range (inc new-degree))] (nth-term result k)))))

(defn abs 
  "Calculates the absolute value of x"
  [x] (max x (- x)))

(defn poly-difference 
  "Calculates the difference between the polynomials p1 and p2 at the point x."
  [p1 p2 x]
  (- (eval-poly p1 x) (eval-poly p2 x)))
  
(defn poly-difference-multi 
  "Calculates the difference between the polynomials p1 and p2 at multiple points."
  [p1 p2 points]
  (if (= 0 (count points))
    '()
    (conj 
     (poly-difference-multi p1 p2 (rest points))
     (poly-difference p1 p2 (first points)))))

(defn linear-polynomial 
  "Constructs a linear polynomial given the root of the linear polynomial."
  [root]
  [(- root) 1.0])

(defn polynomial-from-roots
  "Generates a polynomial with the given roots. The highest coefficient is 1.0"
  [roots]
  (if (= 1 (count roots))
    (linear-polynomial (first roots))
    (mul-poly (linear-polynomial (first roots)) 
              (polynomial-from-roots (rest roots)))))


(defn interval-int-list [a b]
  "Creates a list of integers from a to b inclusive."
  (map #(+ a %) (range (inc (- b a)))))

;
; Genetic Programming part.
;


(def target-polynomial (polynomial-from-roots [1 2 3 5 7]))
(def target-function (poly-to-function target-polynomial))

; This is the fitness function. This should be as low as possible. As close to 0 as possible.
; Actually this will not work. We have to compare a polynomial to the results of the
; generated programs.

; It is probably a better idea to 
(defn fitness-function [p1 p2]
   (apply + (map abs (poly-difference-multi p1 p2 (range 10.0)))))

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


; There are a couple of things that we have to do.
; 1. Generate a random population of initial programs.
; 2. Mutate a program.
; 3. Sexually reproduce two programs.



(defn between 
  "Checks if n lies in the interval [a,b]. Returns true if n is in the
  interval. Returns false otherwise."
  [a b n]
   (and (<= a n) (<= n b)))


(defn count-nodes 
  "Counts the number of nodes in the tree. In other words
   returns the sum of the number of functions and the 
   number of terminals in the tree"
  [tree]
   (if (list? tree)
     (inc (apply + (map count-nodes (rest tree))))
     1))

(defn depth 
  "Calculates the depth of the tree"
  [tree]
  (if (list? tree)
    (if (empty? tree)
      1
      (inc (max (depth (nth tree 1))
                (depth (nth tree 2)))))
    1))


(defn tree-list 
  "Creates a sequence of all the subtrees of the tree."
  [tree]
   (concat (list tree)
           (if (list? tree)
               (tree-list (nth tree 1))
               ())
           (if (list? tree)
               (tree-list (nth tree 2))
               ())))


(defn tree-depth-list
  "Creates a sequence of the depth of all the subtrees of the tree."
  ([tree]
   (tree-depth-list tree 0))
  ([tree parent-depth]
   (let [current-depth (inc parent-depth)]
     (if (list? tree)
       (concat (cons current-depth 
                     (tree-depth-list (left-child tree) current-depth))
               (tree-depth-list (right-child tree) current-depth))
       [current-depth]))))


(defn depth-of-subtrees 
  "Calculates the depth of each subtree in the tree.
   Returns a vector containing the depth of all the subtrees.
   The nth element contains the depth of the nth tree in
   the list rerturned by tree list."
  [tree]
  (into [] (map depth (tree-list tree))))


; Returns all proper subtrees of tree.
(defn all-subtrees [tree]
   (filter list? (tree-list tree)))

; Selects a random subtree from a tree. Does not select leaf nodes.
; [The current implementation can return the entire tree.]
(defn random-subtree [tree]
  (rand-nth (all-subtrees tree)))


; Actually what I want to do is to replace a random node.
; All nodes should be replaced with equal probability.


; Replaces a terminal node. Should only be called with n = 0.
; And tree should not be a list.
(defn replace-terminal-node [tree n new-node]
  (cond
   (list? tree) nil
   (= 0 n) new-node
   :else nil))

; Replaces the nth node with the new tree.
; Replaces the nth node in the tree with the node new-node.
(defn replace-node [tree n new-node]
  (if
   (not (list? tree)) (replace-terminal-node tree n new-node)
   (let [first-child-tree (nth tree 1)
         second-child-tree (nth tree 2)
         count (count-nodes tree)
         count-first (count-nodes (nth tree 1))
         count-second (count-nodes (nth tree 2))]
     (cond
      (= n 0) new-node
      (between 1 count-first n) (list
                                 (first tree) 
                                 (replace-node first-child-tree (dec n) new-node) 
                                 second-child-tree)
      (between (inc count-first) count n) (list
                                           (first tree)
                                           first-child-tree
                                           (replace-node second-child-tree (- n count-first 1) new-node))
      :else nil))))

(defn random-node-index [tree]
  (rand-int (count-nodes tree)))



(defn random-tree-depth []
  (rand-nth (interval-int-list 1 max-tree-depth)))

; 1. Cross over
; 2. Mutate

(defn no-taller-than
  "Checks that the height of the tree is less than or equal to
   the given height."
  [tree height]
  (if (<= (depth tree) height)
    tree
    nil))


; The mutate-tree function should be changed so that
; the depth of the tree never grows.
; We will ensure this by wrapping the contents of mutate
; tree 
(defn mutate-tree [tree]
  (replace-node tree 
                (random-node-index tree) 
                (random-tree (random-tree-depth) :grow)))

(defn cross-over [tree1 tree2]
   (let [random-index-1 (random-node-index tree1)
         random-index-2 (random-node-index tree2)]
     (let [subtree-1 (nth (tree-list tree1) random-index-1)
           subtree-2 (nth (tree-list tree2) random-index-2)]
       [(replace-node tree1 random-index-1 subtree-2)
        (replace-node tree2 random-index-2 subtree-1)])))

; Removes a random subtree and replaces it with a terminal
(defn prune [tree]
  (replace-node tree (random-node-index tree) (random-terminal)))

; Need to test that prune tree actually works.

; Ready for the core of the algorithm.

; Calculate the fitness for the population.
; Cross over a certain part of the population
; Mutate a certain part of the population
; Keep a certain part of the population.


(defstruct individual :program :creation-method :raw-fitness)

(defn random-individual 
  ([]
   (struct individual (random-tree) "random-individual"))
  ([tree-depth method]
   (struct individual (random-tree tree-depth method) "random-individual")))

; Returns a list of pairs. The first element is the tree depth.
; The second element is the method that should be used to generate
; the tree i.e. :grow or :full
(defn create-construction-parameters [population-count tree-depth]
  (apply concat (map #(list (list % :grow) (list % :full))
                     (take (int (/ population-count 2))
                           (cycle (map inc (range tree-depth)))))))


; Returns an individual where the raw fitness has been calculated.
(defn update-individual-fitness [individ]
  (assoc individ :raw-fitness (fitness (individ :program))))

; Returns the population list with the raw fitness of each individual recalculated.
(defn update-population-fitness [population]
  (map update-individual-fitness population))

(defn create-random-population
  ([]
   (create-random-population population-size max-initial-tree-depth))
  ([population-count tree-depth]
   (update-population-fitness 
    (map #(apply random-individual %) 
           (create-construction-parameters population-count tree-depth)))))

(defn create-random-population-with-fitness []
   (create-random-population))

; Finds the fittest individual in a population
(defn fittest-individual [population]
  (first (sort-by :raw-fitness population)))

(defn depth-of-individual [individ]
  (depth (:program individ)))

(defn count-nodes-individual [individ]
  (count-nodes (:program individ)))

;
; Reproduction, Cross Over, Mutation and Pruning
;

; TODO: The mutate-individual function should never be able to increase the 
; depth of a program tree beyond the max-tree-depth.
(defn mutate-individual [individ]
  (struct individual (mutate-tree (individ :program)) "mutation"))

; TODO: The crossover-individuals function should never be able to increase
; the depth of the resulting programs beyound the max-tree-depth.
(defn crossover-individuals [individ1 individ2]
  (let [children (cross-over (:program individ1) (:program individ2))]
    (list
     (struct individual (first children) "cross-over")
     (struct individual (second children) "cross-over"))))

(defn reproduce-individual [individ]
  (assoc individ :creation-method "reproduction"))

(defn prune-individual [individ]
  (struct individual (prune (:program individ)) "pruning"))

; Selects an individual from the population using tournament selection.
; The size of the tournament is given by tournament-size.
(defn tournament-select [tournament-size population]
  (fittest-individual (take tournament-size (shuffle population))))


; Performs reproduction of one individual in the old population.
; Returns the updated new population.
(defn perform-reproduction [old-population new-population]
  (comment println "Perform reproduction") 
  (cons 
   (reproduce-individual (tournament-select reproduction-tournament-size old-population)) 
   new-population))

; Performs mutation on one individual in the old population.
; Returns the updated new population.
(defn perform-mutation [old-population new-population]
  (comment println "Perform mutation")
  (cons (mutate-individual (tournament-select mutation-tournament-size old-population)) new-population))

; Performs crossover between two individuals in the old population.
; Generates two new offspring. Returns the updated new population.
(defn perform-crossover [old-population new-population]
  (comment println "Perform cross over")
  (let [parent1 (tournament-select crossover-tournament-size old-population)
        parent2 (tournament-select crossover-tournament-size old-population)]
    (concat new-population (crossover-individuals parent1 parent2))))

; Performs pruning of one individual in the old population.
; Returns the updated new population
(defn perform-pruning [old-population new-population]
   (comment println "Perform pruning")
   (let [selected-individual (tournament-select pruning-tournament-size old-population)]
     (cons (prune-individual selected-individual) new-population)))

; Reproduce a certain portion of the population.
; Crossover a certain portion of the population.
; Mutate a certain portion of the population.
; Returns the updated new population.
(defn perform-action [old-population new-population]
 (let [p (rand 1.0)
       cop crossover-probability
       mp (+ crossover-probability mutation-probability)
       rp (+ crossover-probability mutation-probability reproduction-probability)]
   (cond
    (< p cop) (perform-crossover old-population new-population)
    (< p mp) (perform-mutation old-population new-population)
    (< p rp) (perform-reproduction old-population new-population)
    :else (perform-pruning old-population new-population))))

(defn calculate-generation-step
  ([population]
   (calculate-generation-step population ()))
  ([old-population new-population]
   (loop [old-pop old-population new-pop new-population]
     (if (>= (count new-pop) population-size)
       (update-population-fitness (take population-size new-pop))
       (recur old-pop (perform-action old-pop new-pop))))))

(defn calculate-generations 
  ([]
   (calculate-generations (create-random-population-with-fitness)))
  ([population]
   (loop [populus population generation 0]
     (if (= max-generation-count generation)
       populus
       (do
         (println (str "Processing generation " (inc generation)))
         (recur (calculate-generation-step populus) (inc generation)))))))

; For some reason all the programs converge on a single program.
; I am unsure why this is happening.
; Perhaps the tournament sizes are too large.


; Record how each individual was created.
; Random Generation

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
