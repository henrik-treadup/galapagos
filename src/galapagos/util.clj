(ns galapagos.util
  (:gen-class))

;
; Utility functions
;

(defn between 
  "Checks if n lies in the interval [a,b]. Returns true if n is in the
  interval. Returns false otherwise."
  [a b n]
   (and (<= a n) (<= n b)))


; Rename this to inc-range for inclusive-range.
(defn inc-range 
  "Creates a list of integers from a to b inclusive.
   Inclusive range
  "
  ([b]
   (range (inc b)))
  ([a b]
   (range a (inc b))))

(def interval-int-list inc-range)

