Galapagos
=========
Galapagos is a small Genetic Programming experiment written in Clojure.

Koza's original Genetic Programming implementation is written in Common Lisp.
I had not forseen any difficulties with implementing a Genetic Programming
system in Clojure since it is also a Lisp. However there is one major
difference between Clojure and Common Lisp that makes writing a Genetic
Programing system in Clojure more difficult.

In Clojure a list is immutable. In Common Lisp is is possible to
destructively modify a list by replacing an element in the list. This
can be done for example using the Common Lisp setf macro.

It obviously possible to write functions that create a new copy of a 
program where a certain branch is replaced or where two branches from
different parents have been crossed over. However these functions do
not feel very clean. Instead it is probably better to come up with a
different representation for the program using a data structure that
is mutable in Clojure.

Vectors are a good choice. However maps might also be a good choice.
With maps we can add lots of extra information to the program tree
like the index of each node and the depth of each node. These pieces
of information are very useful when you are performing the core
Genetic Programming operations.

I am going to have to think on this a bit before I continue.