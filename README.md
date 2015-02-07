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

Genetic Programming and Data Structures
---------------------------------------
The thing about Genetic Programming is that you **want** the program
trees to be mutable. In Clojure the list datastructure is not mutable.
Instead we will have to use a different datastructure. The obvious
choices are vector and map.

Map is probably a better choice because then we could associate additional
information with each node. For example the index of the node and the
depth of the node are useful pieces of information when performing 
crossover and mutations.

One remaining question then is if we should have :left-child and
:right-child keys in the map that are associated with the left and 
right child nodes or if we should have a :children key that is associated
a vector of child nodes. This way we could have an arbitrary number of
child nodes. I think I will go with an arbitrary number of children
since this will be very useful in the case of polynomials.

Root Node
---------
It is probably a good idea to have a root node for the program tree. Compare
this with how Knuth does linked lists in the Art of Computer programming.

The root node has one child. This child is the actual root of the tree. The
purpose of the root node is for the algorithms to be able to treat the actual
root node of the tree the same as all the other nodes.

Node
----
Add a struct called node that contains the following keys.

:function
:children
:depth
:index

 
