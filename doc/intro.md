# 

# TODO: write [great documentation](http://jacobian.org/writing/what-to-write/)

galapagos
=========
galapagos is a small genetic programming experiment. The initial version
will focus on getting a genetic programming algorithm working. The initial
problem to solve will be approximating a polynomial.


The current version does not converge on a solution yet. There are a bunch
of reasons for this. One of which I believe to be that the depth of the
program tree is not bounded during the mutation or crossover operation.
I am working on fixing this.

Also the program needs to be split into separate files and I need
to add some unit tests.