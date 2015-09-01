# numerical-cookbook
An Ada 2012 re-implementation of some of the codes featured in Numerical Recipes, 3rd Edition.

If you plan on reusing this code for your own projects, please note that it is optimized for simplicity, clarity and correctness, not scalability and performance. In particular, the use of stack-allocated vectors and matrices means that large datasets (say, 400x400 matrices) will cause stack overflows on most OS/compiler combinations, and you will probably want to disable the automated package self-testing that occurs during body elaboration.

Developed and tested with GNAT GPL 2015. Uses GNAT-specific extensions, but never in a fashion that is vital to the code operation. An example of such use is the suppression of false alarms from compiler warnings.
