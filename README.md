# numerical-cookbook

An Ada 2012 re-implementation of some of the codes featured in Numerical Recipes, 3rd Edition.

## Compiler configuration and portability

All you should need to build and use this library is an Ada 2012 compliant compiler, such as recent releases of GNAT GPL from AdaCore (distributed at http://libre.adacore.com ).

The library was developed and tested with GNAT GPL 2015 on Linux x86_64. It uses a couple of GNAT-specific extensions to Ada, but never in a fashion that is vital to the code operation. An example of such use is the suppression of false alarms from compiler warnings.

Portability is a goal, so if you find the library to break on another architecture, operating system, or compiler, please report this as a bug.

## Licensing and reuse

Please feel free to study this code and make use of it for your own projects. I have not yet decided which license I will put it under, so at this point copyright laws prevent you from simply copying and pasting it, but I will get around this when I get a release that starts looking like a finished product.

Also, please note that this code is optimized for simplicity, clarity and correctness, not scalability and performance. In particular, the use of stack-allocated vectors and matrices means that large datasets (say, 400x400 matrices) will cause stack overflows on most OS/compiler combinations. Also, you will probably want to disable the automated package self-testing that currently occurs during package elaboration, which I set up to help with development.

Incidentally, the above also means that patches that solely revolve around making things faster or more scalable at the expense of code readability, such as manual algorithm parallelization, will be rejected. However, once I get around the licensing issue, please feel free to develop a more scalable fork of this library.
