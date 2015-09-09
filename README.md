# numerical-cookbook

This is an Ada 2012 re-implementation of some of the codes featured in Numerical Recipes, 3rd Edition. This README file summarizes what we believe is most important for project newcomers, you can learn more about the project in [its Wiki](https://github.com/Neolander/numerical-cookbook/wiki).

## Release model redux

This library uses a rolling release model. Code that is published in the master branch is considered of production quality and ready for use by third parties. We will do everything possible not to break compatibility, and if we end up having to do so, we will maintain the pre-breach code as a separate feature-frozen branch for one year, so as to give library users time to adjust to the changes being performed.

## Compiler configuration and portability

The main.adb source file gives an example of how to instantiate the library. All you should need to build and use this library is an Ada 2012 compliant compiler, such as recent releases of GNAT GPL from AdaCore (distributed at <http://libre.adacore.com>).

The library was developed and tested with GNAT GPL 2015 on Linux x86_64 and Windows 10 x86_64. It uses a couple of GNAT-specific extensions to Ada, but never in a fashion that is vital to the code operation. An example of such use is the suppression of false alarms from compiler warnings.

For fellow GNAT users, we also provide [a gprbuild-compatible project file](https://github.com/Neolander/numerical-cookbook/blob/master/numericalcookbook.gpr), which features the recommended compiler switches to build the library in typical scenarii : unoptimized debug builds with gcov coverage information, release builds with full code optimizations and run-time checks (to test for optimization-induced defects), and release builds without any run-time check for maximal performance.

Portability is a goal, so if you find the library to break on another architecture, operating system, or compiler, please report this as a bug. Contributions enabling users of other compilers to use this library more easily are also welcome.

## Licensing and reuse

Please feel free to study this code and make use of it for your own projects. The library is released under the GPLv3 license, so you can include it in any kind of free software project.

However, please also note that this code is optimized for simplicity, clarity and correctness, not scalability and performance. In particular, the use of stack-allocated vectors and matrices means that large datasets (say, 400x400 matrices) will cause stack overflows on most OS/compiler combinations. Also, for production use, you will probably want to disable the automated package self-testing that currently occurs during package elaboration, which I have set up to help with development. This may be done by flipping a boolean flag in Cookbook.Test.

Incidentally, the above also means that patches that solely revolve around making things faster or more scalable at the expense of code readability, such as manual algorithm parallelization, will be rejected. However, feel free to develop a more scalable fork of this library, and if you tell me about it, I will be happy to link to it here.

## Acknowledgements

This project was started in 2015 and is currently being fully maintained by Hadrien G. Want to see your name here? Please do not hesitate to submit your own contribution!
