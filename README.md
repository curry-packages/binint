# binint - Binary representation of natural and integer numbers

This package contains libraries with data types for a binary representation
of natural and integer numbers.
It is based on the paper _Declaring Numbers_ by Brassel/Fischer/Huch
[ENTS 216, 2008](http://dx.doi.org/10.1016/j.entcs.2008.06.037).

The advantage of this algebraic definition in
contrast to built-in integers is the possibility to use them also for
narrowing, i.e., functional logic programming. Since the operations are also
quite efficient compared to a simple Peano representations, this library is
also the basis of the implementation of integers in the Curry implementation
[KiCS2](https://www.curry-lang.org/kics2/).
