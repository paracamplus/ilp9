
Course 3 lab session (2 hours)
==============================

This lab session focuses on the interpreter (you may safely ignore the
compiler ) in order to add new features to the ILP9 language. This
often implies to extend the grammar, the parser and the interpreter.

### Nano-Project 3.1 ###

Add a `repeat-until` loop.

### Nano-Project 3.2 ###

Add a `do-while` loop. Nota: this is not a `while-do` loop!

### Nano-Project 3.3 ###

Add instructions `break` and `continue`. Harder: implement loops,
`break` and `continue` with a label allowing to escape from the
associated loop.

### Nano-Project 3.4 ###

Add `global v` which yields the global value of the variable `v` in
every lexical context that is, even if there is a `v` local variable.

### Nano-Project 3.5 ###

Reading the value of a non-initialized variable should raise an
exception. Make it happen!

### Nano-Project 3.6 ###

Predefined global variables should not be mutable. Make it happen!

### Nano-Project 3.7 ###

Add the ternary operator `?:` that is, the alternative as an
expression as in C.

### Nano-Project 3.8 ###

Raising an exception is currently done via the function `throw`, turn
`throw` into a new keyword.


Hints
-----

The entry point for tests is the class `InterpreterTest`, look at it
to find where you may add your own code.

Look at the `Makefile` to regenerate the RNG form of the grammar.

Of course, your code must be tested, automatically! If you write new
ILP9 programs, name them `u00xyz-1.xml` (where xyz is some number) and
write their associated `u00xyz-1.result` and `u00xyz-1.print` files.

Good luck.
