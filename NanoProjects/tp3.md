
Course 3 lab session (2+2 hours + workgroup)
==============================

This lab session focuses on the interpreter (you may safely ignore the
compiler) in order to add new features to the ILP9 language. This
implies to extend the grammar, the parser, the interpreter and its
runtime library.

### Nano-Project 3.1 ###

Add a `repeat-until` loop.

```ilp
repeat {
   instructions...
} until ( condition );
```

### Nano-Project 3.2 ###

Add a `do-while` loop. Nota: this is not a `while-do` loop!

```ilp
do {
  instructions...
} while ( condition );
```

### Nano-Project 3.3 ###

Add instructions `break` and `continue` (as in C or Java).

```ilp
loop {
  instructions
  if ( condition ) {
    break
  }
}
```

### Nano-Project 3.4 ###

Add `global v` which yields the global value of the variable `v` in
every lexical context that is, even if there is a `v` local variable.

```ilp
function foo (pi) {
  return (pi + global pi)
}
print foo(1); // prints 4.14159265...
```

### Nano-Project 3.5 ###

Add a parallel binary assignment. This allows to swap two variables as
in `a,b = b,a`.

```ilp
{ 
  a,b = 3,5;
  a,b = b,2*a
  print a; print b; // prints 5, 6
}
```

### Nano-Project 3.6 ###

Introduce a `constant` keyword that qualifies a global variable that
must not be mutated.

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
