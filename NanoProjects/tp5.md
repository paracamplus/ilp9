Course 5 lab session (group work)
=================================

These nano-projects focus on the compiler 
in order to add new features to the ILP9 language. This
implies to extend the grammar, the parser, the compiler and its
runtime library.

NanoProjects extending a previous NanoProject should also enrich the 
interpreter. The other NanoProjects may safely ignore the interpreter.

NanoProjects
------------

### NanoProject 5.1 (extends 3.1) ###

Add a `repeat-until` loop. 

```ilp
repeat {
   instructions...
} until ( condition );
```

### NanoProject 5.2 (extends 3.3) ###

Add instructions `break label` and `continue label`. They may be used
in a `loop label` loop.

```ilp
loop A: {
  instructions...
  loop B: {
    instructions...
    break A; // leaves loop A
    ...
  }
  instructions
}
```

### NanoProject 5.3 (extends 3.4) ###

Add a new keyword such as `global v` which yields the global value of
the variable `v` in every lexical context that is, even if there is a
local variable with the same name. 

```ilp
function foo (pi) {
  return (pi + global pi)
}
print foo(1); // prints 4.14159265...
```

### NanoProject 5.4 (extends 3.5) ###

Add a parallel binary assignment. This allows to swap two variables as
in `a,b = b,a`.

```ilp
{ 
  a,b = 3,5;
  a,b = b,2*a
  print a; print b; // prints 5, 6
}
```

### NanoProject 5.5 (extends 3.6) ###

Introduce a new toplevel keyword `define` keyword that allows to
declare a global immutable variable and initialize it with some
expression.

```ilp
function foo(x) {
   return x + cst
}
define cst = pi * pi;
print foo(2 * cst) // prints something around 29.61
```

### NanoProject 5.6 ###

Introduce a `letremanent in` keyword that qualifies a variable that keeps
its value across multiple evaluations.

```ilp
function foo(x) {
  letremanent r = 2 in {
    if (< r x) {
       r = x
    }
    return r
  }
}
print(foo(0))  // prints 2
print(foo(10)) // prints 10
print(foo(5))  // prints 10
print(foo(15)) // prints 15
```

### NanoProject 5.7 ###

Introduce a kind of `elsif` facility. It will be abstracted by the new
keyword `case` (and `default`) as in

```ilp
case
 condition1 => instructions1
 condition2 => instructions2
 default =>    instructions
esac
```

### NanoProject 5.8 ###

Introduce the `return` keyword as in C or Java.

```ilp
function foo(x) {
  while ( x > 0 ) {
    if ( bar(x) ) {
      return x
      print x   // never called!
    }
    x = x - 1
  }
}
```

### NanoProjet 5.9 (extends 3.9) ###

Allow default value for missing arguments of unary functions. 
When invoked without argument, the variable will be bound to the 
default value. Hence

```ilp
function f (x, y = 2) {
  return x * y
 }
 f(3)   // yields 6 
 f(3,3) // yields 9
 ```
 


Hints
-----

The entry point for tests is the class `CompilerTest`, look at it
to find where you may add your own code.

Look at files `ilp.h` and `ilp.c`. 

Of course, your code must be tested, automatically! If you write new
ILP9 programs, name them `u00xyz-1.xml` (where xyz is some number) and
write their associated `u00xyz-1.result` and `u00xyz-1.print` files.

Good luck!