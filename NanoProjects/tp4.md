
Course 4 lab session (2 hours)
==============================

These nano-projects focus on the compiler (you may safely ignore the
interpreter) in order to add new features to the ILP9 language. This
series of nano-projects aims to provide new primitive functions to ILP9.

NanoProjects
------------

### NanoProject 4.1 ###

Adjoin a new primitive function to the system. This primitive, named
`atoi`, behaves similarly to the `atoi` function of C that is, 
`atoi("813")` returns the integer `813`.

### NanoProject 4.2 ###

Adjoin a new primitive function to the system. This primitive, named
`strstr`, behaves similarly to the `strstr` function of C that is,
`strstr(haystack, needle)` returns the offset (a zero-based integer)
of the first occurrence of the substring needle in the string
haystack.

### NanoProject 4.3 ###

Enrich the `*` operator so that when given `2 * "OK"`, it returns `OKOK`.

### NanoProject 4.4 ###

Adjoin the `xor` primitive function to the system. It takes two
arguments and compute their exclusive disjonction.

### NanoProject 4.5 ###

Adjoin a new primitive function to the system. This primitive named
`now` will return the current elapsed time (expressed in seconds)
since the beginning of the program.

### NanoProject 4.6 ###

Adjoin a new primitive function to the system. This primitive named
`implies` takes two values and return false only when the first
argument is true and the second argument is false.

### NanoProject 4.7 ###

Adjoin the `sin` primitive to the system. That primitive computes the
sine of a number.

### NanoProject 4.8 ###

Adjoin the `hypotenuse` primitive to the system. That primitive takes
two numbers, say `a` and `b` and computes the square root of `a^2 + b^2`.


Hints
-----

The entry point for tests is the class `CompilerTest`, look at it
to find where you may add your own code.

Look at files `ilp.h` and `ilp.c`.

The commands `man atoi` and `man strstr` may be useful.

Of course, your code must be tested, automatically! If you write new
ILP9 programs, name them `u00xyz-1.xml` (where xyz is some number) and
write their associated `u00xyz-1.result` and `u00xyz-1.print` files.

Good luck!
