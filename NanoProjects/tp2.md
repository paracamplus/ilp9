
Course 2 lab session (2 hours + group work)
===========================================

These nano-projects focus on the interpreter. Nano-projects 1-4 aim
at simplifying AST while nano-projects 5-8 ask for additional
primitives.

NanoProjects
------------

### NanoProject 1 ###

Currently, some sequences may contain only one expression. Modify the
system so sequences can only contain more than one expression.

### NanoProject 2 ###

Currently, some sequences may contain an arbitrary number of
expressions. Modify the system so sequences can only contain two
expressions.

### NanoProject 3 ###

Currently, a block may contain no binding. If a block does not
introduce new local variables then it is similar to a sequence.
Modify the system in order to remove blocks without bindings.

### NanoProject 4 ###

Currently, a codefinition may exist without any local function
definition. This codefinition is therefore equivalent to a sequence.
Modify the system in order to remove these useless codefinitions.

### NanoProject 5 ###

Adjoin a new primitive function to the system. This primitive named
`now` will return the current time (expressed in milliseconds).

### NanoProject 6 ###

Adjoin a new primitive function to the system. This primitive named
`implies` takes two values and return false only when the first
argument is true and the second argument is false.

### NanoProject 7 ###

Adjoin the `sin` primitive to the system. That primitive computes the
sine of a number.

### NanoProject 8 ###

Adjoin the `hypotenuse` primitive to the system. That primitive takes
two numbers, say `a` and `b` and computes the square root of `a^2 + b^2`.

