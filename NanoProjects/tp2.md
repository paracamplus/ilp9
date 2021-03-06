Course 2 lab session (2 hours + group work)
===========================================

These nano-projects focus on the interpreter. Ignore the compiler for now.
Words in italics refer to the terms used in `grammar9.rnc`.
NanoProjects 2.1 to 2.5 are program transformations while NanoProjects 2.6 to 2.9 
enrich the runtime library.

NanoProjects
------------

### NanoProject 2.1 ###

Currently, some sequences may contain only one expression. A _sequence_
with a single expression can be replaced by this expression. Modify
the system so sequences can only contain more than one expression.

In other words, this is a program transformation where blocks like

```C
{
   instruction
}
```

may be replaced directly by 

```C
instruction
```

### NanoProject 2.2 ###

Currently, a _sequence_ may contain an arbitrary number of
expressions. Modify the system so sequences can only contain two
expressions.

In other words, this is a program transformation where blocks like

```C
{
   instruction1
   instruction2
   instruction3
   instruction4
}
```

can be replaced directly by 

```C
{ 
  instruction1
  {
    instruction2
    {
      instruction3
      instruction4
    }
  }
}
```

### NanoProject 2.3 ###

Currently, a _block_ may contain no _binding_. When a block does not
introduce new local variables then it is similar to a sequence of
instructions. Modify the system in order to remove blocks without
bindings in favor of sequences.

### NanoProject 2.4 ###

Currently, a _codefinition_ may exist without any local _function_
definition. This codefinition is therefore equivalent to a sequence.
Modify the system in order to remove these useless codefinitions.

### NanoProject 2.5 ###

Currently, alternatives may be binary or ternary depending on whether
they have an alternant or not. Transform all alternatives so they all
have an alternant.

### NanoProjet 2.6 ###

Adjoin a new primitive function to the system. This primitive named
`now` will return the current time of the day (expressed in milliseconds).

### NanoProject 2.7 ###

Adjoin a new primitive function to the system. This primitive, a
predicate (with a boolean result) named `implies` takes two values and
return false only when the first argument is true and the second
argument is false.

### NanoProject 2.8 ###

Adjoin the `sin` primitive to the system. That primitive computes the
sine of a number.

### NanoProject 2.9 ###

Adjoin the `hypotenuse` primitive to the system. That primitive takes
two numbers, say `a` and `b` and computes the square root of `a^2 + b^2`.


Hints
-----

The entry point for tests is the class `InterpreterTest`, look at it
to find where you may add your own code.

No need to alter the grammar for this lab session.

Don't forget that, in ILP9, every value is true but `Boolean.FALSE`.
Don't forget that, in ILP9, values are represented by `BigInteger`,
`BigDecimal`, `String` and `Boolean`.

Of course, your code must be tested, automatically! If you write new
ILP9 programs, name them `u00xyz-1.xml` (where xyz is some number) and
write their associated `u00xyz-1.result` and `u00xyz-1.print` files.

Good luck.