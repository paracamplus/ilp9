
Course 2 lab session (2 hours + group work)
===========================================

These nano-projects focus on the interpreter. Ignore the compiler for now.

NanoProjects
------------

### NanoProject 2.1 ###

Currently, some sequences may contain only one expression. A sequence
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

Currently, some sequences may contain an arbitrary number of
expressions. Modify the system so sequences can only contain two
expressions.

In other words, this is a program transformation where blocks like

```C
{
   instruction1
   instruction2
   instruction3
}
```

may be replaced directly by 

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

Currently, a block may contain no binding. When a block does not
introduce new local variables then it is similar to a sequence of
instructions. Modify the system in order to remove blocks without
bindings.

### NanoProject 2.4 ###

Currently, a codefinition may exist without any local function
definition. This codefinition is therefore equivalent to a sequence.
Modify the system in order to remove these useless codefinitions.

### NanoProject 2.5 ###

Adjoin a new primitive function to the system. This primitive named
`now` will return the current time of the day (expressed in milliseconds).

### NanoProject 2.6 ###

Adjoin a new primitive function to the system. This primitive, a
predicate (with a boolean result) named `implies` takes two values and
return false only when the first argument is true and the second
argument is false.

### NanoProject 2.7 ###

Adjoin the `sin` primitive to the system. That primitive computes the
sine of a number.

### NanoProject 2.8 ###

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
