
Course 1 lab session (2 hours)
==============================

Preliminaries
-------------

Download code for ilp9 with

```shell
git clone https://github.com/paracamplus/ilp9.git
```

With Eclipse, create the ilp9 project and run (successfully) the test
suite com.paracamplus.ilp6.test.WholeTestSuite.java

You may share questions, answers or other useful information on
the [ilp9 group](https://groups.google.com/forum/#!forum/ilp9-2015).
This is a restricted group so please ask to be part of it!

NanoProject
-----------

There is only one nano-project for all groups for this first course.

### NanoProject 1 - question 1.1 ###

Write an XML program that introduces one local variable, say `a`,
binds it to 3 then multiplies `a` by 2. Something that may be expressed
as (in Caml):

```caml
let a = 3 
in 2 * a
```

### NanoProject 1 - question 1.2 ###

Turn this program into an additional test (name that test `u00-1.xml`).
Make sure you know how to run the entire test suite (including that
new test) or just this new single test.


Hints
-----

Study the `grammar9.rnc` to know the tags to use to write the program.

Don't forget to write also files `u00-1.result` and `u00-1.print`.
Look at other tests (in `Samples/`) and study the entry point
`InterpreterTest` to see why they are needed.

Good luck.




