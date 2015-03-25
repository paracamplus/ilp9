Final projects
==============

The projects focus on the compiler (you may safely ignore the
interpreter) or on the interpreter (you may then safely ignore the
compiler) in order to add new features to the ILP9 language. This
implies to extend the grammar, the parser, the compiler (for projects
2-7) or the interpreter (for projects 8) and their respective
runtime library.

Projects
--------

### Project 1 ###

Devise and implement a concrete syntax for ILP9. You may
use ANTLR as a parser generator.

### Project 2 ###

The binary boolean operators require, in order to be applied, the
evaluation of both their operands. Very often, in programming
languages, `and` and `or` are specific keywords with a short-circuit
semantics that is, the left operand is always evaluated but, whenever
the final result may be known, the evaluation of the right operand is not
performed. For example, `false and ...` is always false independently
of what `...` might be. Similarly `true or ...` is always true.

Implement these new keywords `and` and `or`.

### Project 3 ###

Enrich the definition of functions to allow default values
for missing variables. For instance,

```ilp
let f = function (x, y = 4, z = 2*x) {
           print x, y, z
        }
in         f(1);      // prints 1, 4, 2
           f(1,2);    // prints 1, 2, 2
           f(1,2,3);  // prints 1, 2, 3
```

### Project 4 ###

Optimize the AST of ILP9 in order to remove common sub-expressions.
For example,

```ilp
(2*x) + f(2*x) ==  let t = 2*x
                   in t + f(t)
```

### Project 5 ###

Add vectors to ILP9 that is, add new syntaxes to allocate, read and
write vectors and also obtain the length of a vector as in:

```ilp
let v = [1, "a" + "b"]
in v[1] = v.length * v[0];
```

Most of these new characteristics may be implemented as functions.

### Project 6 ###

Implement a cache for method invocation.

The `ILP_find_method` look for the appropriate method: it checks that
the receiver is an instance of the class that introduces the method,
it checks the arity of the invocation, it computes the offset where is
stored the function implementing the method, it returns that function.

Benchmarks show that it is useful to memorize the found methods since
they will probably be fetched again soon. This temporal locality can
be put at work with caches memorizing the last method(s) found. The
best known solution is one cache per invocation site (four of them in
the next example):

```ilp
class Point extends Object {
   field x 
   field y
   method longueur () {
      return this.x + this.y
   }
   method distance (p) {
      return this.longueur()           // site 
           + p.longueur()              // site
}
class PointColore extends Point
   field color
   method longueur () {
      return 2 * super()
   }
}

let c = 1 in
let p0 = new PointColore(0, 0, "red") in
while ( true ) {
   let p1 = new Point(c, ++c) in
   p0.print()                          // site
   p1.distance(p0)                     // site
   p0 = p1
}
```

You may use the `Samples/Scheme/bu8999-6.scm` file for benchmarks.

### Project 7 ###

Retarget the compiler to generate Java code instead of C code.

### Project 8 ###

Embed ILP9 as a scripting language of the JVM according to JSR 223.
The embedding should provide shared global variables between Java and
ILP9. It must also be possible to call regular Java methods from ILP9
as in:

```
d = new java.util.Date()
print d.getTime()
```


Hints
-----

Remember that you have to formally present your projects at the final
meeting of April 1st. Code, test, prepare the presentation, rehearse!

Organize your work in small steps so you have at least one thing to show.

Don't forget to precise the license (GPL3 is suggested) for your code,
Your code might be useful for new editions of this course.

Good luck!

