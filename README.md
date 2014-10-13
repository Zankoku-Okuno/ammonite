Ammonite
========

* A Lisp for people who like ML dialects,
* An ML for people who like Lisp dialects,
* and some extra stuff for good measure.


I find myself using Haskell primarily, and that's sad, because it's not a Lisp.
I use Haskell because it surpasses Lisps at lambda calculus, hierarchical structuring, making refactoring easy, and ad-hoc polymorphism.
But I know the power of Lisp, and would really like to leverage it, if only I didn't have to sacrifice Haskell's better qualities.
This is why I've created Ammonite.

Features
========

Curried Functions
-----------------

All operators (functions and special forms) take one argument and produce one result.
The only reason parenthesis are needed in Lisp is because functions take variable numbers of arguments.
This frees Ammonite up to use parenthesis to disambiguate grouping, just like in every non-Lisp mathematical and programming language we know and love.
Further, it makes partial application easy:
why write `(lambda (xs) (map f xs))` when you could write `map f`?

Indentation-Sensitivity
-----------------------

TODO

Compound Data Structures
------------------------

Certain data structures appear so commonly that we build them in: lists and structures.
A list is as usual, but unlike Lisp, we write them like `[1, 2, 3]`.
A struct is a map identifiers to values, something like a C struct, but very dynamic; we eti
Having special syntax makes it easy to see when you're computing vs. when you're building data.

Both of these data structures are immutable,
but a better adjective is "persistant" as opposed to "ephemeral".
When you manipulate a list or struct, you can be sure that you aren't performing a destructive operation;
the likelihood of those insidious data corruption bugs is dramatically reduced.

Beyond these, we also have a record type, which is a combination of list and struct.
Records have both positional data and named data, and there's special syntax to splice in more of each.
When you want a function of more than one argument, with default arguments, keyword arguments, or variable-length arguments, Ammonite turns to records.
We'll get to examples of records in a little while, once we've seen a bit more syntax.
For now, it suffices to say that function calls involving records look very similar to Python syntax.

Once we have compound data structures, we'll want to traverse them.
Ammonite provides simple syntax do do this.
For example, accessing a field of a struct is as simple as `foo.x`
Indexing a list looks like `arr[5]`.
You can also use negative indices and slice lists: `arr[low_limit ... -1]`.
After traversing to some inner data, you can perform non-destructive updates: `def foo2 foo.x=1`.
And of course, any of these can be nested: `foo.x.y=foo[bar[... limit][-1]]`.

Pattern Matching
----------------

TODO pattern matching
TODO this lets multiple return results `def (a, b) (b, add a b)`

Beyond Prefix Notation
----------------------

Let's face it, pure prefix notation is weird.
For that, Ammonite provides a couple handly syntaxes.

The first is dotted-expression syntax.
When you find youself writing silly code like `equals? 1 2`, why not write `1 .equals? 2`?
They are precicely equivalent.
You can also chain dot-expressions: `1 .add 2 .add 3` is just like `(add (add 1 2) 3))`.
You can even use the dot before a complex expression: `xs .(foldr max 0)` is the same as `foldr max 0 xs`.

And then infixing sometimes isn't enough, so Ammonite allows user-defined distfixes.
The best example is if-then-else.
Provided you've defined the appropriate distfix, we write `if foo then bar me else baz you`, this actually gets translated into `if_then_else_ foo (bar me) (baz you)`.
As we'll see later on, you can even define you own `if_then_else_` wihout much difficulty.

Partial Application
-------------------

Of course, just by having curried functions, it's easy to perform certain parital applications.
For example, `map (add 1)` adds one to every element of a list.
But, what if we wanted to subtract one from every element?
Why write `map (flip sub 1)` or `map (lambda (x) (sub x 1))`, when you can just write `map (sub _ 1)`?
Anonymous points, written `_`, create the lambdas for you.


TODO syntactic abstraction (= first-class expressions, first-class environments, and the vau operator)
	we don't use macros, macros are second-class; we allow you to write your own special forms, indistinguishable from the builtin forms
