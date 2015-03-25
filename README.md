Ammonite
========

The language with more dynamism, more power, more simplicity.

* A Lisp for people who like ML dialects,
* An ML for people who like Lisp dialects,
* and some extra stuff for good measure.


I find myself using Haskell primarily, and that's sad, because it's not a Lisp.
I use Haskell because it surpasses Lisps at lambda calculus, hierarchical structuring, making refactoring easy, and ad-hoc polymorphism.
But I know the power of Lisp, and would really like to leverage it, if only I didn't have to sacrifice Haskell's better qualities.
This is why I've created Ammonite.

Features
========

A Full Suite of Abstraction Tools
---------------------------------

This one is up front becuse it is really the only important one.

Algorithmic abstraction is provided through first-class functions with statically-scoped closure.
This used to be a big deal, but even mainstream langauges have gotten around to moving towards this ideal.
However, if you haven't run into this feature (lambdas) before, go get educated, because it's got the biggest simplicity-to-power ratio of any language feature in the history of computer science, no exaggeration required.

Data abstraction is provided with a combination of tagged data and module-based namespace control.
In some dynamically-typed languages, all the internals are exposed, and it's up to the user to not try to look behind the curtain, but abstract data is more than that.
To be abstract, it must not only provide a specific interface, it must also do nothing beyond what it specified in that interface.
Ammonite can invent new data types as needed with efficient tags, a constructor and a deconstructor.
Modules for namespace-control allow for the internals to be hidden, so the provided constructors and deconstructors can be hidden behind your own, smarter constructors.
Like everything in Ammonite, data types and modules are created on-the-fly and first-class.

Syntactic abstraction in Ammonite is provided through the vau calculus.
For a long time, there was no need for the phrase "syntactic abstraction", because what it really meant was "macros".
However, macros are second-class and can lead to massive increases in code size.
Recently, there have been new advances in syntactic abstraction using a new flavor of f-expression which entirely subsumes macro systems.
The vau calculus provides first-class environments and expressions, making hygenic and unhygenic macros both simple to a fault.
With ammonite, not only is your new sytnax indistinguishable from the built-in syntax, but these "special forms" are even indistinguishable from regular functions.

Finally, we offer control-flow abstraction.
This is an odd one because it is so new (at time of writing, the author is still writing his thesis outlining the system), but the point is that you can efficiently define and implement any control structure you can imagine, even non-local ones.
Even better, the control structures you invent play nice with exceptions and scarce resource management.

TODO concurrency/laziness, but I've yet to come up with suitable language (computational sequence abstraction?)

So there you have it: if you're writing boilerplate in Ammonite, you're doing it wrong.
Which means, if you're writing boilerplate, you might want to consider Ammonite.

Slowness
--------

As programmers, we love to watch the blinkenlights.

Seriously, though, Ammonite is meant for expressiveness first, speed after all other considerations.
We take a big design principle from Arc here: waste as much as you want, because in a couple decades' time, everyone will be wasting that much.

Having said that, our primitives are built on fast libraries (gmp for big rationals, finger trees for strings), and we want to develop foreign function interfaces in the future.
So, even if you are worried about speed, Ammonite may still be your best choice for glue code.

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
After traversing to some inner data, you can perform non-destructive updates: `def foo2 foo.x:=1`.
And of course, any of these can be nested: `foo.x.y:=foo[bar[... limit][-1]]`.

Serious Syntax
--------------

Good syntax can make a language rewarding to use, but bad syntax can turn you away forever.
Lisp is notorious for having just too many parenthesis, and that turns people away.
Ammonite might not have the most familiar syntax, but it's design does good work at maintaining both readability and concision.


All operators (functions and special forms) are curried: they take one argument and produce one result.
The only reason parenthesis are needed in Lisp is because functions take variable numbers of arguments.
This frees Ammonite up to use parenthesis to disambiguate grouping, just like in every non-Lisp mathematical and programming language we know and love.
Further, it makes partial application easy:
why write `(lambda (xs) (map f xs))` when you could write `map f`?

Of course, just by having curried functions, it's easy to perform certain parital applications.
For example, `map (add 1)` adds one to every element of a list.
But, what if we wanted to subtract one from every element?
Why write `map (flip sub 1)` or `map (lambda (x) (sub x 1))`, when you can just write `map (sub _ 1)`?
Anonymous points, written `_`, create the lambdas for you.

Ammonite is indentation-sensitive.
Indentation means a lot to humans, so if your compiler/interpreter ignores it, there's an extra gap between you and the machine.
More interestingly, you can swap between indentation-sensitive and -insensitive styles at will, so you can maximize (or minimize) the readability of your code.

Let's face it, pure prefix notation is weird.
For that, Ammonite provides a couple handly syntaxes that break free of prefix notation without sacrificing the simplicity of the abstract syntax.

The first is dotted-expression syntax.
When you find youself writing silly code like `equals? 1 2`, why not write `1 .equals? 2`?
They are precicely equivalent.
You can also chain dot-expressions: `1 .add 2 .add 3` is just like `(add (add 1 2) 3))`.
You can even use the dot before a compound expression: `xs .(foldr max 0)` is the same as `foldr max 0 xs`.

And when infixing isn't enough, Ammonite allows user-defined distfixes.
The best example is if-then-else.
Provided you've defined the appropriate distfix, we write `if foo then bar me else baz you`, this actually gets translated into `if_then_else_ foo (bar me) (baz you)`.
As we'll see later on, you can even define you own `if_then_else_` without much difficulty.

Pattern Matching
----------------

TODO pattern matching

TODO this lets multiple return results `def (a, b) (b, add a b)`



