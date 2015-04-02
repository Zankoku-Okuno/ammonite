Research Directions
===================

Just some thoughts I've had concerning the tricker implementation details.
Perhaps someone has already solved these, I just haven't read the paper.

Side-effectful Thunks and Concurrency
-------------------------------------

I thought that passing a thunk to another thread would force the thunk, since primitives force their arguments and `channelPut` would be just such a primitive.
However, closures can also be passed around threads, in particular the thread entry point will often be a closure.
Therefore, we really do need a solution for concurrent forcing of thunks.
The most obvious way is to throw a lock around each thunk, but are there any less blocking solutions?

Mutable Environments and Scope Determination
--------------------------------------------

When environments are mutable, a name that was once bound to a closure-constructor might suddenly become bound to something entirely different.
As such, we cannot tell just from the names in an expression and its current context whether a particular subexpression has any given names bound or to what.
What restrictions on the mutability of environments are necessary to allow us to translate into a deBruijn formulation?