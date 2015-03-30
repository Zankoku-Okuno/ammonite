Ammonite Project Status
=======================

This document details each phase of Ammonite interpretation and what needs to be done to complete the system.

As a broad overview: we are wrapping up the sytnax of Ammonite and moving on to interpretation.
Only user-defined distfixes are missing from the syntax.
We need documentation of the language syntax.

Module Responsibilities
-----------------------

Language.Ammonite
	Syntax
		Concrete -- define the Syntax types for the user, textual representation
		Abstract -- define the AST types for the desugared, internal representation
		Parser -- the parser functions (String -> Syntax)
		Printer -- pretty-print AST, and reproduce String from Syntax
		Binary -- read/write binary-formatted AST
		Sugar -- coordinate desugaring procedure (Syntax -> AST)
			Distfix -- algorithms for finding and rewriting distfixes
				Parser -- parse a distfix definition file
				Defaults -- the default distfixes
			DotExpr -- algorithms for finding and rewriting dotted exprs
			AnonPoint -- algorithms to rewrite anon points
			ToAbstract -- convert desugared concrete syntax into abstract syntax
	Gensym -- functions for working with gensyms (for anon points, type tags, cues, &c)
	Load -- manage file imports, both distfixes and values


Parsing
-------

Feature freeze is in effect.

Although, I am considering syntax for `e :: e` and finding a specialized syntax for modules.


Desugaring
----------

	[ ] load and parse distfix file
		[*] parser
		[ ] general directive handler utils
		[ ] recognize distfix directives and pump to parser
		[*] file loader
	[*] or use default
	[*] look through Combine for distfixes and re-write
	[ ] check that no distfix parts are left over
	[*] desugar dotted expressions (outside of Combine, they are "misplaced")
	[*] anonymous points (probably should be thunks)
	[*] translate to AST


Interpretation
--------------

FIXME: changes lists to seqs where xs ++ [x] occurs

	[ ] Environments
		[*] binding
		[*] lookup
		[ ] deconstructor scope
	[ ] Machine monad
		[*] push/pop continuations
		[*] environment swap
		[ ] Result type (good | fatal error)
		[ ] abort
		[ ] stack marks
		[ ] capture, capture+abort
		[ ] thread-local data store
	[ ] MultiMachine
	[ ] Evaluator
		[*] self-evaluating terms
		[ ] basic structual types
			[*] construction
			[ ] inspection
			[ ] modification
		[ ] record type
		[ ] expression type
		[ ] string interpolation
		[ ] modules
		[ ] abstract types
		[*] curried primitives
		[*] thunk forcing
		[ ] major primitives
			[*] define
			[*] lambda
			[*] vau
			[ ] eval
			[ ] modify environments
		[ ] applicative call
		[ ] operative call
		[ ] pluggable builtins
		[ ] pattern matching
			[*] names
			[ ] literals
			[ ] structural types
			[ ] deconstructors (ap)
	[ ] Import


For Future Reference
--------------------

We can stack loaders, so that one relies on the other.
So, the loader for evaling files relies on the loader for parsing files relies on the filesystem.

When lambdas bind multiple args at once, it is a seq, not an ap. Ap is for deconstructors.

You know, it might be nice to allow abstract types to be callable and accessible/updatable.
Not the structural types, though.