On Lisp Syntax
--------------

Lisp syntax has a number of problems.
That's why Ammonite has something better.

 * Too Many Parenthesis --
    Who hasn't made this complaint?
    It's nevertheless true, and perhaps truer precisely because so many people complain about it.
    Parenthesis are only needed in Lisp because it's not known ahead of time how many arguments a Lisp function takes.
    Instead of every function taking variadic arguments, every Ammonite function takes one argument.
    This is called Currying, and it means we can take advantage of prefix notation without obligatory parenthesis.

 * Still Too Many Parenthesis --
    All sorts of special forms require parenthesis around pairs.
    Particular offenders are begin, let, and cond.
    Ammonite has special semicolon-based expression sequencing, just like those imperative languages.
    What's more, you can even leave out the semicolons using indentation-sensitivity.
    And just to one-up every language on the planet, you can mix both styles effortlessly.

 * Not Enough Data Structures --
    Some data structures -- sequences and records -- are everywhere, and they deserve support.
    If you want a sequence in Lisp, you're forced to use a linked list, because that's what the language is based on.
    If you want a record in Lisp, you have to write your own accessors, mutator, etc.
    Ammonite gives them syntactic support.
    Sequences are an abstract type, so the implementation is free to do something efficient.
    Records get syntactic support, so you never write another stupid throwaway accessor/mutator.

 * Ugly Keyword Argument Support --
    Solutions to the keyword argument problem in Lisp are ugly, because they break that (overly) pure syntax.
    Ammonite just looks like a normal language.
    Wait, I thought we were Currying all functions?
    Yes, but we also give syntactic support to records that look like `(1, 2, 3, foo: 4, bar: 5)`.
    Welcome back familiarity: `print ("foo", "bar", sep: ',')`.

 * Prefix Notation Will Bite You --
    The plus sign goes between two numbers.
    We all learned this in, what, kindergarten?
    Yet Lisp still wants us to put the plus sign where?
    And it only gets worse with non-commutative operators, like comparison and subtraction.
    Ammonite allows the programmer to use infixes and, even better, distfixes.
    You get to decide what the operators are, what their precedence is, and how complicated they are.
    If you're a masochist, you can even turn off everything you learned in preschool.

 * Still Not Enough Infixes --
    It's annoying to have to define your operators, it always will be.
    But with Ammonite, just because you were too lazy to define them doesn't mean you can't use them.
    What? I know, right.
    With dotted expressions like `2 .add 2`, its like you'remaking method calls, but you don't need the objects.
    Even better, this feature is orthogonal to the others: `deadbeef .(lettuce .wrap)`.

 * Too Dang Punny --
    Lisp syntax has this insidious feature whereby expressions are actually represented as lists.
    That's great for make concise quips, but not really good for data abstraction.
    Ammonite uses an abstract data type to represent Ammonite programs.
    That is, code is still data in Ammonite, but now its explicit, and therefore easier to think about.

 * Not Enough Partial Evaluation --
    Partial evaluation is a great thing, SICP uses it in tons of examples.
    Too bad that it always looks ugly: `((foo x) y)`.
    Currying already solves this problem: `foo x y` or `foo x` for the partial version.
    No problem

 * Still Not Enough Partial Evaluation --
    In languages that already have currying, what if you want to apply an argument that isn't the next one?
    Ammonite adds anonymous points.
    Now, instead of `lambda x (foo x y)`, you can just write `(foo _ y)`.

That's all very nice, but does it work?
Try these examples on for size.

```
even? x is :cond
  x = 0 -> True
  x = 1 -> False
  else -> even? (x - 2)

map .lookup key
key .(lookup map)
find-my-key is (lookup _ key)
map .find-my-key

cons x y i is case i of :
  1 -> x
  2 -> y
car is (_ 1)
cdr is (_ 2)

t1 is {first: 1, second: 2, third: 3}
t2 is t1.second:=t1.first
```

If you're still not convinced, try reading them in Lisp:

```
(define (even? x) 
  (cond ((= x 0) #t)
        ((= x 1) #f)
        (else (even? (- x 2))))

(lookup map key)
(lookup map key)
(define (find-my-key dumb-name) (lookup dumb-mame key)
(find-my-key map)

(define (cons x y i)
  (cond ((i = 1) x)
        ((i = 2) y)))
(define (car x) (x 1))
(define (cdr x) (x 2))

(define (mk-triple a b c) (list a b c))
(define (triple-first x) (car x))
(define (triple-second x) (cadr x))
(define (triple-third x) (caddr x))
(define (triple-set-first x a) (mk-triple (list a (triple-second x) (triple-third x)))
(define (triple-set-first x b) (mk-triple (list (triple-first x) b (triple-third x)))
(define (triple-set-first x c) (mk-triple (list (triple-first x) (triple-second x) c))
(define t1 (mk-triple 1 2 3))
(define t2 (triple-set-second t1 (triple-first t1)))
```

Did you spot the mistake?
What about the other one?
And the other other one?
There's one more, too.
Actually, how many mistakes are there?
