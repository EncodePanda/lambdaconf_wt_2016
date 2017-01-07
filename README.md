# "Learning FP concepts for greater good"

## Prepare your set-up!

It is adived that prior to arriving at the workshop you will:

1. git clone this repository
2. compile it with sbt `sbt compile` to fetch all needed dependencies

You can use whatever IDE or text editor you prefer. 
This repository will contain fine-grained commits that will slowly extend our workspace. If you want to follow the presentation on your machine, I recommend setting up a short-cut to "jump" into next commit.

## About this workshop

*TL;DR*
1. This is a intro to FP with Scala - minimum: You understand basic language syntax, you understand how implicit arguments work
2. Intended from zero to hero however it is assumed that some audience is familiarized with trivial concepts
(like: higer order functions, immutability)

*Detailed*

This talk is designed for all those interested in learning Functional Programming, to tackle real world problems that they stamp upon every day at work. Functional Programming brings sanity to our industry as it gives you ability to handle complexity with modular & composable code.
Keep in mind however that Functional Programming is not a magic wand that will suddenly make complexity disappear. If your problem is relatively difficult, simple immutable data structures and function composition will not suffice. Complexity is hard, simplicity is however not easy. Good news is: all problems you've encountered have been solved decades ago. Knowing them will prevent you from rediscovering the wheel.

You want to attend this session if you are interested in Functional Programming in general & are seriously devoted to making the next big step into the rabbit's hole.

This session will be an interactive presentation: you can take your laptop with you and try to follow or sit back and listen - whatever works for you best.

Programming language: Scala + ScalaZ (if you prefer Cats, you should be fine as well)

You are: professional software developer.

Things you should already know:

- Immutability
- Higher order functions & lambdas
- Pattern matching
- General recursion
- Curring

In other words: Novice & (partially) Advanced Beginner (*)

Things you will learn:
- Algebraic Data Types
-  Typeclasses (the general idea; some representatives: Show, Semigroup, Monoid; (bonus) Enlighment: *scary* Monad - is just another typeclass)
- Applicative 
- Reader
- Monad transformers
- State
- Task
- Free monad

In other words: lifting you from Novice/Advanced Beginner to a Competent apprentice of Functional Programming (*)

If time will allow us, at the very end we will study source code of Quasar Analytics - where all above mentioned technics are being used to some extent. We will see how they allow keeping code modular & composable, despite rather complex domain.

Note about language used: you should be at least basically familiar with Scala language. You don't need to have professional experience with the language, but understanding its basic constructs will be mandatory. You should at least know: how to define class, methods, functions. Have general understanding of pattern matching & implicit arguments.

(*) see http://lambdaconf.us/downloads/documents/lambdaconf_slfp.pdf
