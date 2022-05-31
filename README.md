# CS510 Advanced Functional Programming Project

## Mike Bottini

# What is this?

This is a Lisp interpreter, implemented in Haskell. The program parses strings
into S-expressions, evaluates them, and returns the result.

# What's implemented?

* Parsing and evaluating of S-expressions.
* The list operations described in John McCarthy's 1960 paper, including
`cons`, `car`, `cdr`, `atom`, `eq`, `lambda`, `if`, and `cond`.
* A `label` function, described by Paul Graham's *The Roots of Lisp* in place
of the Y combinator, which enables recursion.
* The Y combinator is implemented as well for funsies.
* Local variable capture, meaning that inner lambda functions capture the
variables declared in outer lambda functions.
* Global variables, implemented with the `set!` builtin function and the
`State` / `StateT` monads.
* Rudimentary error handling is done with the `Either` monad, both parsing and
evaluation.
* Prelude, implemented in Lisp. This includes higher-order functions like
`map` and `foldl`.

# What's not implemented?

* Macros.
* Other atomic types that aren't integers (strings, characters, keywords, etc).
* Interactive Lisp with reading and writing - that would require putting
everything inside the IO monad.
