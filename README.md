This is a lambda calculus --> Python compiler. Given an lambda calculus expression like `(x.x)(x.x)`, Compiler.hs will emit a Python program that evaluates (beta-reduces) the expression and prints the reduced expression. For example, here the answer would be `x.x`. Unapplied expressions are also beta-reduced. Check out `test.py` to see more examples.

The conceptual foundation is the CEK machine (http://matt.might.net/articles/cek-machines/), a type of state machine for representing lambda calculus terms.

### Caveats

This code was written for an interview project. I have never written production Haskell before. Although I think the code quality is reasonable, the code was optimized for comprehension by my interviewer and there may be extraneous elements or abstractions that could be better.
