# MiniML

\*[Demo]("https://youtube.com/shorts/zUkOLkRqYEE?feature=share")
This was my final project for CS 51: Design and Abstraction in Computation (taught entirely in OCaml).
It’s a tiny Turing-complete language and REPL defined and evaluated in OCaml with support for atomic data types, if-else-then conditionals, let-bound variables, and recursion.
Expressions can be evaluated by one of three evaluators supporting substitution, dynamic environment, and lexical environment semantics.
The course staff templated input parsing (the .mll and .mly files), the CLI interaction handler (miniml.ml), the signature for the env module, and the data types defined in expr.ml. I built the rest and modified the templates as needed.
All three evaluators are fully functional. Substitution was the most fun to implement, dynamic was the most challenging, and lexical was my favorite. If you’d like to try out the project for yourself, feel free to clone the repo, run `make all` from the root directory, and start the REPL with `./miniml.byte`.
