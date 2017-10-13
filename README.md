# SAT.rkt

A simple SAT solver based on DPLL algorithm, written in Racket. Only less than 100 lines of code.

### Usages

As a library, `(require "dpll.rkt")` to load the library, 

* use `(solve filepath)` given a filepath of DIMACS file
* use `(get-model)` to get the assignment of variables
* use `(parse-dimacs-file filepath)` to transform content of a DIMACS file to S-Expressions
* use `(check-sat clauses)` to check satisfiability given clauses in S-Expression form

### TODO

* Shift to Racket package system
* Improve performance, use Conflict-Driven Clause Learning
