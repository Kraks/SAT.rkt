# SAT.rkt

A simple SAT solver based on DPLL algorithm, written in Racket. Only less than 100 lines of code.

### Usages

Use `raco pkg install SAT` to install this package.

As a library, use `(require SAT)` to load the library, 

* use `(solve filepath)` given a filepath of DIMACS file
* use `(get-model)` to get the assignment of variables
* use `(parse-dimacs-file filepath)` to transform content of a DIMACS file to S-Expressions
* use `(check-sat clauses)` to check satisfiability given clauses in S-Expression form

### TODO

* Improve performance, use Conflict-Driven Clause Learning
