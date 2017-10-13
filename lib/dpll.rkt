#lang racket
;; A simple SAT solver using DPLL algorithm.
;; Author: Guannan Wei

#| Input format:
((n_1 n_2 (not n_3)) ...)
|#

(require "parser.rkt")

(provide (all-defined-out))

(define assignment (make-hash))

(define (get-var var)
  (match var
    [`(not ,var^) var^]
    [else var]))

(define (not-neg? var)
  (match var
    [`(not ,var^) #f]
    [else #t]))

(define (propagate var val clauses)
  (hash-set! assignment var val)
  (define (propagate-clause clause)
    (cond [(empty? clause) clause]
          [(eq? (get-var (first clause)) var)
           (if (eq? (not-neg? (first clause)) val)
               #f ; we can eliminate this clause
               (propagate-clause (rest clause)))]
          [(propagate-clause (rest clause)) =>
           (λ (lst) (cons (first clause) lst))]
          [else #f]))
  (filter-map propagate-clause clauses))

(define (iter-propagate vars val-f init-clauses)
  (let loop ([vars vars] 
             [clauses init-clauses])
    (cond [(empty? vars) clauses]
          [else (loop (rest vars)
                      (propagate (get-var (first vars))
                                 (val-f (first vars))
                                 clauses))])))

(define (get-unit-clauses clauses)
  (for/list ([clause clauses] #:when (eq? (length clause) 1))
    (first clause)))

; If clause c1 is a literal and contains a variable p,
; and if clause c2 contains (not p), then reduce c2 to
; a new clasue without (not p) and discard c1
(define (eliminate-unit-clause clauses)
  (iter-propagate (get-unit-clauses clauses) not-neg? clauses))

(define (get-all-variables clauses)
  (apply set (foldl append '() clauses)))

(define (appear-only-once? v cmp lst)
  (cond [(empty? lst) #f]
        [(cmp v (first lst))
         (not (appear-only-once? v cmp (rest lst)))]
        [else (appear-only-once? v cmp (rest lst))]))

; Get a list of pure variables that only appears either negated
; or not negated
(define (get-pure-variables clauses)
  (let ([all-vars (set->list (get-all-variables clauses))])
    (filter (λ (v) (appear-only-once? v
                    (λ (x y) (equal? (get-var x) (get-var y)))
                    all-vars)) all-vars)))

; If a variable p occurs only positively, then p must be ⊤
; If a variable p occurs only negatively, then p must be ⊥
(define (eliminate-pure-variables clauses)
  (iter-propagate (get-pure-variables clauses) not-neg? clauses))

(define (choose-var clauses)
  (cond [(empty? clauses) (error "empty clauses")]
        [else (first (first clauses))]))

(define (dpll clauses)
  (cond [(zero? (length clauses)) #t] ;immediately sat
        [(memf (compose zero? length) clauses) #f] ;immediately unsat
        [else (let ([clauses (eliminate-pure-variables
                              (eliminate-unit-clause clauses))])
                (cond [(zero? (length clauses)) #t]
                      [(memf (compose zero? length) clauses) #f]
                      [else (let ([new-var (get-var (choose-var clauses))])
                              (or (dpll (propagate new-var #t clauses))
                                  (dpll (propagate new-var #f clauses))))]))]))

(define (check-sat clauses)
  (hash-clear! assignment)
  (dpll clauses))

(define (get-model) assignment)

(define (solve filename)
  (check-sat (parse-dimacs-file filename)))
