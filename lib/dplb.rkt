#lang racket

;; A simple SAT solver using iterative DPLL with
;; backjumping and conflict learning.
;; It is a Racket implementation of the `dplb` function
;; in Handbook of Practical Logic and Automated Reasoning.

(require "parser.rkt")
(require "dpll.rkt")
(require "dpli.rkt")

(provide dplb check-sat^^ get-model^^ solve^^)

(define (backjump f p trail)
  (match (backtrack trail)
    [(cons (cons q 'Guessed) tt)
     (define-values (f^ trail^) (unit-propagate f (cons (cons p 'Guessed) tt)))
     (if (memf (compose zero? set-count) f^)
         (backjump f p tt)
         trail)]
    [_ trail]))

(define (dplb f trail)
  (define-values (f^ trail^) (unit-propagate f trail))
  (cond
    [(memf (compose zero? set-count) f^)
     (match (backtrack trail^)
       [(cons (cons p 'Guessed) tt)
        (define trail^^ (backjump f p tt))
        (define conflits (map car (filter (compose ((curry equal?) 'Guessed) cdr) trail^^)))
        (define new-clause (set-add (list->set (map neg conflits)) (neg p)))
        (dplb (cons new-clause f) (cons (cons (neg p) 'Deduced) trail^^))
        ]
       [_ #f])]
    [else (define unassgn (unassigned f trail^))
          (cond
            [(zero? (set-count unassgn)) trail^]
            [else (define p
                    (argmax (posneg-count f^) (set->list unassgn)))
                  (dplb f (cons (cons p 'Guessed) trail^))])]))


(define (check-sat^^ f)
  (define result (dplb f '()))
  (if result #t #f))

(define (get-model^^ f)
  (define result (dplb f '()))
  (if result (map car result) #f))

(define (solve^^ filename)
  (check-sat^^ (parse-dimacs-file filename)))