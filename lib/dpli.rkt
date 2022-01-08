#lang racket

;; A simple SAT solver using iterative DPLL algorithm
;; It is a Racket implementation of the `dpli` function
;; in Handbook of Practical Logic and Automated Reasoning.

(require "utils.rkt")
(require "parser.rkt")
(provide unassigned unit-propagate backtrack posneg-count dpli)

(define (unassigned f trail)
  (define fv (filter
              positive?
              (set->list (get-all-vars f))))
  (define tv (filter
              positive?
              (map (compose abs car) trail)))  
  (set-subtract (list->set fv) (list->set tv)))

(define (unit-subpropagate f assgn trail)
  (define f^ (map
              (λ (c)
                (foldl
                 (λ (l s)
                   (if (set-member? assgn (neg l))
                       s
                       (set-add s l)))
                 (set)
                 (set->list c)))
              f))
  (define new-units (foldl
                     (λ (c s)
                       (if (and (= (set-count c) 1)
                                (not (set-member? assgn (set-first c))))
                           (set-add s (set-first c))
                           s))
                     (set)
                     f^))
  (cond
    [(set-empty? new-units) (values f^ assgn trail)]
    [else (define trail^ (append (set-map new-units (λ (l) (cons l 'Deduced))) trail))
          (define assgn^ (set-union new-units assgn))
          (unit-subpropagate f^ assgn^ trail^)]))

(define (unit-propagate f trail)
  (define assgn (list->set (map car trail)))
  (define-values (f^ assgn^ trail^) (unit-subpropagate f assgn trail))
  (values f^ trail^))

(define (backtrack trail)
  (match trail
    [(cons (cons p 'Deduced) tt) (backtrack tt)]
    [_ trail]))

(define (posneg-count f)
  (λ (v)
    (define pos-len (length (filter (λ (c) (set-member? c v)) f)))
    (define neg-len (length (filter (λ (c) (set-member? c (neg v))) f)))
    (+ pos-len neg-len)))

(define (dpli f trail)
  (define-values (f^ trail^) (unit-propagate f trail))
  (cond
    [(memf (compose zero? set-count) f^)
     (match (backtrack trail^)
       [(cons (cons p 'Guessed) tt) (dpli f (cons (cons (neg p) 'Deduced) tt))]
       [_ #f])]
    [else (define unassgn (unassigned f trail^))
          (cond
            [(zero? (set-count unassgn)) trail^]
            [else (define p
                    (argmax (posneg-count f^) (set->list unassgn)))
                  (dpli f (cons (cons p 'Guessed) trail^))])]))