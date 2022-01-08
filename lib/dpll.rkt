#lang racket
;; A simple SAT solver using DPLL algorithm.
;; Author: Guannan Wei

#| Input format:
((set n_1 n_2 -n_3)) ...)
|#

(require "parser.rkt")
(require "utils.rkt")

(provide dpll)

(define (assgn-update* a1 a2)
  (foldl (λ (kv m) (hash-set m (car kv) (cdr kv))) a1 (hash->list a2)))

(define (pick-var f) (set-first (car f)))

;; dpll : S-exp-formula hash -> hash-map|boolean
(define (dpll f assgn)
  (cond [(memf (compose zero? set-count) f) #f]
        [(zero? (length f)) assgn]
        [(has-unit? f)
         => (λ (uv)
              (define-values (new-f new-assgn) (elim-unit f uv))
              (dpll new-f (assgn-update* assgn new-assgn)))]
        [(has-pure? f)
         => (λ (pv) (dpll (cons (set pv) f) assgn))]
        [else
         (define v (pick-var f))
         (cond [(dpll (cons (set v) f) assgn)
                => (λ (assgn) assgn)]
               [else (dpll (cons (set (neg v)) f) assgn)])]))
