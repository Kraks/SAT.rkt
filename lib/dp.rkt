#lang racket

;; A simple SAT solver using the DP algorithm.
;; It is a Racket implementation of the `dp` function
;; in Handbook of Practical Logic and Automated Reasoning.
;; Currently, this implementation is extremely inefficient
;; and unable to solve 4 queens problem, due to clause number
;; blowup induced by the resolution rule.

(require "parser.rkt")
(require "dpll.rkt")

(provide dp)

(define (resolve-on v f)
  (define-values (pos-cs not-pos) (partition (λ (c) (set-member? c v)) f))
  (define-values (neg-cs others) (partition (λ (c) (set-member? c (neg v))) not-pos))
  (append others
          (for*/list ([pc (map
                           (λ (c) (filter (λ (l) (not (equal? v l))) (set->list c)))
                           pos-cs)]
                      [nc (map
                           (λ (c) (filter (λ (l) (not (equal? (neg v) l))) (set->list c)))
                           neg-cs)])
            (list->set (append pc nc)))))

(define (pick-pivot f)
  (define vs (set->list (list->set (set-map (get-all-vars f) abs))))
  (define (result-length v)
    (define pos-len (length (filter (λ (c) (set-member? c v)) f)))
    (define neg-len (length (filter (λ (c) (set-member? c (neg v))) f)))
    (- (- (* pos-len neg-len) pos-len) neg-len))
  (argmin result-length vs))

(define (tautologous? c)
  (define pos (list->set (set-map c abs)))
  (not (equal? (set-count pos) (set-count c))))

(define (dp f assgn)
  (cond [(memf (compose zero? set-count) f) #f]
        [(zero? (length f)) assgn]
        [(has-unit? f)
         => (λ (uv)
              (define-values (new-f new-assgn) (elim-unit f uv))
              (dp new-f (assgn-update* assgn new-assgn)))]
        [(has-pure? f)
         => (λ (pv) (dp (cons `(,pv) f) assgn))]
        [else
         (define v (pick-pivot f))
         (dp (filter-not tautologous? (resolve-on v f)) assgn)]))