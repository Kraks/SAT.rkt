#lang racket
;; A simple SAT solver using DPLL algorithm.
;; Author: Guannan Wei

#| Input format:
((n_1 n_2 (not n_3)) ...)
|#

(require "parser.rkt")
(require rackunit)

(provide (all-defined-out))

(define (get-var var)
  (match var
    [`(not ,var^) var^]
    [else var]))

(define (not-neg? var)
  (match var
    [`(not ,var^) #f]
    [else #t]))

(define (neg var)
  (match var
    [`(not ,var^) var^]
    [_ `(not ,var)]))

(define (unit? cls) (eq? 1 (length cls)))

(define (has-unit? f)
  (define unit-vars (map car (filter unit? f)))
  (if (empty? unit-vars) #f
      (car unit-vars)))

(define (get-all-vars cls)
  (apply set (foldl append '() cls)))

(define (has-pure? f)
  (define pure-vars
    (foldl append '()
           (filter (λ (vs) (eq? 1 (length vs)))
                   (group-by get-var (set->list (get-all-vars f))))))
  (if (empty? pure-vars) #f
      (car pure-vars)))

(define (assgn-update* a1 a2)
  (foldl (λ (kv m) (hash-set m (car kv) (cdr kv))) a1 (hash->list a2)))

(define mt-assgn (make-immutable-hash))

(define (not-contains v) (λ (cls) (if (member v cls) #f #t)))

(define remove-var (curry remove))
    
(define (elim-unit f uv)
  (define assgn (make-immutable-hash (if (not-neg? uv) `((,uv . #t)) `((,uv . #f)))))
  (define new-f (map (remove-var (neg uv)) (filter (not-contains uv) f)))
  (values new-f assgn))

(define (pick-var f) (car (car f)))

;; dpll : S-exp-formula hash -> hash-map|boolean
(define (dpll f assgn)
  (cond [(memf (compose zero? length) f) #f]
        [(zero? (length f)) assgn]
        [(has-unit? f)
         => (λ (uv)
              (define-values (new-f new-assgn) (elim-unit f uv))
              (dpll new-f (assgn-update* assgn new-assgn)))]
        [(has-pure? f)
         => (λ (pv) (dpll (cons `(,pv) f) assgn))]
        [else
         (define v (pick-var f))
         (cond [(dpll (cons `(,v) f) assgn)
                => (λ (assgn) assgn)]
               [else (dpll (cons `((not ,v)) f) assgn)])]))

;; check-sat : S-exp-formula -> boolean
(define (check-sat f)
  (define result (dpll f mt-assgn))
  (if result #t #f))

;; get-model : S-exp-formula -> hash-map|boolean
(define (get-model f)
  (define result (dpll f mt-assgn))
  (if result result #f))

;; solve : string -> boolean
(define (solve filename)
  (check-sat (parse-dimacs-file filename)))
