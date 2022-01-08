#lang racket
;; A simple SAT solver using DPLL algorithm.
;; Author: Guannan Wei

#| Input format:
((set n_1 n_2 -n_3)) ...)
|#

(require "parser.rkt")
(require rackunit)

(provide mt-assgn assgn-update* get-all-vars elim-unit neg has-unit? has-pure? check-sat get-model solve solve/model)

(define (get-var var) (abs var))

(define (not-neg? var) (> var 0))

(define (neg var) (- 0 var))

(define (unit? cls) (equal? 1 (set-count cls)))

(define (has-unit? f)
  (define unit-vars (map set-first (filter unit? f)))
  (if (empty? unit-vars) #f
      (car unit-vars)))

(define (get-all-vars cls)
  (foldl set-union (set) cls))

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

(define (not-contains v) (λ (cls) (if (set-member? cls v) #f #t)))

(define (remove-var v) (λ (cls) (set-remove cls v)))
    
(define (elim-unit f uv)
  (define assgn (make-immutable-hash (if (not-neg? uv) `((,uv . #t)) `((,(neg uv) . #f)))))
  (define new-f (map (remove-var (neg uv)) (filter (not-contains uv) f)))
  (values new-f assgn))

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

(define (solve/model filename)
  (get-model (parse-dimacs-file filename)))