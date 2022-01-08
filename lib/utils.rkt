#lang racket

(provide get-all-vars elim-unit neg has-unit? has-pure?)

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

(define (not-contains v) (λ (cls) (if (set-member? cls v) #f #t)))

(define (remove-var v) (λ (cls) (set-remove cls v)))
    
(define (elim-unit f uv)
  (define assgn (make-immutable-hash (if (not-neg? uv) `((,uv . #t)) `((,(neg uv) . #f)))))
  (define new-f (map (remove-var (neg uv)) (filter (not-contains uv) f)))
  (values new-f assgn))