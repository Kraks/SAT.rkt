#lang racket

(require "dp.rkt")
(require "dpll.rkt")
(require "dpli.rkt")
(require "dplb.rkt")
(require "parser.rkt")

(provide (all-defined-out))

;; check-sat : S-exp-formula -> boolean
(define (check-sat f [algo 'dpll])
  (define result
    (match algo
      ['dp (dp f)]
      ['dpll (dpll f (make-immutable-hash))]
      ['dpli (dpli f '())]
      ['dplb (dplb f '())]
      [_ (error "unsupported algorithm!")]))
  (if result #t #f))

;; get-model : S-exp-formula -> hash-map|boolean|list
(define (get-model f [algo 'dpll])
  (match algo
    ['dp (error "the DP implementation cannot output model!")]
    ['dpll (define result (dpll f (make-immutable-hash)))
           (if result result #f)]
    ['dpli (define result (dpli f '()))
           (if result (map car result) #f)]
    ['dplb (define result (dplb f '()))
           (if result (map car result) #f)]
    [_ (error "unsupported algorithm!")]))

;; solve : string -> boolean
(define (solve filename [algo 'dpll])
  (check-sat (parse-dimacs-file filename) algo))

(define (solve/model filename [algo 'dpll])
  (get-model (parse-dimacs-file filename) algo))
