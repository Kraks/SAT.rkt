#lang racket

;; Tests

(require rackunit)
(require "lib/dpll.rkt")
(require "lib/parser.rkt")

(check-true (check-sat `(,(set 1 2 3))))
(check-true (check-sat `(,(set 1 2) ,(set -1 -2))))
(check-true (check-sat `(,(set 1 -5 4)
                         ,(set -1 5 3 4)
                         ,(set -3 -4))))

(check-false (check-sat `(,(set))))
(check-false (check-sat `(,(set) ,(set 1))))
(check-false (check-sat `(,(set 1 2 3) ,(set 2 3 4) ,(set))))
(check-false (check-sat `(,(set 1) ,(set -1))))

(check-false (check-sat `(,(set 1 2)
                          ,(set -1 -2)
                          ,(set 3 4)
                          ,(set -3 -4)
                          ,(set 1 3)
                          ,(set -1 -3)
                          ,(set 2 4)
                          ,(set -2 -4)
                          ,(set -2 -3)
                          ,(set -1 -4))))

(define cnf1 (open-input-string
              (string-append
               "c\n"
               "c start with comments\n"
               "c\n"
               "p cnf 5 3\n"
               "1 -5 4 0\n"
               "-1 5 3 4 0\n"
               "-3 -4 0")))
(define parsed-cnf1 (parse cnf1))
(check-equal? parsed-cnf1
              (set->list (set (set 1 -5 4)
                              (set -1 5 3 4)
                              (set -3 -4))))
(check-true (check-sat parsed-cnf1))

(check-false (solve "tests/2queens.cnf"))
(check-false (solve "tests/3queens.cnf"))
(check-true (solve "tests/4queens.cnf"))
(check-true (solve "tests/5queens.cnf"))
(check-true (solve "tests/8queens.cnf"))
(check-true (solve "tests/12queens.cnf"))

(define (get-all-cnf-files dir)
  (map (λ (p) (string-append dir "/" (path->string p)))
       (filter (λ (p) (equal? #"cnf" (filename-extension p))) (directory-list dir))))

(time (for ([f (get-all-cnf-files "tests/uuf50-218")])
  (displayln f)
  (check-false (solve f))))

(time (for ([f (get-all-cnf-files "tests/uf50-218")])
  (displayln f)
  (check-true (solve f))))