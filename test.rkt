#lang racket

;; Tests

(require rackunit)
(require "lib/dpll.rkt")
(require "lib/parser.rkt")

(check-equal? (propagate 1 #t '((1)))
              '())
(check-equal? (propagate 1 #t '((1 2)))
              '())
(check-equal? (propagate 1 #t '((3 1 2)))
              '())
(check-equal? (propagate 1 #f '((2 4 1 3) (1 2 (not 3))))
              '((2 4 3) (2 (not 3))))
(check-equal? (propagate 2 #f '(((not 2))))
              '())
(check-equal? (propagate 2 #f '((1 2 (not 2)) (2 3 4)))
              '((3 4)))
(check-equal? (propagate 2 #t '(((not 2))))
              '(()))
(check-equal? (propagate 2 #t '((1 2 3) (2) ((not 2))))
              '(()))

(check-equal? (eliminate-unit-clause '((1 2 3)))
              '((1 2 3)))
(check-equal? (eliminate-unit-clause '((1 2 3) (2) ((not 2))))
             '(()))
(check-equal? (eliminate-unit-clause '((1 2 3) (4 5 6) (7)))
              '((1 2 3) (4 5 6)))

(check-true (appear-only-once? 1 eq? '(1 2 3)))
(check-true (appear-only-once? 3 eq? '(1 2 3)))
(check-false (appear-only-once? 1 eq? '(3 2 1 1)))

(check-equal? (get-pure-variables '(((not 2))))
              '((not 2)))
(check-equal? (get-pure-variables '(((not 2) (not 3)) (3)))
              '((not 2)))
(check-equal? (list->set (get-pure-variables '((1 2 3) (4 5 6))))
              (set 4 6 2 3 5 1))
(check-equal? (list->set (get-pure-variables '((1 2) ((not 2) 3))))
              (set 3 1))

(check-equal? (eliminate-pure-variables '((1 2 3)))
              '())
(check-equal? (eliminate-pure-variables '((1 2) ((not 2))))
              '(((not 2))))

(check-equal? (get-all-variables '((1 2 3) (4 5 6)))
              (set 1 2 3 4 5 6))
(check-equal? (get-all-variables '((1 (not 2) 3) (1 (not 2) 3)))
              (set 1 '(not 2) 3))

(check-true (dpll '((1 2 3))))
(check-true (dpll '((1 2) ((not 1) (not 2)))))
(check-true (dpll '((1 (not 5) 4)
                    ((not 1) 5 3 4)
                    ((not 3) (not 4)))))

(check-false (dpll '(())))
(check-false (dpll '(() (1))))
(check-false (dpll '((1 2 3) (2 3 4) ())))
(check-false (dpll '((1) ((not 1)))))

; 2 queen problem
(check-false (dpll '((1 2)
                     ((not 1) (not 2))
                     (3 4)
                     ((not 3) (not 4))
                     (1 3)
                     ((not 1) (not 3))
                     (2 4)
                     ((not 2) (not 4))
                     ((not 2) (not 3))
                     ((not 1) (not 4)))))

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
              '((1 (not 5) 4)
                ((not 1) 5 3 4)
                ((not 3) (not 4))))
(check-true (dpll parsed-cnf1))

(check-false (solve "tests/2queens.cnf"))
(check-false (solve "tests/3queens.cnf"))
(check-true (solve "tests/4queens.cnf"))
(check-true (solve "tests/5queens.cnf"))
(check-true (solve "tests/8queens.cnf"))
(check-true (solve "tests/12queens.cnf"))
