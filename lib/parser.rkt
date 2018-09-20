#lang racket

;; Parser for DIMACS format

(provide (all-defined-out))

(define (all-but-last l)
  (cond [(empty? l) (error 'all-but-last "empty list")]
        [(empty? (rest l)) empty]
        [else (cons (first l) (all-but-last (rest l)))]))

(define (parse in-port)
  (define aux
    (lambda (x)
      (let ([n (string->number x)])
        (if (< n 0)
            `(not ,(abs n))
            n))))
  
  (sequence-fold
   (lambda (acc line)
     (cond [(non-empty-string? line)
            (cond [(char=? (string-ref line 0) #\c) acc]
                  [(char=? (string-ref line 0) #\p) acc]
                  [(char=? (string-ref line 0) #\%) acc]
                  [(char=? (string-ref line 0) #\0) acc]
                  [else (append acc
                                `(,(all-but-last
                                    (map aux
                                         (string-split line #:trim? #t)))))])]
           [else acc]))
   '()
   (in-lines in-port)))

(define (parse-dimacs-file filename)
  (call-with-input-file filename parse))
