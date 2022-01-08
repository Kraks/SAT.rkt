#lang racket

;; Parser for DIMACS format

(provide parse parse-dimacs-file)

(define (all-but-last l)
  (cond [(empty? l) (error 'all-but-last "empty list")]
        [(empty? (rest l)) (set)]
        [else (set-add (all-but-last (rest l)) (first l))]))

(define (parse in-port)
  (set->list
   (sequence-fold
    (lambda (acc line)
      (cond [(non-empty-string? line)
             (cond [(char=? (string-ref line 0) #\c) acc]
                   [(char=? (string-ref line 0) #\p) acc]
                   [(char=? (string-ref line 0) #\%) acc]
                   [(char=? (string-ref line 0) #\0) acc]
                   [else (set-add acc
                                  (all-but-last
                                   ;(map aux
                                   (map string->number
                                        (string-split line #:trim? #t))))])]
            [else acc]))
    (set)
    (in-lines in-port))))

(define (parse-dimacs-file filename)
  (call-with-input-file filename parse))
