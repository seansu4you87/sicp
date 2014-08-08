#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (deep-reverse l)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (if (pair? (car l))
                                (deep-reverse (car l))
                                (car l))
                            acc))))
  (iter l '()))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)
