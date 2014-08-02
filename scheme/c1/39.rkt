#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (cont-frac index-n index-d k)
  (define (recurse i)
    (display i)
    (display " ")
    (display (index-n i))
    (display " ")
    (display (index-d i))
    (newline)
    (if (= i k)
        (/ (index-n k) (index-d k))
        (/ (index-n i)
           (+ (index-d i)
              (recurse (+ i 1))))))
  (recurse 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(tan-cf 1.0 5)
