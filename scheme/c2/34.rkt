#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coefficient higher-terms)
                (+ this-coefficient (* x higher-terms)))
              0
              coefficient-seq))

;; f(x) = 1 + 3x + 0x^2 + 5x^3 + 0x^4 + x^5
;; f(2) = 79
(horner-eval 2 (list 1 3 0 5 0 1))
