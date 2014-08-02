#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (double proc)
  (lambda (x)
          (proc (proc x))))

(define (incr x) (+ x 1))

((double incr) 1)

;; 2x, 4x, 16x = 21
(((double (double double)) incr) 5)
