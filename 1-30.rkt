#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (incr x) (+ x 1))
(define (id x) x)

(sum id 1 incr 10)
(sum id 1 incr 100)
