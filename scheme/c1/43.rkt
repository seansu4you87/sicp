#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1)) x)))))

(define (square x) (* x x))

((repeated square 2) 5)
