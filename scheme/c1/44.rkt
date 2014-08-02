#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1)) x)))))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  (lambda (x)
    (repeated smooth n) x))
