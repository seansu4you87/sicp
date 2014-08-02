#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (sqrt-iter guess x)
  (if (good-enough-2? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough-2? guess x)
  (< (/ (abs (- guess (improve guess x))) guess) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 16)
(sqrt 625)
(sqrt 0.001)
(sqrt 625000000000000000000000000)
