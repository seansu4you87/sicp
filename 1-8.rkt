#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
  (avg3 guess guess (/ x (square guess))))

(define (avg3 x y z)
  (/ (+ x y z) 3))

(cube-root 1)
(cube-root 8)
(cube-root 27)
