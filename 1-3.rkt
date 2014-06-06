#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (min a b c)
  (cond ((and (<= a b) (<= a c)) a)
        ((and (<= b a) (<= b c)) b)
        ((and (<= c a) (<= c b)) c)))

(define (square a)
  (* a a))

(define (hypotenuse a b)
  (+ (square a) (square b)))

(define (max-hypotenuse a b c)
  (cond ((= (min a b c) a) (hypotenuse b c))
        ((= (min a b c) b) (hypotenuse a c))
        ((= (min a b c) c) (hypotenuse a b))))

(hypotenuse 2 3)
(max-hypotenuse 1 2 3)
