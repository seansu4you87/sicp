#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (incr2 x)
  (+ x 2))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))

  (* (/ h 3.0)
     (+ (yk 0) 
        (* 4 (sum yk 1 incr2 (- n 1)))
        (* 2 (sum yk 2 incr2 (- n 2)))
        (yk n))))

(simpson cube 0 1 10)
(simpson cube 0 1 100)
(simpson cube 0 1 1000)
