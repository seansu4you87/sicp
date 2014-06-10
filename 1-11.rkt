#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

; Recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 2))))

(define (f-iter a b c n)
  (if (= n 0)
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))

(f 1)
(f2 1)
(f 2)
(f2 2)
(f 3)
(f2 3)
(f 4)
(f2 4)
(f 5)
(f2 5)
(f 6)
(f2 6)
