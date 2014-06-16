#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
(fib 11)
(fib 12)
(fib 13)
(fib 14)
(fib 15)
(fib 16)
(fib 17)
(fib 18)
(fib 19)
(fib 20)
