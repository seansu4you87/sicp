#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (square x)
  (* x x))

(define (expmod b n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (square (expmod b (/ n 2) m)) m))
        (else (remainder (* b (expmod b (- n 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(fast-prime? 561 700)

