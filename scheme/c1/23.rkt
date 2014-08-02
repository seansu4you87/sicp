#! /Applications/Racket v6.0.1/bin/racket
#lang planet neil/sicp

(define (divides? n d)
  (= (remainder n d) 0))
(define (smallest-divisor n)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n d)
    (cond ((> (* d d) n) n)
          ((divides? n d) d)
          (else (find-divisor n (next d)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))
