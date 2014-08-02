#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (divides? n d)
  (= (remainder n d) 0))

(define (smallest-divisor n)
  (define (find-divisor n d)
    (cond ((> (* d d) n) n)
          ((divides? n d) d)
          (else (find-divisor n (+ d 1)))))
  (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
