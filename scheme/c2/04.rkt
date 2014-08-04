#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 1 2))
(car z)
(cdr z)
