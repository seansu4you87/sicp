#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (powers-of 2 z))

(define (cdr z)
  (powers-of 3 z))

(define (powers-of x n)
  (if (> (remainder n x) 0)
      0
      (+ 1 (powers-of x (/ n x)))))

(car (cons 4 6))
(cdr (cons 4 6))

(car (cons 1 3))
(cdr (cons 1 3))

(car (cons 10 12))
(cdr (cons 10 12))
