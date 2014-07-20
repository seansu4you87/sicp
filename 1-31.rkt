#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

;; recursive product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;; iterative product
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; Uses

;; Factorial
(define (id x) x)
(define (incr x) (+ x 1))

(define (fac n)
  (product-iter id 1 incr n))

;; Pi
(define (pi accuracy)
  (define (term-pi n)
    (if (even? n)
        (/ (+ n 2) (+ n 3))
        (/ (+ n 3) (+ n 2))))
  (* 4.0 (product-iter term-pi 0 incr accuracy)))


(fac 5)
(fac 10)
(pi 10)
(pi 100)
(pi 1000)
(pi 10000)

