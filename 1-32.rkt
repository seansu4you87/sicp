#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

;; accumulate
(define (accumulate combiner null-term term a next b)
  (if (> a b)
      null-term
      (combiner (term a)
                (accumulate combiner null-term term (next a) next b))))

;; accumulate-iter

;; helpers for sum and product
(define (id x) x)
(define (incr x) (+ x 1))

;; sum in accumulate
(define (sum a b)
  (accumulate + 0 id a incr b))

;; product in accumulate
(define (product a b)
  (accumulate * 1 id a incr b))

;; tests
(sum 1 5)
(sum 1 10)
(product 1 5)
(product 1 10)
