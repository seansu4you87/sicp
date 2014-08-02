#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (acc-filter combiner null-term term a next b filter)
  (if (> a b)
      null-term
      (combiner (if (filter a) (term a) null-term)
                (acc-filter combiner null-term term (next a) next b filter))))

;; Example 1
; (define (sum-sq-primes a b)
;   (acc-filter + 0 square a incr b prime?))

;; Example 2

(define (gcd a b)
  (cond ((< a b) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (relative-prime? a b)
  (= (gcd a b) 1))

(define (id x) x)
(define (incr x) (+ x 1))

(define (product-relative-primes n)
  (define (filter x)
    (relative-prime? x n))
  (acc-filter * 1 id 1 incr n filter))

(product-relative-primes 10)
