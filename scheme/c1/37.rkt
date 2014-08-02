#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (cont-frac index-n index-d k)
  (define (recurse i)
    (if (= i k)
        (/ (index-n k) (index-d k))
        (/ (index-n 1)
           (+ (index-d 1)
              (cont-frac index-n index-d (- k 1))))))
  (recurse 1))

(define k 11)
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

(define (cont-frac-iter index-n index-d k)
  (define (iter i memo)
    (if (= i k)
        memo
        (iter (+ i 1) (/ (index-n i) (+ (index-d i) memo)))))
  (iter 1 (/ (index-n k) (index-d k))))

(define j 11)
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                j)
