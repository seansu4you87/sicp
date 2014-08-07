#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (filt f l)
  (if (null? l)
      l
      (if (f (car l))
          (cons (car l) (filt f (cdr l)))
          (filt f (cdr l)))))

(define (same-parity . numbers)
  (let ((f (if (even? (car numbers))
               even?
               odd?)))
    (filt f numbers)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
