#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point cos 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.0))
; (sqrt 4)

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(display phi)
(newline)
