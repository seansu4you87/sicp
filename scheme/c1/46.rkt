#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (iterative-improve check improve)
  (define (iterate guess)
    (display guess)
    (newline)
    (if (check guess)
        guess
        (iterate (improve guess))))
  (lambda (x)
    (iterate x)))

(define (sqrt n)
  (define (check x)
    (< (abs (- (* x x) n)) 0.00001))
  (define (improve x)
    (/ (+ x (/ n x)) 2))
  ((iterative-improve check improve) 1.0))

(define (fixed-point f guess)
  (define (check x)
    (< (abs (- x (f x))) 0.00001))
  ((iterative-improve check f) guess))

; (define (sqrt x)
;   (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
;                1.0))
(sqrt 4)

; (define phi
;   (fixed-point (lambda (x) (+ 1 (/ 1 x)))
;                1.0))

; (display phi)
; (newline)
