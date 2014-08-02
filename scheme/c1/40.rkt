#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
          (/ (- (g (+ x dx)) (g x))
             dx)))

(define (newton-transform g)
  (lambda (x)
         (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; sqrt impl
(define (sqrt x)
  (newtons-method (lambda (y) (- (* y y) x))
                  1.0))

(define (cubic a b c)
  (lambda (x)
          (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 3 3 1) 1)
