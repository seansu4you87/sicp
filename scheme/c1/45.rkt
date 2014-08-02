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

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x))
       2)))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1)) x)))))

(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root n x)
  (define root
    (lambda (y)
      (/ x (expt y (- n 1)))))
  (fixed-point (repeated (average-damp root)
                         (floor (log2 n)))
               1.0))

;; 2  1
;; 3  2
;; 4  2
;; 5  2
;; 6  2
;; 7  2
;; 8  3
;; 9  3
;; 10 3
;; 11 3
;; 12 3
;; 13 3
;; 14 3
;; 15 3
;; 16 4
;; 17 3
; (nth-root 16 65536)
(nth-root 8 256)
