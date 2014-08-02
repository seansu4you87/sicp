#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

; N 1
; D 1 2 1 1 4 1 1 6 1 1  8  1  1  10 1
; k 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

(define (cont-frac index-n index-d k)
  (define (recurse i)
    (display i)
    (display " ")
    (display (index-d i))
    (newline)
    (if (= i k)
        (/ (index-n i) (index-d i))
        (/ (index-n 1)
           (+ (index-d i)
              (recurse (+ i 1))))))
  (recurse 1))

(define (index-d k)
  (if (= k 1)
      1
      (if (= (remainder (- k 2) 3) 0)
          (+ 2 (* 2 (/ (- k 2) 3)))
          1)))

(+ 2 (cont-frac (lambda (k) 1.0)
                index-d
                1000))
