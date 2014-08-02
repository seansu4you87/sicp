#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (double x)
  (+ x x))

(define (halve x)
  (if (even? x)
      (/ x 2)
      "wtf"))

(define (* a b)
  (define (iter a b p)
    (cond ((= b 0) p)
          ((even? b) (iter (double a) (halve b) p))
          (else (iter a (- b 1) (+ p a)))))

  (iter a b 0))

(* 9 9)
(* 8 8)
(* 7 7)
(* 6 6)
(* 5 5)
(* 4 4)
(* 2 3)
