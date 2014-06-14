#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (double x)
  (+ x x))

(define (halve x)
  (if (even? x)
      (/ x 2)
      "wtf"))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

(* 9 9)
(* 8 8)
(* 7 7)
(* 6 6)
(* 5 5)
(* 4 4)
(* 2 3)
