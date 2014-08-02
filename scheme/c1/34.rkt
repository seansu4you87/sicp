#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

; using let as syntatic sugar for lambdas

(define (f g)
  (g 2))

(define (square x) (* x x))

(f square)
; 4

(f (lambda (z) (* z (+ z 1))))
; 6

(f f)
; (f 2) => (2 2)
; this breaks because we give a number 2, an argument of 2, and 2 is not a
; procedure
