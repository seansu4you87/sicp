#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define x (list 1 2 3)) ; '(1 2 3)
(define y (list 4 5 6)) ; '(4 5 6)

(append x y) ; '(1 2 3 (4 5 6))
(cons x y)   ; '((1 2 3) 4 5 6)
(list x y)   ; '((1 2 3) (4 5 6))
