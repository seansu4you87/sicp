#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (fringe tree)
  (define (leaf? branch)
    (not (pair? branch)))

  (cond ((null? tree) (list))
        ((leaf? tree) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
