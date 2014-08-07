#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (square x)
  (* x x))

(define (square-list-a items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map (lambda (x) (square x)) items))

(square-list-b (list 1 2 3 4))
