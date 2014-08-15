#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node)
                             1))
                       tree)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
