#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
