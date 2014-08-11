#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree)))
              (cons (tree-map f (cdr tree))))))

(define (square x) (* x x))

(define (square-tree tree)
  (tree-map square tree))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

; (square-tree x)
; (square-tree-map x)
