#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (fold-right op initial (cdr seq)))))

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
    (iter initial seq))

(fold-right / 1 (list 1 2 3)) ; 3/2/1/1 = 3/2
(fold-left  / 1 (list 1 2 3)) ; 1/3/2/1 = 1/6

(fold-right list nil (list 1 2 3)) ; (3 (2 (1 ())))
(fold-left  list nil (list 1 2 3)) ; (((() 3) 2) 1)

;; for fold-right and fold-left to be equivalent, the operation must be commutative
