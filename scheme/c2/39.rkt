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

(define (reverse seq)
  ; (fold-right (lambda (x acc)
  ;               (append acc (list x)))
  ;             nil
  ;             seq))
  (fold-left (lambda (acc x)
               (cons x acc))
             nil
             seq))

;; (1 (2 (3 nil)))
(reverse (list 1 2 3))
