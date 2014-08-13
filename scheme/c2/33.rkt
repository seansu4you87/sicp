#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3 4 5) (list 6 7 8 9 10))
(append (list 1 2 3 4 5) 6)

(define (length sequence)
  (accumulate (lambda (x y)
                (+ y 1))
              0
              sequence))

(length nil)
(length (list 1))
(length (list 1 2 3 4 5))
