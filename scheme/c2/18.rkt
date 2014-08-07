#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (reverse l)
  (define (reverse-iter l acc)
    (if (null? l)
        acc
        (reverse-iter (cdr l) (cons (car l) acc))))
  (reverse-iter l nil))

(reverse (list 1 4 9 16 25))
