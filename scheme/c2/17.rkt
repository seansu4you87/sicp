#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34))
