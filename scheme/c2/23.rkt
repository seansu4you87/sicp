#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (f-r-each f l)
  (map f l)
  (#t))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
