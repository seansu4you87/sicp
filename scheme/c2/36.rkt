#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
