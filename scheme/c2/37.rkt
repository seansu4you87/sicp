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

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row)
         (dot-product m-row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           ; (accumulate (lambda (n-col result-row)
           ;               (cons (dot-product m-row n-col) result-row))
           ;             nil
           ;             cols))
           (matrix-*-vector cols m-row))
           m)))

(define vec (list 1 2 3 4))

(define mat (list (list 1 2 3 4)
                  (list 4 5 6 6)
                  (list 6 7 8 9)))

(define sq-mat (list (list 1 2)
                     (list 3 4)))

(dot-product vec vec)
(matrix-*-vector mat vec)
(transpose sq-mat)
(matrix-*-matrix sq-mat sq-mat)
