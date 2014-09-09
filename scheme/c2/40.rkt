#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define nil '())

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (enumerate-interval start end)
  (define (iter curr end seq)
    (if (> curr end)
        seq
        (iter (+ curr 1)
              end
              (append seq (list curr)))))
  (iter start end nil))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 6)
