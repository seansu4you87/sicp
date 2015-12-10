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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

;; adjoin-position
;; empty-board
;; safe?

