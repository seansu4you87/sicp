#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i))
     2))

(width-interval (add-interval (make-interval 1 5)
                              (make-interval 2 8)))
(+ 2 3)

(width-interval (sub-interval (make-interval 1 5)
                              (make-interval 2 8)))
(+ 2 3)

(width-interval (mul-interval (make-interval 1 5)
                              (make-interval 2 8)))
(* 2 3)
