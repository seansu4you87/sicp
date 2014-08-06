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
  (if (< (* (lower-bound y) (upper-bound y)) 0)
    (error "Division error (interval spans 0)" y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i))
     2))

(div-interval (make-interval 1 2)
              (make-interval -1 1))
