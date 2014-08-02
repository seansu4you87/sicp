#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (pascal n)
  (define (triangle n)
    (if (= n 0)
        0
        (+ n (triangle (- n 1)))))

  (define (left-edge n)
    (triangle n))

  (define (right-edge n)
    (- (triangle n) 1))

  (define (first-lte x func start)
    (cond ((< x (func start)) (func (- start 1)))
          ((= x (func start)) (func start))
          ((> x (func start)) (first-lte x func (+ start 1)))))

  (define (first-left-edge-lte x)
    (first-lte x left-edge 0))

  (define (second-left-edge-lt x)
    (first-left-edge-lte (- (first-left-edge-lte x) 1)))

  (define (first-right-edge-lte x)
    (first-lte x right-edge 0))

  (define (second-right-edge-lt x)
    (first-right-edge-lte (- (first-right-edge-lte x) 1)))

  (define (left-edge? x)
    (= (first-left-edge-lte x) x))

  (define (right-edge? x)
    (= (first-right-edge-lte x) x))

  (define (delta x)
    (- x (first-left-edge-lte x)))

  (define (left-parent x)
    (+ (second-left-edge-lt x) (- (delta x) 1)))

  (define (right-parent x)
    (+ (second-left-edge-lt x) (delta x)))

  (if (or (left-edge? n) (right-edge? n))
      1
      (+ (pascal (left-parent n)) (pascal (right-parent n)))))

; (pascal 0)

; (pascal 1)
; (pascal 2)

; (pascal 3)
; (pascal 4)
; (pascal 5)

; (pascal 6)
; (pascal 7)
; (pascal 8)
; (pascal 9)

(pascal 10)
(pascal 11)
(pascal 12)
(pascal 13)
(pascal 14)
