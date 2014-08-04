#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (avg x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (make-point (avg (x-point (start-segment segment))
                   (x-point (end-segment segment)))
              (avg (y-point (start-segment segment))
                   (y-point (end-segment segment)))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rect tl w h))
(define (width-rect rect))
(define (height-rect rect))

(define (peri-rect rect))
(define (area-rect rect))

;; Rect at (0,0) with width 4 and height 8
;; Area
;; Perimeter
