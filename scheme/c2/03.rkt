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

; impl 1 using points
; (define (make-rect tl w h)
;   (cons tl (make-point (+ (x-point tl) w) (+ (y-point tl) h))))

; (define (width-rect rect)
;   (- (x-point (cdr rect)) (x-point (car rect))))

; (define (height-rect rect)
;   (- (y-point (cdr rect)) (y-point (car rect))))

; impl 1 using point and pair of (weight, height)

(define (make-rect tl w h)
  (cons tl (cons w h)))

(define (width-rect rect)
  (car (cdr rect)))

(define (height-rect rect)
  (cdr (cdr rect)))

(define (peri-rect rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

;; Rect at (0,0) with width 4 and height 8
(define rect (make-rect (make-point 0 0) 4 8))
(display "Perimeter is: ")
(peri-rect rect)
(display "Area is: ")
(area-rect rect)
