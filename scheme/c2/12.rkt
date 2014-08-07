#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* (/ p 100) c)))

(define (percent i)
  (* (/ (width i) (center i)) 100))
