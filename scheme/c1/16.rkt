#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

(define (square x)
  (* x x))

(define (fast-expt b n)
  (define (fast-expt-iter b n power-counter product-counter)
    (cond ((= power-counter n) product-counter)
          ((> (* 2 power-counter) n) (fast-expt-iter b n (+ power-counter 1) (* product-counter b)))
          (else (fast-expt-iter b n (* 2 power-counter) (square product-counter)))))
  (fast-expt-iter b n 1 b))

(define (fast-expt-state b n)
  (define (fast-expt-state-iter b n state)
    ; if n = 0: state
    ; if state = 1: n = n - 1, state = b, iterate
    ; if n is even: n = n / 2, state = state * (iterate (n/2))
    ; else (odd):   n = n - 1, state = b * state
    (cond ((= n 0) state)
          ((= state 1) (fast-expt-state-iter b (- n 1) b))
          ((even? n) (fast-expt-state-iter (square b) (/ n 2) state))
          (else (fast-expt-state-iter b (- n 1) (* b state)))))
  (fast-expt-state-iter b n 1))

(fast-expt 3 10)
(fast-expt-state 3 10)
