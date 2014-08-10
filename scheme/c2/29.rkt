#! /Applications/Racket v6.0.1/bin/racket
#lang racket/base

;; (left right) = (cons left (cons right nil))
(define (make-mobile left right)
  (list left right))

;; (10 structure) -> (cons 10 (cons structure nil))
;; structure is either a mobile, or a weight
(define (make-branch length structure)
  (list length structure))

;; left-branch
(define left-branch car)

;; right-branch
(define (right-branch mobile)
  (car (cdr mobile))

;; branch-length
(define branch-length car)

;; branch-structure
(define (branch-structure branch)
  (car (cdr branch))

;; weights (total-weight proc in SICP)
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (mobile-weight structure)
        structure)))

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; balanced
;; length * weight and if each submobile hanging off is balanced
(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (mobile-balanced? mobile)
          #t)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left)
            (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))

;; would barely need to change
