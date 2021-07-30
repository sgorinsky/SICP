#lang scheme

;; 3.1: Create make-accumulator which creates instances of accumulators

(define (make-accumulator initial)
  (let ((sum initial))
    (lambda (add)
      (set! sum (+ add sum))
      sum)))

;; separate accumulators A and B both manage different locally persistent state variables
(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25

(define B (make-accumulator 5))
(B 10) ; 15
(B 10) ; 25