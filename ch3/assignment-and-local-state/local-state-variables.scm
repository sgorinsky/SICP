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

;; 3.2: Create make-monitored to keep track of how many times instance of obj was called
(define (make-monitored fn)
  (let ((calls 0))
    (lambda (message)
      (if (eq? message 'how-many-calls)
          calls
          ((lambda (x)
            (set! calls (+ calls 1))
            (fn x)) message)
          ))))
          

(define sq-mon (make-monitored (lambda (x) (* x x))))
(sq-mon 5)
(sq-mon 'how-many-calls)
(sq-mon 5)
(sq-mon 'how-many-calls)
  