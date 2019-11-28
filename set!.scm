#lang scheme

(define foo
  (lambda (x) (* x x)))

(define a 4)

(define (bar)
  (set! a (foo a)) ;; alters a in the global environment
  a) 

(define b 5)

(define (buoy)
  (define b 5) b)

(define (boo)
  (set! b (+ b 1)) b)