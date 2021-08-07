#lang scheme

(define v1 (make-vector 5 1))
(vector-set! v1 0 30)
(vector-ref v1 0)

(define (vector-map vector fn)
  (define (loop vect n)
    (if (> n (- (vector-length vector) 1)) vect
        (begin
          (vector-set! vect n (fn (vector-ref vector n)))
          (loop vect (+ n 1)))))
  (loop (make-vector (vector-length vector)) 0))

(vector-map v1 (lambda (x) (* x x)))