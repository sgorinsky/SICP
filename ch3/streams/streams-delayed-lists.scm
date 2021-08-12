#lang sicp

;; stream primitives
(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; what delay and force look like -- a lambda expression that is waiting to be evaluated
;(define (delay proc)
;  (lambda () proc))
;
;(define (force delayed-proc)
;  (delayed-proc))

; ex: define pair of one and promise of recursive call to itself to fulfill
(define ones
  (cons-stream 1 ones))

;; Analog procs for lists
(define (stream-filter pred? stream)
  (cond ((stream-null? stream) stream)
        ((pred? (stream-car stream))
         (cons-stream (stream-car stream) (stream-filter pred? (stream-cdr stream))))
        (else (stream-filter pred? (stream-cdr stream)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; processes stream as list
(define (show-stream stream)
  (if (stream-null? stream)
      stream
      (cons (stream-car stream) (show-stream (stream-cdr stream)))))

; ex: integers from 1 to n
(define (stream-range from to)
  (if (= from to)
      the-empty-stream
      (cons-stream from (stream-range (+ from 1) to))))

;; 3.50: Complete generalized stream-map proc
(define (stream-map-general proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-general (cons proc (map stream-cdr argstreams))))))

(show-stream (stream-map-general + (stream-range 1 10) (stream-range 1 10)))

      