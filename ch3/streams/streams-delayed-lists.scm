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

(define (display-stream s)
  (stream-for-each display-line s))

; processes stream as list
(define (show-stream stream)
  (if (stream-null? stream)
      stream
      (cons (stream-car stream) (show-stream (stream-cdr stream)))))

; ex: range of integers from 1 to n exclusive end
(define (stream-range from to)
  (if (= from to)
      the-empty-stream
      (cons-stream from (stream-range (+ from 1) to))))

; stream enumerate interval inclusive end
(define (stream-enumerate-interval from to)
  (if (> from to)
      the-empty-stream
      (cons-stream from (stream-enumerate-interval (+ from 1) to))))

; memo-proc for memoizing last entries seen in stream
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))

;; 3.50: Complete generalized stream-map proc
(define (stream-map-general proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-general (cons proc (map stream-cdr argstreams))))))

(show-stream (stream-map-general + (stream-range 1 10) (stream-range 1 10)))

;; 3.51: After defining show below, what does interpreter print for following procs?
(define (display-line element)
  (display element)
  (newline))

(define (show x) (display-line x) x)

; procs to evaluate
(define x
  (stream-map show (stream-enumerate-interval 0 10))) ; 0 b/c only evaluates first element

;; evaluates (display-line x) in show proc when stream-cdr forces evaluation of promise
(newline)
(stream-ref x 5) ; 1 2 3 4 5 5

(newline)
(stream-ref x 7) ; 6 7 7

;; 3.52: What is output of procs? Differences of results w/ and w/o memoization?
(define sum 0)

(define (accum x)
  (set! sum (+ x sum)) sum)

(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z
  (stream-filter
   (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(display-stream z)

; W/o memoization, we'd have to calculate results again thus skewing them.

