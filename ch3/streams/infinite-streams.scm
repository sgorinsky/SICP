#lang sicp

;; stream primitives
(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; Stream functors
(define (stream-filter pred? stream)
  (cond ((stream-null? stream) stream)
        ((pred? (stream-car stream))
         (cons-stream (stream-car stream) (stream-filter pred? (stream-cdr stream))))
        (else (stream-filter pred? (stream-cdr stream)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? (stream-car streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; infinite streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1)) ; redefining integers below


(define (divisible? x y) (= (modulo x y) 0))

; sieve of eratosthenes: filters rest of stream with current prime and leaves candidates in stream until we
;                        reach the cand, then we know it's prime
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

; creating integers by adding two streams
(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))
