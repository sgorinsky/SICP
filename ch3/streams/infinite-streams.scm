#lang sicp
(define square (lambda (x) (* x x)))

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

(define sieve-of-eratosthenes (sieve (integers-starting-from 2)))
; creating integers by adding two streams in a "just-in-time" fashion
(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

; start w/ (0 promise) -> (0 (1 promise)) -> where promise adds first and second elements then recursively
;          builds stream by adding element just created w/ previous element created
(define fibs
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; build prime stream w/ stream-filter and prime? predicate
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

; scale-stream
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

; define powers of two by recursively multiplying successive elements in stream by 2
; (cons-stream 1 (stream-map (* 2 (cons-stream 1 (stream-map (* 2 (cons-stream 1 ...)
(define double (cons-stream 1 (scale-stream double 2)))

