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

; build prime stream w/ stream-filter and prime? predicate which checks existing list of primes stream
;     these two procs are mutually referential
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

;; 3.53: Describe what happens in proc below
(define s (cons-stream 1 (add-streams s s)))

; (1 (promise of map + (1 promise) (1 promise)))
; (1 (2 (promise of map + (2 promise) (2 promise))))...
; s returns a stream of powers of two...
; ...and without fail, (stream-ref s 4) returns 16


;; 3.54: Implement analog to add-stream, mul-streams, and implement factorial
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorial
  (cons-stream 1 (mul-streams factorial (integers-starting-from 2))))

;; 3.55: Partial Streams, where each element is sum of all elements up until and including that element
(define (partial-sums stream)
  (define (iter sum strm)
    (if (stream-null? strm)
        the-empty-stream
        (let ((next-sum (+ (stream-car strm) sum)))
          (cons-stream next-sum (iter next-sum (stream-cdr strm))))))
  (iter 0 stream))

; (stream-ref (partial-sums ones) 2) ; 3 (1 2 3)
; (stream-ref (partial-sums integers) 2) ; 6 (1 3 6)

;; 3.56: Enumerate in ascending order all positive integers with no prime-factors other than 2, 3, or 5
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else (cons-stream
                        s1car
                        (merge (stream-cdr s1)
                               (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3) (scale-stream S 5)))))

;; 3.57: Recall definition of memo-proc. How many calls to delay are cut out when memoizing force calls in
;;       fibonacci

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))

; recall def of fibs:
;(define fibs
;  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; For each new element in fibonacci, f(n) = f(n-1) + f(n-2)
;   W/o memoization, each of the values creating f(n) in map have to be recomputed and then
;     those must be so on and so forth until 
;   Therefore, the complexity w/o memo is O(2^n) to calculate each element in fibs

; Including memo however cuts down the lookup for each f(n) to O(1) b/c we have the last two values calculated,
;   f(n-1) and f(n-2), conveniently at the front of the memo table.

; Thus, memo cuts down on ~2^n calculations for each element created in fib

;; 3.58: Interpret the following procedure
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; This procedure creates a series by floor dividing a number * radix by a divisor. This number then
;    tries to closely approximate in a series by streaming the sum of expanded remainders


;; 3.59: Streams as integration series
; a. Create proc integrate-series that takes a stream as input -- a0, a1, ... -- and returns a0/1, a1/2, ...
(define (integrate-series a-stream)
  (define (iter stream denom)
    (cons-stream
     (/ (stream-car stream) denom)
     (iter (stream-cdr stream) (+ denom 1))))
  (iter a-stream 1))

; b. Create procs for sine and cosine series given that the derivatives of sine and cosine are cosine and sine
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (integrate-series (stream-map (lambda (x) (* -1 x)) cosine-series))))

;; 3.60: Implement mul-series with add-streams
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

; first element is 1 followed by 0s
(define sine^2+cos^2
  (add-streams (mul-series cosine-series cosine-series) (mul-series sine-series sine-series)))

;; 3.61: invert-unit-series

; SX = 1, where S is power series w/ constant term 1 and X is its inverse
; if we define (1 + Sr) where Sr is (stream-cdr S) --> (1 + Sr)X = 1
; X + SrX = 1
; X = 1 - SrX
; therefore, X is a recursive call to an inversion proc with arg S ... MIND BLOWN
(define (invert-unit-series S)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr S) (invert-unit-series S)) -1))) 
  
(define inv (mul-series (invert-unit-series cosine-series) cosine-series)) ; (1 0 0 0 0 0 0 ...)
