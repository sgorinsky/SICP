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

;; 3.3: Modify make-account so that it creates password protected accounts
(define (make-account balance password)
  (let ((secret password))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    (lambda (pass m)
      (if (not (eq? pass secret))
          (lambda args "Incorrect password") ; This clause expects to return a procedure
          (dispatch m)))))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'secret 'withdraw) 40)
((acc 'secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)