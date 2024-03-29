#lang scheme

(define (make-account balance)
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
  dispatch)

;; we see that there are two environments binding different local versions of balance
; 3.11: Draw environment diagrams for following procedure calls
(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)

(define acc2 (make-account 50))
((acc2 'deposit) 40)
((acc2 'withdraw) 60)