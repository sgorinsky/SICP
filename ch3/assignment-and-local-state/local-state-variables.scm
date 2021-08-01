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
;; 3.4: Include local variable that "calls-cops" if an account is accessed more than 7 times with an incorrect password
;; 3.7: Modifying make-account to allow for make-joint below
(define (make-account balance password)
  (let ((secrets (list password)) (accesses 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define call-the-cops
      "Calling the cops")
    (define (check-password pass)
      (define (iter passes)
        (cond ((null? passes) #f)
              ((eq? (car passes) pass) #t)
              (else (iter (cdr passes)))))
      (iter secrets))
    (define (add-password pass)
      (set! secrets (append secrets (list pass))))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'add-password) add-password)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    (lambda (pass m)
        (if (not (check-password pass))
            (begin (set! accesses (+ accesses 1))
                   (lambda args
                     (if (> accesses 7)
                         call-the-cops
                         "Incorrect password")))
            (dispatch m)))))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'secret 'withdraw) 40)
((acc 'secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)
((acc 'wrong-secret 'withdraw) 40)


;; 3.7: Introduce make-joint account which allows for joint accounts with more than one password
(define (make-joint account original-password new-password)
  (begin ((account original-password 'add-password) new-password) account))

(define acc2 (make-joint acc 'secret 'new-secret))
((acc2 'new-secret 'withdraw) 5)
((acc2 'secret 'deposit) 100) ;; is it a bug to be able to access the joint account with the other password?

;; 3.8: Define a proc f such that evaluating (+ (f 0) (f 1)) left-right returns 0 and right-left returns 1
(define f
  (lambda ()
    (let ((count 1))
      (lambda (x)
        (set! count (* x count))
        count))))

(define f1 (f))
(+ (f1 0) (f1 1)) ;; left-right --> 0
(define f2 (f))
(+ (f2 1) (f2 0)) ;; right-left --> 1
    