#lang scheme

; 2.19: list form

(define (no-more? l)
  (= (length l) 0))

(define (except-first-denomination lst)
  (cdr lst))

(define (first-denomination lst)
  (car lst))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define us-coins (list 100 50 25 10 5 1))

(define uk-coins (list 200 100 50 20 10 5 2 1))

; original count-change recursive procedure

(define (count-change amount)
  (cc-original amount 5))

(define (cc-original amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= coins 0)) 0)
        (else
         (+ (cc-original amount (- coins 1))
            (cc-original (- amount (get-coin coins))
                coins)))))

(define (get-coin kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

