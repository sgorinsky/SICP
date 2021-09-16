#lang sicp

;; primitives
;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp)
      'false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; if constructor to be used by cond->if
(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

;; 4.25: Suppose we define factorial in terms of unless. What happens if we evaluate (factorial 5)? Will it work in normal order?
;(define (factorial n)
;  (unless (<= n 1) (* n (factorial (- n 1))) 1))

; a. If we evaluate (factorial 5), the program runs infinitely because it tries to evaluate (factorial (- n 1)) infinitely.
;    That is, it doesn't check the conditions that would terminate the infinite calling of factorial.

; b. We can actually model normal-order expressions through translating this into an if statement and that works.
(define factorial
  (lambda (n)
    ((lambda (fact) (fact fact n))
     (lambda (fn k)
       (if (<= k 1)
           1
           (* k (fn fn (- k 1))))))))

;; 4.26: Ben Bitdiddle claims it's possible to implement unless in applicative order as a special form. Alyssa P Hacker
;;       counters by mentioning that would only be syntax, not a proc to be used with higher-order procs. Fill in the details on
;;       both sides of the argument. Implement unless as a derived expression and give an example where it might be better to have
;;       unless as a proc rather than special form.

; a. Alyssa is right. If unless were implemented as a special form, then it's more akin to an if or cond expression (which it's
;        attempting to be). Though Ben is right that it would work.

; b. Implement as a derived expression
(define (unless? exp)
  (tagged-list? exp 'unless))
(define (condition exp)
  (cadr exp))
(define (usual-value exp)
  (caddr exp))
(define (exceptional-value exp)
  (cadddr exp))

(define (unless->if exp)
  (make-if (condition exp) (exceptional-value exp) (usual-value exp)))

; analyze clause:
; ((unless? exp) (analyze (unless->if exp)))

; c. Might be nicer to have unless as proc if you want to see both evaluated procs but only want one return value.