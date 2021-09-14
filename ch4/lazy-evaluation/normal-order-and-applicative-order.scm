#lang sicp

(define (unless condition usual-value exceptional-value)
  (if condition
   exceptional-value
   usual-value))

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