#lang sicp

;; 4.15: Prove that procedure halts? to determine if an expression halts -- that is, if a proc on a or (proc a) returns a value --
;;       is impossible

(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

; can't construct halts b/c if it calls itself infinitely, then we'll never be able to break out of predicate
;    can't count calls to itself b/c procs can call themselves an arbitrary number of times
;    hard to construct a proc that determines if (p a) will eventually reach a base case