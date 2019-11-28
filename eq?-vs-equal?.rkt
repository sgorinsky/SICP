#lang scheme
(define a (list 'a 'b 'c))
(define b (list 'a 'b 'c))

(equal? a b) ;; equal? checks if two symbols are the same
(eq? a b) ;; eq? checks if two things are referenced the same in memory