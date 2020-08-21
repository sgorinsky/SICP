#lang scheme
(require "quotation.scm" "differentiation.scm")

(define (element-of-set x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-of-set x (cdr set)))))