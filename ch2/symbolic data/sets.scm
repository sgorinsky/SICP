#lang scheme
(require "quotation.scm" "differentiation.scm")

(define (element-of-set x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-of-set x (cdr set)))))

(define (adjoin-element x set)
  (if (element-of-set x set)
      set
      (cons x set)))

(define (intersection-set a b)
  (cond ((or (null? a) (null? b)) null)
        ((element-of-set (car a) b) (cons (car a) (intersection-set (cdr a) b)))
        (else (intersection-set (cdr a) b))))