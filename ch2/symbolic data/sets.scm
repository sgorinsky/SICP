#lang scheme
(require "quotation.scm" "differentiation.scm")

;; Note that these implementations of unordered lists as sets is highly inefficient
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

;; 2.59: Union-set
(define (union-set a b)
  (cond ((or (null? a)) b)
        ((element-of-set (car a) b) (union-set (cdr a) b))
        (else (cons (car a) (union-set (cdr a) b)))))