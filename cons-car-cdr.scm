#lang scheme

;; new version of cons using a dispatch procedure
(define (cons x y)
    (define (dispatch m)
      (cond ((= m 0) x)
            ((= m 1) y)
            (else (error "Argument no 0 or 1 - CONS" m))))
    dispatch)

(define (car z) (z 0)) ;; <-- it's like saying dispatch 0 where we made x and y

(define (cdr z) (z 1))
