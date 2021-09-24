#lang sicp

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items) '()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2) ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

; requires lazy evaluator to call cdr
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

;; 4.32: What makes lazy lists "lazier" than streams?
; Well in lazy lists, both elements of list are thunks to be forced
; We can take advantage of extra laziness by having lazy lists of lazy lists

;; 4.33: Modify evaluator to handle (car '(a b c))
; handle eval quoted clause by treating extended quoted expressions as list to be evaluated
;((quoted? exp)
; (if (not (pair? (text-of-quotation exp)))
;     (text-of-quotation exp)
;     (eval (text-of-quotation exp)) env))
