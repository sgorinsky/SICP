#lang sicp

(define (cons x y) (lambda (m) (m x y))) ; cons pair is a proc that hasn't been called
(define (car z) (z (lambda (p q) p))) ; z is a cons (lazy) list that expects a procedure as its lambda arg
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
(define a (cons 1 (cons 2 (cons 3 '()))))
(display (cdr a))
(newline)
(define (ones) (cons 1 ones))
(display (car ((cdr (ones))))) ; need to call ones to elicit a lazy list

;(define (integral integrand initial-value dt)
;  (define int
;    (cons initial-value
;          (add-lists (scale-list integrand dt) int)))
;  int)
;
;(define (solve f y0 dt)
;  (define y (integral dy y0 dt)) (define dy (map f y))
;  y)

;; 4.32: What makes lazy lists "lazier" than streams?
; Well in lazy lists, both elements of list are thunks to be forced
; We can take advantage of extra laziness by having lazy lists of lazy lists

;; 4.33: Modify evaluator to handle (car '(a b c))
; handle eval quoted clause by treating extended quoted expressions as list to be evaluated
;((quoted? exp)
; (if (not (pair? (text-of-quotation exp)))
;     (text-of-quotation exp)
;     (eval (text-of-quotation exp)) env))


;; 4.34: Modify the driver loop so that lazy pairs and lists will print in some reasonable way
; We'll want to tag lazy lists in some way so that we can extract its first contents then provide some indicator like ... to
;    express the rest of the elements