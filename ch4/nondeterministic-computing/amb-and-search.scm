#lang sicp

; amb is a special form that "ambiguously" returns a value
; ie. (list (amb 1 2 3) (amb 'a 'b)) has 6 different possibilities
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
(amb n (an-integer-starting-from (+ n 1))))

; 4.35: an-integer-between
(define (an-integer-between begin end)
  (if (= begin end) end
  (amb begin (an-integer-between (+ begin 1) end))))