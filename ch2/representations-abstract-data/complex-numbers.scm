#lang scheme
(require "../symbolic-data/differentiation.scm")

(define square
  (lambda (x) (* x x)))
  
;; two procedures for making complex numbers since complex numbers come in two forms:
;;     rectangular: a + bi ; polar: re^(i * theta)

;; Representations of complex numbers (revised to distinguish between rect and polar)
;; Rectangular 

;(define (real-part-rectangular z) (car z))
;(define (imag-part-rectangular z) (cdr z))
;
;(define (magnitude-rectangular z)
;  (sqrt (+ (square (real-part-rectangular z))
;           (square (imag-part-rectangular z)))))
;
;(define (angle-rectangular z)
;  (atan (imag-part-rectangular z)
;        (real-part-rectangular z)))
;
;;; Polar
;(define (real-part-polar z)
;  (* (magnitude-polar z) (cos (angle-polar z))))
;(define (imag-part-polar z)
;  (* (magnitude-polar z) (sin (angle-polar z))))
;
;(define (magnitude-polar z) (car z))
;
;(define (angle-polar z) (cdr z))
;
;(define (make-from-real-imag-polar x y)
;  (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x))))
;
;(define (make-from-mag-ang-polar r a)
;  (attach-tag 'polar (cons r a)))


;; We can use type tag to distinguish between rectangular and polar coords
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; Predicates check for type-tag
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z) (eq? (type-tag z) 'polar))

;; Generic Selectors

;(define (real-part z)
;  (cond ((rectangular? z)
;         (real-part-rectangular (contents z)))
;        ((polar? z)
;         (real-part-polar (contents z)))
;        (else (error "Unknown type: REAL-PART" z))))
;
;(define (imag-part z)
;  (cond ((rectangular? z)
;         (imag-part-rectangular (contents z)))
;        ((polar? z)
;         (imag-part-polar (contents z)))
;        (else (error "Unknown type: IMAG-PART" z))))
;
;(define (magnitude z)
;  (cond ((rectangular? z)
;         (magnitude-rectangular (contents z)))
;        ((polar? z)
;         (magnitude-polar (contents z)))
;        (else (error "Unknown type: MAGNITUDE" z))))
;
;(define (angle z)
;  (cond ((rectangular? z)
;         (angle-rectangular (contents z)))
;        ((polar? z)
;         (angle-polar (contents z)))
;        (else (error "Unknown type: ANGLE" z))))
;
;;; examples of constructing rect and polar complex-numbers from z (updated)
;(make-from-real-imag (real-part z) (imag-part z))
;
;(define (make-from-real-imag x y) (make-from-real-imag-rectangular x y))
;
;(define (make-from-mag-ang r a) (make-from-mag-ang-polar r a))
;
;;; Operations
;(define (add-complex z1 z2)
;  (make-from-real-imag (+ (real-part z1) (real-part z2))
;                       (+ (imag-part z1) (imag-part z2))))
;
;(define (sub-complex z1 z2)
;  (make-from-real-imag (- (real-part z1) (real-part z2))
;                       (- (imag-part z1) (imag-part z2))))

;(define (mul-complex
;         (make-from-mag-ang
;          (define (div-complex (make-from-mag-ang z1 z2)
;                               (* (magnitude z1) (magnitude z2))
;                               (+ (angle z1) (angle z2)))) z1 z2)
;         (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;; Package (collection of procedures) that provides content to a table that is looked up
;;    by get and put procedures whenever a relevant item needs to be accessed
;; The procedures may be named the same thing in both the rectangular and polar
;;    packages, but they are local to the given package's scope