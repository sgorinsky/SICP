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

;; Rectangular-package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar-package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

;; apply-generic looks in table for name of given operation, and applies procedure if found
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

;; For example, with apply-generic, we can define our selectors as follows...
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; 2.73: In section 2.3.2 when we made a symbolic differentiation program, we used
;;       predicates to determine the type of a given expression (product?, sum?, ...)
;;       But we can get the type tag in in another way...

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a: What is the new proc doing? Why aren't we assimilating number? and variable?
;;    It's finding the necessary 'deriv operation associated with a particular type
;;    with the given expression in the package/op table and applying it as necessary.
;;    Since the derivative of a number/same vars are the building blocks of derivatives
;;    and have no "type" associated with them (that can be gotten from '+, '*, etc) b/c
;;    there are none, we just apply the basic operations for derivs of numbers/vars.

;; b: Procedures for derivs of sums and products