#lang scheme
;; We can use type tag to distinguish between rectangular and polar coords
(define (type-tag datum)
  (if (pair? datum) (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; Method of modularizing system design and creating packages for others to interface with
; ie. Representing complex-numbers in different ways
; And note: although both packages have some colliding names for certain procedures,
;    they operate within each package's namespace

;; At the moment, we don't have put and get selectors

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
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

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
(define (install-sum-package) ;; copy-paste procs from earlier deriv and put procs in table
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s)) ;; second item of sum list
  (define (augend s) (caddr s)) ;; third item of sum list
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp)
    (make-sum
     (deriv (addend exp))
     (deriv (augend exp))))

  (define (tag x) (attach '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (install-product-package)
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p)) ;; second item of product list
  (define (multiplicand p) (caddr p)) ;; third item of product list
 
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
 
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          ((and (variable? m1) (variable? m2) (eq? m1 m2)) (list '** m1 2))
          (else (list '* m1 m2))))

  (define (deriv-product exp)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (define (tag x) (attach '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

;; c: Create another deriv package other than sum or product
(define (install-exponentiation-package)
  (define (** x y)
  (define (power a n)
    (if (= n 0) a
        (power (* a x) (- n 1))))
  (power 1 y))

  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

  (define (base exp)
    (cadr exp))

  (define (exponent exp)
    (caddr exp))

  (define (make-exponentiation a b)
    (cond ((=number? a 0) 0)
          ((=number? b 1) a)
          ((or (=number? b 0) (=number? a 1)) 1)
          ((and (number? a) (number? b)) (** a b))
          (else (list '** a b))))

  (define (deriv-exp exp)
    (if (eq? (base exp) var)
             (make-product
              (exponent exp)
              (make-exponentiation (base exp) (make-sum (exponent exp) (- 1))))
             0))

  (define (tag x) (attach '** x))
  (put 'deriv '(**) deriv-exp)
  (put 'make-exponentiation '**
       (lambda (x y) (tag (make-exponentiation x y))))
  'done)