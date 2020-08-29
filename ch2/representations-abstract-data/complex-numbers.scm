#lang scheme

;; two procedures for making complex numbers since complex numbers come in two forms:
;;     rectangular: a + bi ; polar: re^(i * theta)

;; examples of constructing rect and polar complex-numbers from z
(make-from-real-imag (real-part z) (imag-part z))

(make-from-mag-ang (magnitude z) (angle z))

;; Operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
(define (mul-complex
         (make-from-mag-ang
          (define (div-complex (make-from-mag-ang z1 z2)
                               (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2))))
          z1 z2)
         (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;; Representations of complex numbers

;; Rectangular 

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; Polar
(define (magnitude-polar z) (car z))

(define (real-part-polar z) (* (magnitude-polar z) (cos (angle z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle z))))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang-polar r a) (cons r a))

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