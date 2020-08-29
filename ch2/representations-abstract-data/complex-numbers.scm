#lang scheme

;; two procedures for making complex numbers since complex numbers come in two forms:
;;     rectangular: a + bi ; polar: re^(i * theta)

;; examples of constructing rect and polar complex-numbers from z
(make-from-real-imag (real-part z) (imag-part z))

(make-from-mag-ang (magnitude z) (angle z))

;; complex operations
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