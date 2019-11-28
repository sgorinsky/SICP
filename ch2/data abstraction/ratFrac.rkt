#lang scheme

(define (make-rat n d)
  (cons (/ n (gcd n d)) (/ d (gcd n d))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

(define a (make-rat -7 14))

(define (add-rat a b)
  (make-rat (+ (* (car a) (cdr b)) (* (car b) (cdr a))) (* (cdr a) (cdr b))))