#lang scheme

;; 2.44 define up-split used by corner split


;; 2.46: 2-d vector constructor
(define (make-vect elements)
  (lambda x
    (if (number? x) x
        (cons (car x) (make-vect (cdr x)))))
  elements)

(define (x-coord v)
  (car v))

(define (y-coord v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (x-coord v1) (x-coord v2))
               (+ (y-coord v1) (y-coord v2))))

(define (subt-vect v1 v2)
  (cons (- (x-coord v1) (x-coord v2))
        (- (y-coord v1) (y-coord v2))))

(define (scale s v)
  (cons (* s (x-coord v))
        (* s (y-coord v))))

(define a (make-vect (list 1 2)))
(define b (make-vect (list 3 4)))

