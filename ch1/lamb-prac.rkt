#lang scheme
((lambda (x) (+ x x))
 ((lambda (x) (* x x)) 5))

( (lambda (x) (* x 2))
   ((lambda (x) (* x x))
    ((lambda (x y) (+ x y))
     ((lambda (x) (* x x)) 2) ((lambda (x) (* x 2)) 3))) )

( (lambda (x y z) (* x y z))
    ((lambda (x) (* x 4)) 2)
    ((lambda (y) (+ y 2)) 2)
    ((lambda (z) (+ 1 z)) 1) )

(define (f a)
    (let ((a (+ a 3)))
    (* a 4)))

( (lambda (x y z)
    (let ((a (+ x 2))
          (b (* y 4))
          (c (- z 3)))
      (+ a (* c (- b 1)))))
  ((lambda (x) (- x 3)) 4) 2 4)


;Exercise 1.35
(define (close-enough? v1 v2) (< (abs (- v1 v2)) .00001))

(define (try f guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
        next
    (try f next))))
  
(define (golden-ratio x)
  (try (lambda (x) (+ 1 (/ 1 x))) 3))

;; map-square
(define (map f l)
    (if (null? l) l
        (cons (f (car l)) (map f (cdr l)))))

(define square
    (lambda (x) (* x x)))

(define a (list 1 2 3 4 5))

(map square a)