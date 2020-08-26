#lang scheme

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (if (< (car x) (cdr x)) (car x)
      (cdr x)))

(define (upper-bound x)
  (if (> (car x) (cdr x)) (car x)
      (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (sub-interval x y)
  (if (> (upper-bound x) (upper-bound y))
      (make-interval (- (lower-bound x) (lower-bound y))
                     (- (upper-bound x) (upper-bound y)))
      (make-interval (- (lower-bound y) (lower-bound x))
                     (- (upper-bound y) (upper-bound x)))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
 
(define (div-interval x y)
  (if (or (= 0 (upper-bound y)) (= 0 (lower-bound y))) 'undefined
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;9-case mul-interval
