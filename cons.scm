#lang scheme

(define (make-segment start-segment end-segment)
  (cons (- (car end-segment) (car start-segment))
        (- (cdr end-segment) (cdr start-segment))))

(define (make-point x y)
  (cons x y))

(define (x-point z) (car z))
(define (y-point z) (cdr z))

(define (print-point p) (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define square (lambda (y) (* y y)))
(define (rect a b c d)
  (define area 
    (* (sqrt (+ (square (- (cdr d) (cdr c)))
                (square (- (car d) (car c)))))
       (sqrt (+ (square (- (cdr c) (cdr b)))
                (square (- (car c) (car b)))))))
  (define perim
    (+ (sqrt (+ (square (- (cdr d) (cdr c)))
                (square (- (car d) (car c)))))
       (sqrt (+ (square (- (cdr c) (cdr b)))
                (square (- (car c) (car b)))))
       (sqrt (+ (square (- (cdr b) (cdr a)))
                (square (- (car b) (car a)))))
       (sqrt (+ (square (- (cdr a) (cdr d)))
                (square (- (car a) (car d)))))))   
  (cons a (cons b (cons c (cons d (cons perim area))))))


(define a (make-point 1 2))
(define b (make-point 1 4))
(define c (make-point 3 4))
(define d (make-point 3 2))
(define r (rect a b c d))

(define (ons x y) (lambda (m) (m x y)))

(define (ar z)
  (z (lambda (p q) p)))

(define (dr z)
  (z (lambda (p q) q)))



