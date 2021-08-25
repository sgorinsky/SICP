#lang sicp

; stream boilerplate
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-ref stream n)
  (if (<= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
(define (stream-map proc . streams)
  (cons-stream
   (apply proc (map car streams))
   (stream-map (cons proc (map stream-cdr streams)))))
(define (add-streams . streams)
  (stream-map + streams))
(define (scale-stream stream scale)
  (stream-map (lambda (x) (* x scale)) stream))

;; 3.77: Modify integral proc so it expects integrand as delayed arg
(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand)) initial-value)
                   dt)))))

;; 3.78: Create proc that generates successive values of y from diff eq