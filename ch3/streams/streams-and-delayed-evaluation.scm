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
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt)) 
  (define ddy (add-streams (scale-stream a dy) (scale-stream b y)))
  y)

;; 3.79: Generalize solve-2nd so d^2 y/dt^2 = f(dy/dt, y)
(define (general-solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; 3.80: RLC Diffeq
(define (RLC R L C vc0 iL0 dt)
  (define diL (add-streams (scale-stream vc (/ 1 L)) (scale-stream iL (/ -R L))))
  (define dvc (scale-stream iL (/ -1 C)))
  (define vc (integral (delay dvc) vc0 dt))
  (define iL (integral (delay diL) iL0 dt))
  (cons vc iL)) ; ((ptr to vc stream) . (ptr to iL stream))
  