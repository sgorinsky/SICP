#lang scheme

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (average x y)
	(/ (+ x y) 2))

(define square (lambda (x) (* x x)))

(define tolerance 0.00001)
(define (fixed-pt f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2))
		tolerance))
	(define (try guess)
		(let ((next (f guess)))
		(if (close-enough? guess next)
			next
		(try next))))
	(try first-guess))

(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f n)
	(if (= n 0) (lambda (x) x)
		(compose f (repeated f (- n 1)))))

(define (pow x n)
	(if (= n 0) 1
		(* x (pow x (- n 1)))))

(define cube (lambda (x) (* x x x)))

(define (sqrt x)
	(fixed-pt (average-damp (lambda (y) (/ x y))) x))

(define (cbrt x)
	(fixed-pt (average-damp (lambda (y) (/ x (square y)))) x))

(define (frth-rt x)
	(fixed-pt 
		(average-damp 
			(average-damp 
				(lambda(y) (/ x (cube y))))) x))

(define (log2 x) (/ (log x) (log 2))) 

(define (nth-root n x) 
   	(fixed-pt 
   		((repeated average-damp (floor (log2 n))) 
                 (lambda (y) (/ x (pow y (- n 1))))) 
                1.0)) 



(nth-root 5 32)


(define (iterative-improve close-enough? improve)
	(lambda (x) 
		(define (iter n)
			(if (close-enough? n) n
				(iter (improve n))))
	(iter x)))

(define (sq-rt x)
	((iterative-improve 
		(lambda (y) (< (abs (- (square y) x)) .0001))
		(lambda (y) (/ (+ y (/ x y)) 2))) 1.0))

(define (fixed-point f guess)
	((iterative-improve
		(lambda (y) (< (abs (- y (f y))) .00001))
		f) guess))

(define (cubert x)
	((iterative-improve
		(lambda (y) (< (abs (- (cube y) x)) .0001))
		(lambda (y) (/ (+ x (/ (+ y (/ x y)) 2)) 2) 1.0))
		

((lambda (x y) (* x y)) 
	4 
	(let ((y 10)) (* y 8)))

((lambda (x) (x 4)) (lambda (y) (* y (+ y 2))))
