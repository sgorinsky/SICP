;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname newton-transform) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (fixed-point f guess)
  (if ((lambda (x) (< (abs (- (f x) x)) .00001)) guess) (f guess)
      (fixed-point f (f guess))))

(define square (lambda (x) (* x x)))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx .00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define cube (lambda (x) (* x x x)))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 3) 1)

(define (double f)
  (lambda (x) (f (f x))))

(define inc (lambda (x) (+ x 1)))

(define (composition f g)
  (lambda (x) (f (g x))))

((composition square inc) 6)

(define (repeated f n)
  (lambda (x) (if (> n 0) ((composition f (repeated f (- n 1))) x) x)))

(define (repeat-d f n)
  (if (= n 0) (lambda (x) x)
      (composition f (repeat-d f (- n 1)))))
    
((repeated square 2) 5)
((repeat-d square 2) 5)

(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth square 2) 5)

(define (pow x n)
  (if (= n 0) 1
      (* x (pow x (- n 1)))))

(define (srt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))


(define (cbrt x)
	(fixed-point (average-damp (lambda(y) (/ x (square y)))) x))

(define (frth-rt x)
	(fixed-point 
		(average-damp 
			(average-damp 
				(lambda(y) (/ x (cube y))))) x))


(define (log2 x) (/ (log x) (log 2))) 

(define (nth-root n x) 
   	(fixed-point 
   		((repeated average-damp (floor (log2 n))) 
                 (lambda (y) (/ x (pow y (- n 1))))) 
                1.0)) 