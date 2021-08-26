#lang sicp

(define square (lambda (x) (* x x)))

; rand primitives
(define (rand-update x)
    (let ((a 7901) (b 7907) (m 7909))
      (modulo (+ (* a x) b) m)))

(define random-init
  (random 100))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x)) x)))

; monte-carlo estimation of pi
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

; show what same thing would look like without modularity
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1) trials-passed
                     x2)))))) (iter trials 0 initial-x))

(define (approximate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))



;; 3.5: implement monte-carlo integration as proc estimate-integral to estimate area of
;       rectangle that encloses unit circle at random

; point prims
(define (make-point x y)
  (list x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cadr point))

; rect prims
(define (make-rectangle p1 p2)
  (list p1 p2))
(define (get-bottom-left rect)
  (car rect))
(define (get-top-right rect)
  (cadr rect))

; circle prims + pred
(define (make-circle x y rad)
  (list (list x y) rad))
(define (get-center circle)
  (car circle))
(define (get-radius circle)
  (cadr circle))

(define (circle-predicate x y r)
  (let ((circle (make-circle x y r)))
    (let ((center (get-center circle)) (radius (get-radius circle)))
      (define (test-point point)
        (let ((x (get-x point)) (y (get-y point)) (cx (get-x center)) (cy (get-y center)))
          (<= (+ (square (- x cx))
                 (square (- y cy)))
              (square radius))))
      test-point)))

; rand-in-range for estimate-integral
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; monte-carlo estimate-integral
(define (estimate-integral pred x1 x2 y1 y2 number-trials)
  (let ((rect (make-rectangle (make-point x1 y1) (make-point x2 y2))))
    (define (is-inside-pred-test)
      (define (random-point-in-rect)
        (list (random-in-range (get-x (get-bottom-left rect)) (get-x (get-top-right rect)))
              (random-in-range (get-y (get-bottom-left rect)) (get-y (get-top-right rect)))))
      (pred (random-point-in-rect)))
    (monte-carlo number-trials is-inside-pred-test)))

(define unit-circle-test (estimate-integral (circle-predicate 0 0 1) -2 2 -2 2 100000))

;; 3.6: Generalized random-number-generator that allows for 'reset and 'generate procs
(define random-number-generator
  (let ((x random-init))
    (define (reset seed)
      (set! x seed)
      x)
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (dispatch m)
      (cond ((eq? m 'reset) reset)
            ((eq? m 'generate) (generate))
            (else (error "No proc " m))))
    dispatch))
            
(define x random-number-generator)
      

