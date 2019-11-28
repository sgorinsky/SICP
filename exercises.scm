#lang scheme

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (square x)
  (* x x))

;1.2:
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;1.3: procedure that takes 3 numbers as arguments and returns the sum of the squares of the two larger numbers
 (define (sum-of-larger-squares a b c)
    (cond ((or (> a b) (> a c))
          (cond ((> b c) (+ (* a a) (* b b)))
                ((> c b) (+ (* a a) (* c c)))))
          ((or (> b a) (> b c))
          (cond ((> a c) (+ (* b b) (* a a)))
                ((> c a) (+ (* b b) (* c c)))))
          ))
;1.11: computing f(n) through a recursive and iterative process
(define (func n)
  (if (< n 3) n
      (+ (func (- n 1)) (* 2 (func (- n 2))) (* 3 (func (- n 3))))))

 (define (f n) (f-iter n 0 1 2)) 
  (define (f-iter i a b c) 
   (cond ((< i 0) i) 
         ((= i 0) a) 
         (else (f-iter (- i 1) b c (+ c (* 2 b) (* 3 a))))))

;1.12: Pascal's Triangle
(define (Pascal row entry)
  (cond ((< row entry) 0)
        ((or (= row entry) (= entry 1)) 1)
        (else (+ (Pascal (- row 1) (- entry 1)) (Pascal (- row 1) entry)))))

;1.13: Fib(n)
(define (power n x y count)
  (if (= count n) x
  (power n (* x y) y (+ count 1))))

(define (^ x n) (power (- n 1) x x 0))

(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))

(define (Fib-phi n)
  (inexact->exact (/ (^ phi n) (sqrt 5))))
(define (Fib-psi n)
  (inexact->exact (/ (^ psi n) (sqrt 5))))

(define (Fib n)
  (inexact->exact (round (- (Fib-phi n) (Fib-psi n)))))
  
;1.16 - recursive and iter

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
        
(define (fast-exp-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-exp-iter a (square b) (/ n 2)))
        (else (fast-exp-iter (* a b) b (- n 1)))))

(define (f-exp-iter b n)
  (fast-exp-iter 1 b n))

;1.17: positives only
(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(define (fast-mult-iter a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mult-iter (double a) (halve b)))
        (else (+ a (fast-mult-iter a (- b 1))))))


;1.29
(define (Simpsons-Rule func a b n)
  (* (/ (h a b n) 3) (Simp func a b a n 1)))


(define (h a b n)
  (/ (- b a) n))

(define (cube x)
  (* x x x))

(define (Simp func a b c n count)
  (cond ((odd? n) 0)
        ((= a b) (func b)) 
        ((= count 1) (+ (func a) (Simp func (+ (h c b n) a) b c n (+ count 1))))        
        ((even? count) (+ (* 4.0 (func a)) (Simp func (+ (h c b n) a) b c n (+ count 1))))
        (else (+ (* 2.0 (func a)) (Simp func (+ (h c b n) a) b c n (+ count 1))))))
        

;1.30
(define (s-iter a b next result)
  (if (= (- a 1) b) result
      (s-iter (next a) b next (+ a result))))

(define (sum-iter a b)
  (s-iter a b incr 0))



;1.31
(define (factorial b)
  (product 1 b))

(define (product a b)
  (sigma * a incr b))

(define (incr x)
  (+ x 1))

(define (inc-by-two x)
  (+ x 2))
  
(define (sigma term a next b)
  (if (> a b) 1
      (term a (sigma term (next a) next b))))

(define (fact-iter a count)
  (if (= count 0) a
      (fact-iter (* a count) (- count 1))))

(define (factorial-iter a)
  (fact-iter a (- a 1)))

(define (pi-approx a b)
  (cond ((> a b) 4)
        ((even? a) (* (/ a (+ a 1)) (pi-approx (+ a 1) b)))
        (else (* (/ (+ a 1) a) (pi-approx (+ a 1) b)))))

(define (pi-series n)
  (pi-approx 2 n))
      
;1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b) a
      (accumulate combiner null-value term (combiner (term a) (next a)) next b)))

;1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))