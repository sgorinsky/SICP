;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname nth-power) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (average x y) 
   (/ (+ x y) 2.0)) 
  
 (define (average-damp f) 
   (lambda (x) (average x (f x)))) 
  
 (define tolerance 0.00001) 
  
 (define (fixed-point f first-guess)
   (try f first-guess)) 

   (define (close-enough? v1 v2) 
     (< (abs (- v1 v2)) tolerance)) 

(define (try f guess) 
   (let ((next (f guess))) 
       (if (close-enough? guess next) 
           next 
           (try f next)))) 
   
  
 (define (repeated f n) 
   (if (= n 1) 
       f 
       (lambda (x) (f ((repeated f (- n 1)) x))))) 
  
 (define (get-max-pow n)
   (iter n 1 2))

 (define (iter n p r) 
     (if (< (- n r) 0) 
         (- p 1) 
         (iter n (+ p 1) (* r 2)))) 
    
   
  
 (define (powd b p)
   (itera 1 b p))
 
;;(define (even? x) 
;;  (= (remainder x 2) 0)) 
    
(define (sqrd x) 
  (* x x)) 
    
(define (itera res a n) 
 (if (= n 0) 
   res
    (if (even? n) 
      (itera res (sqrd a) (/ n 2)) 
      (itera (* res a) a (- n 1))))) 
    
  
 (define (nth-root n x) 
   (fixed-point ((repeated average-damp (get-max-pow n)) 
                 (lambda (y) (/ x (powd y (- n 1))))) 
                1.0))

 (define (log2 x) (/ (log x) (log 2)))

 (define (n-root n x) 
   (fixed-point ((repeated average-damp (floor (log 2 n))) 
                 (lambda (y) (/ x (powd y (- n 1))))) 
                1.0))

 (n-root 5 32)