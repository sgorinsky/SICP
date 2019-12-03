#lang scheme

;recursive factorial
(define (factorial n)
  (if (= n 1) n
      (* n (factorial (- n 1)))))

;iterable factorial
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial-iter n)
  (fact-iter 1 1 n))

;Ackermann's Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))