#lang scheme

(define (latticePaths rows cols)
  (cond ((or (= rows 1) (= cols 1) (= rows cols)) 1)
        ((and (= cols (- rows 1)) (> rows 1)) (- rows 1))
        (else (+ (latticePaths (- rows 1) cols) (latticePaths (- rows 1) (- cols 1))))))

(define (power x n)
  (define (p hold a b)
    (if (= b 1) hold
        (p (* hold a) a (- b 1))))
  (p x x n))

(define (pascal-sum-row row)
  (define (p-s-r r c)
    (if (= r c) 1
        (+ (latticePaths r c) (p-s-r r (+ c 1)))))
  (p-s-r row 0))

(pascal-sum-row 5)