;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lambdas-lets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (plus4 a)
  (let ((x a))
    (lambda () (+ x 4))))

((plus4 4))


(define times2 (lambda (x) (* 2 x)))

(define play (lambda (x y z) (* (+ x 2) (- y 1) (* z 2))))

(let ([x 5]) (* x 3))
