;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname roots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(let ((a 3))
    (let ((b (+ a 5)))
          (* a b)))

(define (square x) (* x x))

(let ((-b (- 4))) (+ 3 -b))

(define (roots a b c)
    (let ((d (sqrt (- (square b) (* 4 a c))))
          (-b (- b))
          (2a (* 2 a)))
      (list (/ (+ -b d) 2a)
          (/ (- -b d) 2a) )))