;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sum-to-squares) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x) (* x x))
(define (sum-squares a b)
  (+ (square a) (square b)))
(define add +)
(define (sum-to-squares a b)
  (if (> a b) 0
      (+ (square a) (sum-to-squares (+ a 1) b))))