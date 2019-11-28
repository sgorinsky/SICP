#lang scheme
(define square (lambda (x) (* x x)))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-lst lst)
  (map (lambda (x) (* x x)) lst))

(define (square-list list)
  (if (null? list)
      '()
      (cons ((lambda (x) (* x x)) (car list)) (square-list (cdr list)))))

(define (square-l items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-ls items)
  (define (iter things answer) (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))


(define (square-lt items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (square (car things)) 
                    ))))
  (iter items '()))