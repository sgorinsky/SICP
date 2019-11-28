#lang scheme

(define (append l1 l2)
  (if (not (pair? l1)) (cons l1 l2)
      (cons (car l1) (append (cdr l1) l2))))

(define (fringe x)
  (if (not (pair? x)) x
      (append (fringe (car x)) (fringe (cdr x)))))

(define x (list (list (list 1 2) (list 3 4)) (list 5 6)))

(fringe x)