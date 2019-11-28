#lang scheme

(define (memq x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) l)
        (else (memq x (cdr l)))))
(memq 'apple '(x (apple sauce) y apple pear))

;; 2.54: recursive eq?
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (flatten l)
  (cond ((null? l) l)
        ((not (pair? l)) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(flatten '(this is (a total) racket))

(define (recursive-eq? l1 l2)
  (define (each-eq? q1 q2)
    (cond ((and (null? q1) (null? q2)) #t)
          ((not (eq? (car q1) (car q2))) #f)
          (else (each-eq? (cdr q1) (cdr q2)))))
  (each-eq? (flatten l1) (flatten l2)))

(recursive-eq? '(this is a list) '(this (is a) list))

(recursive-eq? '(this is a list) '(this (is a) big list))
    