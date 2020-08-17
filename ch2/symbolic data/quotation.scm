#lang scheme

(define (memq x l)
  (cond ((null? l) #f)
        ((eq? (car l) x) l)
        (else (memq x (cdr l)))))
(memq 'apple '(x (apple sauce) y apple pear))

;; 2.53: evaluating following expressions
(list 'a 'b 'c) ;; (a b c)
(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; ((y1 y1))
(cadr '((x1 x2) (y1 y2))) ;; (y1 y1)
(pair? (car '(a short list))) ;; #f
(memq 'red '((red shoes) (blue socks))) ;; #f
(memq 'red '(red shoes blue socks)) ;; (red shoes blue socks)

;; 2.54: equal? checks if each element if all elements in a and b are the same
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (pair? (car a)) (pair? (car b)))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((not (eq? (car a) (car b))) #f)
        (else (equal? (cdr a) (cdr b)))))

(equal? '(this is a list) '(this is a list))

;; recursive-eq
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
    