#lang scheme

;; helper functions
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (filter predicate? lst)
  (cond ((null? lst) lst)
        ((predicate? (car lst))
            (cons (car lst) (filter predicate? (cdr lst))))
        (else (filter predicate? (cdr lst)))))

(define (map p sequence)
  (accumulate (lambda args (cons (p (car args)) (cadr args))) null sequence))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval start end)
  (cond ((> start end) null)
        ((= start end) (list end))
        (else (cons start (enumerate-interval (+ start 1) end)))))

;; 2.42: complete all unfinished procs in queens proc

(define (create-board size)
  (define (generate-list-lists c n)
    (define (generate-list a)
      (if (= a 0) null
          (cons 0 (generate-list (- a 1)))))
    (if (= n 0) null
        (cons (generate-list c) (generate-list-lists c (- n 1)))))
  (generate-list-lists size size))
         
(define (queens board-size)
  (define empty-board
    (create-board board-size)) 
  (define (queen-cols k)
    (if (= k 0) (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))