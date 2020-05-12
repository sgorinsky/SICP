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

(define empty-board null)

(define (adjoin-position row col rest)
  (cons (list row col) rest))

;; incomplete
(define (safe? col positions)
  #t)

(define (queens board-size) 
  (define (queen-cols kth-col) 
    (if (= kth-col 0) (list null)
        (filter
         (lambda (positions) (safe? kth-col positions))
         (flatmap
          (lambda (rest-of-queens)
            (display (list "rest-of-queens: " rest-of-queens))
            (newline)
            (map (lambda (new-row)
                   (display (list "new-row k:" new-row kth-col (enumerate-interval 1 board-size)))
                   (newline)
                   (adjoin-position new-row kth-col rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- kth-col 1))))))
  (queen-cols board-size))

;; basically (queens 2) without filter
(define (test k)
  (if (= k 0) (list null)
      (flatmap (lambda (rest)
                 (newline)
                 (display (list "rest: " rest))
           (map (lambda (y)
                  (display (list "adjoin: " (list k y)))
                  (adjoin-position k y rest))
                (list 1 2)))
         (test (- k 1)))))

(test 2)

(define (test-2 k)
  (if (= k 0) (list null)
      (flatmap (lambda (y)
                 (newline)
                 (display (list "k y: " k y))
                 (map (lambda (rest)
                        (display (list "rest: " rest))
                        (adjoin-position k y rest))
                      (test-2 (- k 1))))
         (list 1 2))))

(test-2 2)

(define (test-3 k)
  (if (= k 0) (list null)
  (map (lambda (rest)
         (display (list "rest: " rest))
         (newline)
         (cons (list k 1) rest))
       (test-3 (- k 1)))))
   
(newline)
(test-3 3)
(newline)
;; making permutations to see nested tail-recursion better
(flatmap (lambda (x)
   (map (lambda (y)
          (map (lambda (z) (list x y z))
                 (enumerate-interval 1 3)))
        (enumerate-interval 1 3)))
 (enumerate-interval 1 3))

