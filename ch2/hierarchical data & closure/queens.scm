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

(define (get-coordinate-by-column target-column coordinates) 
  (cond ((null? coordinates) null) 
        ((= target-column (cadr (car coordinates))) (car coordinates)) 
        (else (get-coordinate-by-column target-column (cdr coordinates)))))

(define (safe? test-column positions) 
  ;is the coordinate in the set of positions with the given column 
  ;"safe" with respect to all the other coordinates (that is, does not 
  ;sit on the same row or diagonal with any other coordinate)? 
  ;we assume all the other coordinates are already safe with respect 
  ;to each other  
  (define (two-coordinate-safe? c1 c2)
    (let ((row1 (car c1)) 
          (row2 (car c2)) 
          (col1 (cadr c1)) 
          (col2 (cadr c2))) 
      (if (or (= row1 row2) ;; row check
              (= (abs (- row1 row2)) (abs (- col1 col2)))) ;; diagonal check
          #f 
          #t))) 
  (let ((test-coordinate (get-coordinate-by-column test-column positions))) 
    ;check the test coordinate pairwise against every other coordinate, 
    ;rolling the results up with an "and," and seeding the and with 
    ;an initial "true" value (because a list with one coordinate is 
    ;always "safe" 
    (accumulate
     (lambda (coordinate results)  
             (and (two-coordinate-safe? test-coordinate coordinate) results)) 
           #t 
           (remove test-coordinate positions)))) 

(define empty-board null)

(define (adjoin-position row col rest)
  (cons (list row col) rest))
  
(define (queens board-size) 
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