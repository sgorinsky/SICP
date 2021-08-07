#lang scheme

;; primitives for mutable association lists
(define (mlist . args)
  (define (mut-list mlst)
    (if (null? mlst)'()
        (mcons (car mlst) (mut-list (cdr mlst)))))
  (mut-list args))

;; massoc -- equal? key check
(define (massoc key mlst)
  (if (null? mlst) #f
      (let ((pair (mcar mlst)))
        (if (equal? (mcar pair) key)
            pair
            (massoc key (mcdr mlst))))))

;; massq -- eq? key check
(define (massq key mlst)
  (if (null? mlst) #f
      (let ((pair (mcar mlst)))
        (if (eq? (mcar pair) key)
            pair
            (massoc key (mcdr mlst))))))
            

;; Tables
(define (make-table)
  (mcons 'table null))

(define (entry pair)
  (mcar pair))

(define (lookup key table)
  (if (null? table) #f
      (let ((pair (mcar table)))
        (if (eq? key (entry pair))
            pair
            (lookup key (mcdr table))))))
