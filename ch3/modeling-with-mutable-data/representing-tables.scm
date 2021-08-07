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
  (mcons '*table* null))

(define (entry pair)
  (mcar pair))

(define (lookup key table)
  (if (null? table) #f
      (let ((record (massoc key (mcdr table))))
        (if record
            (mcdr record)
            #f))))

(define t (make-table))
(lookup 'table t)

(define (insert! key value table)
  (let ((record (massoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (let ((new-entry (mlist (mcons key value))))
          (set-mcdr! new-entry (mcdr table))
          (set-mcdr! table new-entry)))))

;(insert! 'a 1 t)
;(insert! 'a 3 t)
;(insert! 'b 10 t)