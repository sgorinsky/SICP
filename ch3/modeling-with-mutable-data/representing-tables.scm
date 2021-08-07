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
  (mlist '*table))

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

(define (insert! table key value)
  (let ((record (massoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (let ((new-entry (mlist (mcons key value))))
          (set-mcdr! new-entry (mcdr table))
          (set-mcdr! table new-entry)))))

; (insert! 'a 1 t)
; (insert! 'a 3 t)
; (insert! 'b 10 t)

;; 2d tables
(define (lookup-2d k1 k2 table)
  (let ((subtable (massoc k1 (mcdr table))))
    (if subtable
        (let ((record (massoc k2 (mcdr subtable))))
          (if record
              (mcdr record)
              #f))
        #f)))

(define (insert-2d! table k1 k2 val)
  (let ((subtable (massoc k1 (mcdr table))))
    (if subtable
        (let ((record (massoc k2 (mcdr subtable))))
          (if record
              (set-mcdr! record val)
              (let ((new-record (mlist (mcons k2 val))))
                (set-mcdr! new-record (mcdr subtable))
                (set-mcdr! subtable new-record))))
        (let ((new-subtable (mlist (mlist k1))))
          (set-mcdr! new-subtable (mcdr table))
          (set-mcdr! table new-subtable)
          (let ((new-record (mlist (mcons k2 val))))
            (set-mcdr! (mcar new-subtable) new-record))))))
          

              