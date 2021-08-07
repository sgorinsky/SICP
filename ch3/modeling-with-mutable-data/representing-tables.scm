#lang scheme

;; primitives for mutable association lists
(define (mlist . args)
  (define (mut-list mlst)
    (if (null? mlst)'()
        (mcons (car mlst) (mut-list (cdr mlst)))))
  (mut-list args))

;; massoc -- equal? key check
(define (massoc key mlst)
  (if (null? mlst)
      #f
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
        (let ((new-entry (mcons key value)))
          (set-mcdr! table (mcons new-entry (mcdr table)))))))

; (insert! 'a 1 t)
; (insert! 'a 3 t)
; (insert! 'b 10 t)

;; 2d tables
(define (lookup-2d table k1 k2)
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
              (let ((new-record (mcons k2 val)))
                (set-mcdr! subtable (mcons new-record (mcdr subtable))))))
        (let ((new-subtable (mlist k1)))
          (set-mcdr! table (mcons new-subtable (mcdr table)))
          (let ((new-record (mlist (mcons k2 val))))
            (set-mcdr! new-subtable new-record))))))

(insert-2d! t 'a 'b 'c)
(lookup-2d t 'a 'b)

;; table object
(define (table)
  (let ((local-table (make-table)))
    (define (lookup k1 k2)
      (let ((subtable (massoc k1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc k2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! k1 k2 val)
      (let ((subtable (massoc k1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc k2 (mcdr subtable))))
              (if record
                  (set-mcdr! record val)
                  (let ((new-record (mcons k2 val)))
                    (set-mcdr! subtable (mcons new-record (mcdr subtable))))))
            (let ((new-record (mcons k2 val)))
              (let ((new-subtable (mcons k1 (mlist new-record))))
                (set-mcdr! local-table (mcons new-subtable (mcdr local-table)))))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            ((eq? m 'print) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
      
(define lt (table))
((lt 'insert) 'a 'b 'c)
((lt 'insert) 'a 'c 'd)
((lt 'lookup) 'a 'b)