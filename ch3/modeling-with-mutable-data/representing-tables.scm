#lang scheme

;; primitives for mutable association lists
(define (mlist . args)
  (define (mut-list mlst)
    (if (null? mlst)'()
        (mcons (car mlst) (mut-list (cdr mlst)))))
  (mut-list args))

;; massoc -- equal? key check
(define (massoc key mlst)
  (if (not (mpair? mlst))
      #f
      (let ((record (mcar mlst)))
        (cond ((not (mpair? record)) #f)
              ((equal? (mcar record) key) record)
              (else (massoc key (mcdr mlst)))))))

;; massq -- eq? key check
(define (massq key mlst)
  (if (not (mpair? mlst))
      #f
      (let ((record (mcar mlst)))
        (cond ((not (mpair? record)) #f)
              ((eq? (mcar record) key) record)
              (else (massq key (mcdr mlst)))))))
            

;; Tables
(define (make-table)
  (mlist '*table*))

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

(define g (make-table))
(insert! g 'a 1)
(insert! g 'a 3)
(insert! g 'b 10)

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
  (let ((local-table (make-table)) (lookup-proc massoc))
    (define (lookup k1 k2)
      (let ((subtable (lookup-proc k1 (mcdr local-table))))
        (if subtable
            (let ((record (lookup-proc k2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! k1 k2 val)
      (let ((subtable (lookup-proc k1 (mcdr local-table))))
        (if subtable
            (let ((record (lookup-proc k2 (mcdr subtable))))
              (if record
                  (set-mcdr! record val)
                  (let ((new-record (mcons k2 val)))
                    (set-mcdr! subtable (mcons new-record (mcdr subtable))))))
            (let ((new-record (mcons k2 val)))
              (let ((new-subtable (mcons k1 (mlist new-record))))
                (set-mcdr! local-table (mcons new-subtable (mcdr local-table)))))))
      'ok)

    (define (set-lookup!)
      (display "Use 'eq?' or 'equal?' ")
      (let ((response (read)))
        (cond ((or (eq? response 'eq?) (eq? response 'eq)) (set! lookup-proc massq))
              ((or (eq? response 'equal?) (eq? response 'equal)) (set! lookup-proc massoc))
              (else (error "Improper answer: " response)))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            ((eq? m 'set-lookup) (set-lookup!))
            ((eq? m 'print) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
      
(define lt (table))
((lt 'insert) 'a 'b 'c)
((lt 'insert) 'a 'c 'd)
((lt 'lookup) 'a 'b)

;; 3.24: Include in make-table constructor an arg same-key? which tests equality of key
(define (create-table same-key?)
  (let ((local-table (make-table)))

    (define assoc ; already defined massoc and massq above so no need to copy-paste definition
      (if (eq? same-key? eq?)
          massq
          massoc))
      
    (define (lookup k1 k2)
      (let ((subtable (assoc k1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc k2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    
    (define (insert! k1 k2 val)
      (let ((subtable (assoc k1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc k2 (mcdr subtable))))
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

(define ls (create-table 'eq?))
((ls 'insert) 'a 'b 'c)
((ls 'lookup) 'a 'b)

;; 3.25: Generalize table lookups to >= 1d
(define (generalized-table)
  (let ((local-table (make-table)) (assoc-proc massoc))

    (define (construct-table key)
      (mlist key))
    
    (define (lookup key-list)
      (define (helper table keys)
        (if (null? keys)
            #f
            (let ((subtable (assoc-proc (car keys) (mcdr table))))
              (if subtable
                  (if (null? (cdr keys))
                      (mcdr subtable)
                      (helper subtable (cdr keys)))
                  #f))))
      (helper local-table key-list))
                             
    (define (insert! key-list val)
      (define (helper table keys)
        (if (null? keys) val
        (let ((records (assoc-proc (car keys) (mcdr table))))
          (if records
              (if (null? (cdr keys))
                  (set-mcdr! records val)
                  (helper records (cdr keys)))
              (begin
                (set-mcdr! table
                           (mcons
                            (if (null? (cdr keys))
                                (mcons (car keys) val)
                                (mcons (car keys) (mlist (helper (mlist null) (cdr keys)))))
                            (mcdr table)))
                (mcar (mcdr table)))))))
      (helper local-table key-list)
      'ok)

    (define (set-lookup!)
      (display "Use 'eq?' or 'equal?' ")
      (let ((response (read)))
        (cond ((or (eq? response 'eq?) (eq? response 'eq)) (set! assoc-proc massq))
              ((or (eq? response 'equal?) (eq? response 'equal)) (set! assoc-proc massoc))
              (else (error "Improper answer: " response)))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert!)
            ((eq? m 'set-lookup) (set-lookup!))
            ((eq? m 'print) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define gt (generalized-table))
((gt 'insert) (list 'b 'c) 'e)
((gt 'insert) (list 'b 'c) 'D)