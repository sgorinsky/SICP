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

;; 3.26: Describe a table implementation where the (key, value) records are organized using a binary tree,
;;       assuming that keys can be ordered in some way (e.g., numerically or alphabetically).

;; Each node could be a pair whose car is a key and whose cdr is a pair referencing other nodes
;;   If the keys are ordered in the binary tree, then we can more easily search for the key we're looking for
;;   and recursively search the subtree of each node for the key we're searching,
;;   otherwise add a new node whose cdr is a new subtree

(define (tree-table)
  ; selectors
  (define (create-branch key val)
    (mlist (mcons key val) null null))
  (define (get-node branch)
    (mcar branch))
  (define (get-key branch)
    (mcar (get-node branch)))
  (define (get-value branch)
    (mcdr (get-node branch)))
  (define (left-branch branch)
    (mcar (mcdr branch)))
  (define (right-branch branch)
    (mcar (mcdr (mcdr branch))))
  (define (go-to-next branch key)
    (cond ((null? branch) null)
          ((< key (get-key branch)) (left-branch branch))
          ((> key (get-key branch)) (right-branch branch))
          (else (equal? key (get-key branch)) (get-value branch))))

  ; predicates
  (define (is-leaf? branch)
    (and (null? left-branch) (null? right-branch)))
  (define (is-next-empty? branch key)
    (or (is-leaf? branch) (null? (go-to-next branch key))))
  
  ; mutators
  (define (create-left-branch! branch key val)
    (set-mcar! (mcdr branch) (create-branch key val)))
  (define (create-right-branch! branch key val)
    (set-mcar! (mcdr (mcdr branch)) (create-branch key val)))
  (define (set-value! branch val)
    (set-mcdr! (get-node branch) val))
  (define (add-branch! branch key)
    (if (is-next-empty? branch key)
        (if (< key (get-key branch))
            (begin
              (create-left-branch! branch key null)
              (left-branch branch))
            (begin
              (create-right-branch! branch key null)
              (right-branch branch)))
        (add-branch! (go-to-next branch key) key)))
               
  ; finders
  (define (assq-tree branch key)
    (cond ((null? branch) #f)
          ((eq? key (get-key branch)) branch)
          (else (assq-tree (go-to-next branch key) key))))

   
           
  (let ((local-table null))
    ;; internal procs
    (define (insert! key-list val)
      (define (helper branch keys)
        (let ((subtree (assq-tree branch (car keys))))
          (if subtree
              (if (null? (cdr keys))
                  (set-value! subtree val)
                  (begin
                    (if (not (mpair? (get-value subtree)))
                        (set-value! subtree (create-branch (cadr keys) null))
                        subtree)
                    (helper (get-value subtree) (cdr keys))))
              (helper (add-branch! branch (car keys)) keys))))

      (cond ((not (pair? key-list)) (error "Provide a list of keys"))
            ((null? local-table)
             (begin
               (set! local-table (create-branch (car key-list) null))
               (helper local-table key-list)))
             (else (helper local-table key-list)))
      'ok)

    (define (lookup key-list)
      (define (helper tree keys)
        (if (null? keys)
            #f          
            (let ((tree (assq-tree tree (car keys))))
              (if tree
                  (if (null? (cdr keys))
                      (get-value tree)
                      (helper (get-value tree) (cdr keys)))
                  #f))))
      (helper local-table key-list))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
            ((eq? m 'lookup) lookup)
            ((eq? m 'print) local-table)
            (else (error "No proc: " m))))
    
    dispatch))

(define tree (tree-table))
((tree 'insert) (list 1 3) 2)
(tree 'print)
((tree 'insert) (list 0) 4)
(tree 'print)
((tree 'insert) (list 2) 5)
(tree 'print)
((tree 'insert) (list 10 2) 3)
((tree 'lookup) (list 1))
((tree 'insert) (list 1 2 3) 4)
((tree 'lookup) (list 1 2))

;; 3.26: Draw env diagram for memoized fib
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup table x)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert-2d! table x result) result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))
      