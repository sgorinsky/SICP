#lang scheme

;; a package that imports mutable primitives for working on assoc table
;; mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)


(define alist '((john . rhythm) (paul . bass) (george . drums) (george . lead)))

(define b (cdr (cons 'guitar 'paul)))

(assoc b alist) ;; uses equal? to make key comparisons 
(assq b alist) ;; uses eq? to make key comparisons

(assoc 'mick alist)

;; sentinel or dummy node at front to avoid issues with mutating assoc-list
;; we're making a mutable assoc list so that we can change it at any point we want
;; using get and put
(define the-table (mlist '*table*))


(define (get key table)
  (let ((record (assoc key (mcdr table)) ))
    (if (not record)
        #f
        (mcdr record))))

(define (put key value table)
  (let ((record (assoc key (mcdr table)) ))
    (set-mcdr! table
               (mcons (mcons key value)
                      (mcdr the-table)))
    (set-mcdr! record value)))
    

;; memoized version of fibonacci
;; recall original fib runs in 2^n time, really slow!


;; instead, we can store our fibonacci numbers in an assoc list
;; and use them to help us find more answers in the sequence

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
'ok)

                                     
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) false)) false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))



(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table) result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2)))) ))))



(define (fast-fib n)
  (if (< n 2)
      n
      (let ((old (get 'fib n)))
        (if (number? old)
            old
            (begin
              (put 'fib n (+ (fast-fib (- n 1))
                             (fast-fib (- n 2))))
              (get 'fib n))))))