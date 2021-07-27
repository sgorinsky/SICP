#lang scheme
;; data-directed-programming
;; abstraction primitives
(define (type-tag obj)
  (car obj))

(define (contents obj)
  (cdr obj))

(define (attach-tag tag obj)
  (cons tag obj))

(define (make-square s)
  (attach-tag 'square s))

(define table
  (list
   (list 'square (list 'area (lambda (x) (* x x))))))                        

;; get proc from table using obj and op as keys
(define (get obj op)
  (define (get-from-list row)
    (if (null? row) #f
        (let ((cand (car row)))
          (if (eq? op (car cand))
              (cadr cand)
              (get-from-list (cdr row))))))
  (define (get-from-table table)
    (let ((cand (car table)))
      (if (eq? (car cand) obj)
          (get-from-list (cdr cand))
          (get-from-table (cdr table)))))
  (get-from-table table))

;; operate is a generic abstraction proc
(define (operate op obj)
  (let ((proc (get (type-tag obj) op)))
    (if proc
        (proc (contents obj))
        (error "Unknown operator for type"))))

;; abstract out operate proc to lookup 'area op
(define (area shape)
  (operate 'area shape))

(define s4
  (make-square 4))

(area s4)
  

;; message-passing
;; another paradigm where we create lambda procedures we pass messages to

(define (message-pass-square s)
  (lambda (message)
    (cond ((eq? message 'area) (* s s))
          ((eq? message 'perimeter) (* 4 s))
          (else (error "Unknown operator for type")))))

;; ex: instantiate a lambda procedure which expects a "message" to generate
;;     some value
(define mps4 (message-pass-square 4))
(mps4 'area)