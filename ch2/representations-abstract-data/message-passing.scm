#lang scheme
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

(define (operate op obj)
  (let ((proc (get (type-tag obj) op)))
    (if proc
        (proc (contents obj))
        (error "Unknown operator for type"))))

(define (area shape)
  (operate 'area shape))

(define s4
  (make-square 4))

(area s4)
  
