#lang scheme

;; we're trying to create a procedure that takes in a message and returns a value

;; in this case, we can make a procedure that accepts a message and
;; returns a value associated with that message (ie. 'area or 'perimeter)

(define (make-square side)
  (lambda (message)
    (cond ((eq? message 'area)
           (* side side))
          ((eq? message 'perimeter)
           (* side 4))
          (else (error "Unknown message")))))

(define (make-circle radius)
  (lambda (message)
    (cond ((eq? message 'area)
           (* pi radius radius))
          ((eq? message 'perimeter)
           (* 2 pi radius))
          (else (error "Unknown message")))))

;; message works like this:
(define (operate op obj)
  (obj op))

(define (area shape)
  (operate 'area shape))

;; we can make a square procedure object and pass a message to it
((make-square 5) 'area)

;; or we can just do an operation on an object by passing the message to it
(operate 'area (make-square 5))
