#lang scheme

(define count
  (lambda ()
    ((lambda (result)
       (lambda ()
         (set! result (+ result 1))
         result))
     0)))

(define make-count
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))
