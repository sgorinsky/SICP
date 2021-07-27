#lang scheme

;; create some procedure that manages some internal state of proc instantiations
(define counter
  (let ((count 0))
    (lambda (message)
      (cond ((eq? 'add message) (set! count (+ count 1)) count)
            ((eq? 'subtract message) (set! count (- count 1)) count)
            ((eq? 'show message)
                  (display count)
                  (newline))
            (else (error "No proc for type message"))))))

(define c1 counter)
(c1 'add)
(c1 'add)
(c1 'add)
(c1 'subtract)
