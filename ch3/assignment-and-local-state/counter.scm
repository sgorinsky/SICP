#lang scheme

;; create some procedure that manages some internal state of proc instantiations
(define counter
  (let ((start 0))
    (lambda (add)
      (display start)
      (newline)
      (set! start (+ start 1)))))

(define c1 counter)
(c1 'add)
(c1 'add)
(c1 'add)