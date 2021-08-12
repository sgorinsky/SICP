#lang sicp

;; make-serializer
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true) false)))

(define (clear! cell) (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell) (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell)))) the-mutex))


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; 3.47: Implement semaphores (n-sized generalizations of mutexes)
; a. Using mutexes
