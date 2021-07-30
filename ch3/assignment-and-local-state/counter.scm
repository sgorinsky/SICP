#lang scheme

(define count
  (let ((result 0))
    (lambda ()
      (set! result (+ result 1))
      result)))

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

(define sayer
  (lambda (method sentence)
    (cond ((eq? method 'say) (list sentence sentence))
          (else (error "No method for type")))))

(define class-counter
  (let ((class-count 0)) ;; class variable 
    (lambda (instantiate)
      (if (not (eq? instantiate 'instantiate))
          (error "Unrecognizable arg")
          (let ((count 0)) ;; instance variable
            (lambda (message)
              (cond ((eq? message 'add-both)
                     (set! class-count (+ class-count 1))
                     (set! count (+ count 1))
                     (list count class-count))
                    ((eq? message 'add)
                     (set! count (+ count 1))
                     (list count class-count))
                    ((eq? message 'add-class)
                     (set! class-count (+ class-count 1))
                     (list count class-count))
                    (else (error "No proc for type message")))))))))       