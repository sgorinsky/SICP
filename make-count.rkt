#lang scheme

(define %
    (lambda (x y) (modulo x y)))

;; this is a reference to the environment model

(define make-count ;; global env
  (let ((glob 0)) ;; glob env, points to parent (global), where glob starts at 0
    (lambda ()    ;; body of let expression pointing to glob env
      (let ((loc 0)) 
        (lambda () 
          (set! loc (+ loc 1)) ;; body of procedure in loc env
          (set! glob (+ glob 1))
          (list loc glob))))))


;; let's have a message-passing implementation of make-count
(define make-ct
  (let ((glob 0))
    (lambda ()
      (let ((loc 0))
        (lambda (msg)
          (cond ((eq? msg 'glob)
                 (lambda ()
                   (set! glob (+ glob 1))
                   glob))
                ((or (eq? msg 'local) (eq? msg 'loc))
                 (lambda ()
                   (set! loc (+ loc 1))
                   loc))
                (else (error "Unknown message"))))))))

;; we can make a sub-class that extends make-count using message passing
;; works when we pass 'local to it, calls the parent procedure that references
;;     make-ct, the message-passing version of make-count
(define make-buzzer
  (lambda ()
    (let ((parent (make-ct)))
      (lambda (msg)
        (cond ((eq? msg 'local)
            (lambda ()
              (let ((result ((parent 'local)) ))
                (if (= (% result 7) 0)
                    'buzz
                    result))))
            (else (parent msg)) )))))
                


;; testing out make-count

(define c1 (make-count))
(define c2 (make-count))
(c1)
(c1)
(c1)
(c2)
(c2)

;; we can also define another environment extending c2
(define a c2)
'a
(a)
'c2
(c2)
;; since a references c2, it changes c2's loc and glob since all
;; instantiations of make-count create environments that point to glob's procedure
      