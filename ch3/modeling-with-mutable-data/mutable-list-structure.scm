#lang scheme
(require rnrs/mutable-pairs-6)

;; 3.12: Suppose append! is defined below: explain what happens in <response> from following proc calls

;(define (append! x y)
;  (set-cdr! (last-pair x) y)
;  x)

;(define x (list 'a 'b))
;(define y (list 'c 'd)
;(define z (append x y)) z
;(cdr x) -> ⟨response⟩ --> ('b) ;; not mutated yet

;(define w (append! x y)) ;; mutates last-pair of x '(b) to include y
;(cdr x) -> ⟨response⟩ --> ('b 'c 'd)

;; 3.13: What does following call below do?
;(define (make-cycle x)
;  (set-cdr! (last-pair x) x) x)

;(define z (make-cycle (list 'a 'b 'c)))
; end of list references its car creating cycle:
; --> (list 'a 'b 'c) --> (list 'a 'b 'c) --> ...

;; 3.14: What does mystery proc do?
;(define (mystery x)
;  (define (loop x y)
;    (if (null? x) y
;        (let ((temp (cdr x)))
;          (set-cdr! x y) (loop temp x))))
;  (loop x '()))

;; mystery proc reverses list by recursively taking first element of x, storing it in y, and mutating
;;  cdr x to point to it then calling itself again until x is null