#lang scheme

(define (mlast-pair mlist)
  (cond ((null? mlist) (error "mutable list is null, must have at least 1 element"))
        ((null? (mcdr mlist)) mlist)
        (else (mlast-pair (mcdr mlist)))))

;; basic example of mutating cdr of 
(define x-mut (mcons 'a (mcons 'b null))) ; {a b}
(define y-mut (mcons 'c (mcons 'd null))) ; {c d}

; proc mutates cdr of last-pair to point to l2
(define (append! mut-l1 mut-l2)
  (set-mcdr! (mlast-pair mut-l1) mut-l2)) 

(begin
  (append! x-mut y-mut)
  x-mut) ; {a b c d}

;; 3.12: Suppose append! is defined below: explain what happens in <response> from following proc calls
; Note: instead of running procs, will just analyze
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

;; 3.15: What does (set-to-wow! z1) look like?
(define z1 (mcons x-mut x-mut))

(define (set-to-wow! mlist)
  (set-mcar! (mcar mlist) 'wow)
  mlist)

(set-to-wow! z1) ; modifies (caar z1) which is a reference to x-mut so x-mut is modified as well
x-mut