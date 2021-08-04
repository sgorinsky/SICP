#lang scheme

(define (mlast-pair mlist)
  (cond ((null? mlist) (error "mutable list is null, must have at least 1 element"))
        ((null? (mcdr mlist)) mlist)
        (else (mlast-pair (mcdr mlist)))))

; mutable list structure
(define (mlist . lst)
  (define (helper hlst)
    (if (null? hlst) hlst
        (mcons
         (car hlst)
         (helper (cdr hlst)))))
  (helper lst))

;; basic example of mutating cdr of 
(define x-mut (mcons 'a (mcons 'b '{}))) ; {a b}
(define y-mut (mlist 'c 'd)) ; {c d}

; proc mutates cdr of last-pair to point to l2
(define (append! mut-l1 mut-l2)
  (set-mcdr!
   (mlast-pair mut-l1)
   (if (mpair? mut-l2) mut-l2 (mlist mut-l2))))

(append! x-mut y-mut) ; {a b c d}

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

;; 3.16: Why is count-pairs proc below wrong? Make 3 pairs where proc
;;       return 3, 4, 7, and doesn't return anything at all

;; wrong b/c it assumes list is pair of nums
;; if car isn't atom, makes recursive call and adds one erroneously
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x)) 1)))

; 3
(count-pairs (list 'a 'b 'c))
; -> [   ][   ] -> [   ][   ] -> [   ][ / ]
;      |             |             |
;      v             v             v
;      a             b             c

; 4
(define a (cons 'foo '()))
(count-pairs (list (cons a a)))
; -> [   ][ / ]
;      |    
;      v
;    [   ][ / ]
;      |    |
;      v    v ; points to a, not cdr a
;    [   ][ / ]
;      |
;      v
;     foo

; 7
(define b (cons a a))
(count-pairs (cons b b))
; -> [   ][   ]
;      |    |
;      v    v
;    [   ][   ]
;      |    |
;      v    v
;    [   ][ / ]
;      |
;      v
;     foo


; infinite loop
;; (set-mcdr! (mlast-pair? mlist) (mcar mlist))
;; (count-pairs mlist)

;; 3.17: Devise correct count-pairs

(define (has-seen? entry seen)
  (cond ((null? seen) #f)
        ((eq? entry (mcar seen)) #t)
        (else (has-seen? entry (mcdr seen)))))

(define (count-pairs-correct l)
  (let ((seen (mlist 'ZZZ)))
    (define (iter lst)
      (cond ((or (null? lst) (not (pair? lst))) 0)
            ((not (has-seen? lst seen))
             (begin
               (append! seen lst)
               (+ (iter (car lst))
                  (iter (cdr lst)) 1)))
            (else
             (+ (iter (cdr lst)) 0))))
    (iter l)))

(count-pairs-correct (list a a)) ; 3
(count-pairs-correct (cons b b)) ; 3

;; 3.18: Detect cycle in mutable list
(define has-cycle?
  (let ((seen (mlist 'ZZZ)))
    (lambda (mlst)
      (cond ((or (null? mlst) (not (mpair? mlst))) #f)
            ((has-seen? mlst seen) #t)
            (else (begin
                    (append! seen (mlist mlst))
                    (or (has-cycle? (mcar mlst))
                        (has-cycle? (mcdr mlst)))))))))

(define circular
  (let ((circ (mlist 1 2 3 4)))
    (append! circ circ)
    circ))

(has-cycle? circular)
        