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
(define (has-cycle? mutable-list)
  (let ((seen (mlist 'ZZZ)))
    (define (helper mlst)
      (cond ((or (null? mlst) (not (mpair? mlst))) #f)
            ((has-seen? mlst seen) #t)
            (else (begin
                    (append! seen (mlist mlst))
                    (or (helper (mcar mlst))
                        (helper (mcdr mlst)))))))
    (helper mutable-list)))

(define circular
  (let ((circ (mlist 1 2 3 4)))
    (append! circ circ)
    circ))

(has-cycle? circular) ; #t

;; 3.19: Rewrite 3.18 with algo that uses constant space
(define (detect-cycle? mlst)
  (define (race ml1 ml2)
    (cond ((or (null? ml1) (not (mpair? ml2))) #f)
          ((eq? ml1 ml2) #t)
          (else (race (mcdr ml1) (mcdr (mcdr ml2))))))
  (if (not (mpair? mlst))
      #f
      (race mlst (mcdr mlst))))

(detect-cycle? circular) ; #t

;; Queues
(define queue
  (let ((q (mlist null)))
    (define front-ptr
      (mcar q))
    (define rear-ptr
      (mcdr q))
    (define (set-front-ptr! item)
      (set-mcar! q item))
    (define (set-rear-ptr! item)
      (set-mcdr! q item))
    (define (is-empty?)
      (null? front-ptr))
    (define (get-front)
      (if (is-empty?)
          (error "No peeking, queue is empty")
          (mcar front-ptr)))
;    (define (get-back)
;      (if (is-empty?)
;          (error "Can't view back, queue is empty")
;          (mcar rear-ptr)))      
    (define (insert-queue! element)
      (let ((item (mlist element)))
        (if (is-empty?)
            (begin
              (set! front-ptr item)
              (set! rear-ptr front-ptr))
            (begin
              (set-mcdr! rear-ptr item)
              (set! rear-ptr item))))) 
    (define (delete-queue!)
      (if (is-empty?)
          (error "Queue is already empty")
          (set! front-ptr (mcdr front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'peek) (get-front))
            ; ((eq? m 'back) (get-back))
            ((eq? m 'poll) (delete-queue!))
            ((eq? m 'push)
             (lambda (item) (insert-queue! item)))
            ((eq? m 'view) front-ptr)
            (else (error "No method for queue"))))
    dispatch))

(define q queue)
((q 'push) 1)
((q 'push) 2)
((q 'push) 3)
(q 'peek)
; (q 'back)
(q 'poll)
(q 'peek)
; (q 'back)
(q 'poll)
(q 'peek)
; (q 'back)
(q 'poll)
; (q 'peek) ; error

;; Book's implementation of queue
(define (front-ptr queue)
  (mcar queue))

(define (rear-ptr queue)
  (mcdr queue))

(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair) (set-rear-ptr! queue new-pair) queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (mcdr (front-ptr queue))) queue)))

;; 3.21: Why would we have to define own print proc to represent queue to scheme interpreter?

(define q1 (make-queue))
(insert-queue! q1 'a)
;((a) a)
(insert-queue! q1 'b)
;((a b) b)
(delete-queue! q1)
;((b) b)
(delete-queue! q1)
;(() b)

;; Interpreter's response looks like item is inserted twice b/c front-ptr references entire list and rear-ptr
;;    references last element, so insertion shows new element at end of front-ptr and car of rear-ptr.
;; And the reason why even the b element "stays" after we "delete" it is b/c we don't actually move rear-ptr
;;     away from it, only the null ptr. Rear-ptr will be readjusted after new insertion.  

(insert-queue! q1 'a)
(insert-queue! q1 'c)

;; In order to view queue the way Ben Bitdiddle wants, new proc print-queue shows only queue from front-ptr
(define (print-queue queue)
  (front-ptr queue))


(define q2 (make-queue))
(insert-queue! q2 'a)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

;; 3.22: Implement queue as object with local state -- answer above in my original queue implementation
;;       but will tighten things up by defining front-ptr and rear-ptr
(define (instantiate-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (is-empty?)
      (null? front-ptr))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (insert element)
      (let ((item (mlist element)))
        (if (is-empty?)
            (begin
              (set-front-ptr! item)
              (set-rear-ptr! front-ptr))
            (begin
              (set-mcdr! rear-ptr item)
              (set-rear-ptr! (mcdr rear-ptr))))))
    (define (delete)
      (if (is-empty?)
          (error "Cannot delete element, queue is empty")
          (set-front-ptr! (mcdr front-ptr))))
    (define (front)
      (mcar front-ptr))
    (define (rear)
      (mcar rear-ptr))
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'insert)
             (lambda (element) (insert element) (print-queue)))
            ((eq? m 'delete)
             (begin (delete) (print-queue)))
            ((eq? m 'peek) (front))
            ((eq? m 'print) (print-queue))
            (else (error "No proc for queue"))))
    dispatch))

(define iq (instantiate-queue))
((iq 'insert) 1)
((iq 'insert) 2)
((iq 'insert) 3)
((iq 'insert) 4)
(iq 'delete)
(iq 'delete)
(iq 'delete)
(iq 'delete)

;; 3.23: Implement double-ended-queue deque w/ insertion and deletion at each end
(define (deque)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (next-ptr ptr)
      (mcdr ptr))
    (define (prev-ptr ptr)
      (mcdr (mcar ptr)))
    
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (set-next-ptr! ptr item)
        (set-mcdr! ptr item))
    (define (set-prev-ptr! ptr item)
      (let ((ptr-pair (mcar ptr)))
        (set-mcdr! ptr-pair item)))
    
    (define (empty-deque?)
      (if (or (null? front-ptr) (null? rear-ptr))
          (begin (set-front-ptr! null) (set-rear-ptr! null) #t)
          #f))
    
    (define (front-deque)
      (if (empty-deque?)
          (error "Deque is empty")
          (mcar (mcar front-ptr))))
    (define (rear-deque)
      (if (empty-deque?)
          (error "Deque is empty")
          (mcar (mcar rear-ptr))))
    (define (get-element ptr)
      (mcar (mcar ptr)))
    
    (define (front-insert-deque! element)
      (let ((item (mlist (mcons element '()))))
        (if (empty-deque?)
            (begin
              (set-front-ptr! item)
              (set-rear-ptr! front-ptr))
            (begin
              (set-next-ptr! item front-ptr)
              (set-prev-ptr! front-ptr item)
              (set-front-ptr! item)))))
    (define (rear-insert-deque! element)
      (let ((item (mlist (mcons element rear-ptr))))
        (if (empty-deque?)
            (begin
              (set-front-ptr! item)
              (set-rear-ptr! front-ptr))
            (begin
              (set-mcdr! rear-ptr item)
              (set-rear-ptr! (mcdr rear-ptr))))))
 

    (define (front-delete-deque!)
      (if (empty-deque?) (error "Deque is empty")
          (let ((next (next-ptr front-ptr)))
            (begin
              (if (not (null? next))
                  (set-prev-ptr! next null)
                  '())
              (set-front-ptr! next)))))

    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "Deque is empty"))
            ((eq? front-ptr rear-ptr) (set-front-ptr! null))
            (else (begin
                    (set-rear-ptr! (prev-ptr rear-ptr))
                    (and (not (empty-deque?)) (set-next-ptr! rear-ptr null))))))
      
          
    (define (pretty-print-deque)
      (define (print ptr)
          (if (null? ptr) '()
              (mcons
               (get-element ptr)
               (print (next-ptr ptr)))))
        (print front-ptr))

    (define (ugly-print-deque)
      front-ptr)
    
    (define (dispatch m)
      (cond ((eq? m 'insert-front)
             (lambda (element) (front-insert-deque! element) (ugly-print-deque)))
            ((eq? m 'insert-rear)
             (lambda (element) (rear-insert-deque! element) (ugly-print-deque)))
            ((eq? m 'delete-front)
             (begin (front-delete-deque!) (ugly-print-deque)))
            ((eq? m 'delete-rear)
             (begin (rear-delete-deque!) (ugly-print-deque)))
            ((eq? m 'front) (front-deque))
            ((eq? m 'rear) (rear-deque))
            ((eq? m 'print) (pretty-print-deque))
            (else (error "No proc for queue"))))
    dispatch))

(define d (deque))
((d 'insert-front) 5)
((d 'insert-front) 4)
((d 'insert-front) 3)
((d 'insert-front) 2)
((d 'insert-rear) 6)
(d 'print)
(d 'front)
(d 'rear)
