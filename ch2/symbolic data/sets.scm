#lang scheme
;; (require "quotation.scm" "differentiation.scm")

;; Note that these implementations of unordered lists as sets is highly inefficient
(define (element-of-set x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-of-set x (cdr set)))))

(define (adjoin-element x set)
  (if (element-of-set x set)
      set
      (cons x set)))

(define (intersection-set a b)
  (cond ((or (null? a) (null? b)) null)
        ((element-of-set (car a) b) (cons (car a) (intersection-set (cdr a) b)))
        (else (intersection-set (cdr a) b))))

;; 2.59: Union-set
(define (union-set a b)
  (cond ((or (null? a)) b)
        ((element-of-set (car a) b) (union-set (cdr a) b))
        (else (cons (car a) (union-set (cdr a) b)))))

;; 2.60: Design all prior primitives given non-unique elements in set
;; a. element-of-set is same

;; b. adjoin element has no need for element of set checks -> O(N)
(define (adjoin-element-dup x set)
  (cons x set))

;; c. intersection-set is same too since we need to check if (car a)/(car b) is in b/a

;; d. O(N) Union-set without element-of-set checks
;;         Basically an append procedure
(define (union-set-dup a b)
  (if (null? a)
      b
      (cons (car a) (union-set-dup (cdr a) b))))

;; Suppose we have ordered sets, we know quicker when an element is in the set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


;; find intersection with ordered-lists as sets
;;    O(N) approach by recursively calling cdr of list with smaller car
(define (intersection-set-ordered a b)
  (if (or (null? a) (null? b)) '()
      (let ((x (car a)) (y (car b)))
        (cond ((= x y) (cons x (intersection-set-ordered (cdr a) (cdr b))))
              ((< x y) (intersection-set-ordered (cdr a) b))
              (else (intersection-set-ordered a (cdr b)))))))

;; 2.61: adjoin-set-ordered
;;       assuming both sets are ordered lists
(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

;; 2.62: union-set-ordered
(define (union-set-ordered a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else (let ((x (car a)) (y (car b)))
           (cond ((= x y) (cons x (union-set-ordered (cdr a) (cdr b))))
                 ((< x y) (cons x (union-set-ordered (cdr a) b)))
                 (else (cons y (union-set-ordered a (cdr b)))))))))

;; Binary Trees
; Primitives
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x tree)
  (cond ((null? tree)  #f)
        ((number? tree) (equal? x tree))
        ((equal? x (entry tree)) #t)
        ((or (< x (entry tree)) (null? (cddr tree)))
         (element-of-tree-set? x (left-branch tree)))
        (else (element-of-tree-set? x (right-branch tree)))))

(define tree '(5 (3 (2 () ()) (4 () ())) (8 (7 () ()) (9 () ()))))
(displayln (list "Is 6 in tree-set:" (element-of-tree-set? 6 tree)))

(define (adjoin-element-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-element-tree x (left-branch set)) (right-branch set)))
        (else (make-tree (entry set) (left-branch set) (adjoin-element-tree x (right-branch set))))))

(displayln (list "adjoin 6 to tree-set:" "\n" (adjoin-element-tree 6 tree)))

;; 2.63: Two procs for converting trees to lists
(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree) result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
(copy-to-list tree '()))

;; a: Are the two procedures for converting trees to lists the same?
;  -> Proc 1 builds up a list by appending the resulting list from left recursive calls
;         to right resulting list. Append call iterates through list at each step
;         and is expensive. N recursive steps with up to N calls (len of left-branch)
;         from append
;  -> Proc 2 traverses tree and only concats prev entry trees to new entry trees
;         saving extra time taken to append. 