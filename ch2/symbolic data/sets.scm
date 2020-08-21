#lang scheme
(require "quotation.scm" "differentiation.scm")

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


(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

  