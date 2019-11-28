#lang scheme

(define %
  (lambda (x y) (modulo x y))) 
;; some practice problems from sicp
;; first few problems for ch2 are in other files like make-rat

(define (length lst)
  (define (len l count)
    (if (equal? null l)
        count
        (len (cdr l) (+ count 1))))
  (len lst 0))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (appnd l1 l2)
  (if (not (pair? l1))
      (cons l1 l2)
      (cons (car l1) (append (cdr l1) l2))))

;; 2.17: last-pair returns last element in list
(define (last-pair lst)
  (let ((len (length lst)))
    (define (l-p l count)
      (if (= (+ count 1) len)
          (car l)
          (l-p (cdr l) (+ count 1))))
  (l-p lst 0)))

;; 2.18: reverse takes a list as arg and returns it backwards
(define (reverse lst)
  (if (not (pair? lst))
      lst
      (append (reverse (cdr lst)) (list (car lst)))))
  
;; 2.19: counting change program that counts ways to make change for coins
;;       given list
;; Note: we don't need to define the helper functions the problem asked for
;;       since we already have length

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= (length coins) 0)) 0)
        (else (+ (cc amount (cdr coins))
                 (cc (- amount (car coins)) coins)))))

(define us-coins (list 1 5 10 25 50)) ;; order of list doesn't matter
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins) ;; should be 292

;; 2.20: same-parity should return a list of numbers with same parity as first entry
(define (same-parity x . y)
  (let ((remainder (% x 2)))
    (define (par lst)
      (cond ((null? lst) lst)
            ((= remainder (% (car lst) 2))
               (cons (car lst) (par (cdr lst))))
            (else (par (cdr lst)))))
    (cons x (par y))))
        
(same-parity 1 2 3 4 5 6 7 8 9 10)
(same-parity 0 1 2 3 4 5 6 7 8 9)

;; map
(define (map proc lst)
  (if (null? lst) lst
      (cons (proc (car lst)) (map proc (cdr lst)))))

;; using map, we can pass lambda procedures as arguments that pass in a list as an arg
(define (scale-list lst factor)
  (map (lambda (x) (* factor x)) lst))

(scale-list (list 1 2 3 4 5) 10)

;; 2.21: square-list
;; w/ map
(define (square-list lst)
  (map (lambda (x) (* x x)) lst))

;; w/o map
(define (sq-list lst)
  (if (null? lst) lst
      (cons (* (car lst) (car lst)) (sq-list (cdr lst)))))

;(square-list (list 1 2 3 4))
;(sq-list (list 1 2 3 4))



;; 2.22: why is this function definition of square-things backwards?
(define square (lambda (x) (* x x)))

(define (s-l items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items null))
;; since we are building up answer as the list we want to return, each iteration, the list
;; is passed as the car to what we build up so we have multiple layers of pairs as our answer

;; for the backwards version where iter returns (cons (square (car things)) answer),
;; we are prepending with each iteration the car of the cdr we passed in before

;(s-l (list 1 2 3 4))

;; 2.23: for-each isn't quite like map b/c it doesn't return a list, but applies a function
;;       to each element in a list
(define (for-each f l)
  (if (null? l)
      true
      ( (lambda (x) (f (car l))) (for-each f (cdr l)) )))

(for-each (lambda (x) (display x) (newline)) (list 57 321 88))

;; 2.24: (list 1 (list 2 (list 3 4))) (1 (2 (3 4))) (1 . --> (2 . --> (3 4)

;; 2.25: Get 7 with cars and cdrs
;; a) (1 3 (5 7) 9): cdaddr, (cdr (car (cdr (cdr (list 1 3 (cons 5 7) 9)))))
;; b) ((7)): caar, (car (car (list (list 7))))
;; c) (1 (2 (3 (4 (5 (6 7)))))): cadadadadadadr,
;;        (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
;;            (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))


;; 2.26: (define x (list 1 2 3)), (define y (list 4 5 6))
;; a) (append x y) --> (1 2 3 4 5 6)
;; b) (cons x y) --> ((1 2 3) 4 5 6)
;; c) (list x y) --> ((1 2 3) (4 5 6))

;; 2.27: deep-reverse: reverse all the sublists of the list passed as an argument
(define (deep-reverse l)
  (cond ((null? l) '())
        ((list? l) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
        (else l)))
(deep-reverse (list (list 1 2) (list 3 4)))

;; 2.28: fringe: flattens a list
(define (fringe lst)
  (define (flatten l)
    (if (not (pair? l) ) l
      (appnd (flatten (car l)) (flatten (cdr l))))) ;; diff append implementation, appnd
  (define (remove-nulls li)
    (cond ((null? li) null)
          (else (cons (car li) (remove-nulls (cdr li))))))
  (remove-nulls (flatten lst)))
                      
(define (fringe2 items)
  (define (iter items acc)
    (if (null? items)
        acc
        (let ((head (car items))
              (tail (cdr items)))
          (if (list? head)
              (iter tail (append acc (fringe2 head)))
              (iter tail (append acc (list head)))))))
(iter items '()))

(define nil (quote ()))
 
(define (fringe3 x)
  (cond ((null? x) nil)
        ((pair? x) (append (fringe3 (car x)) (fringe3 (cdr x))))
        (else (list x))))

(define x (list 1 2 3))
(fringe3 (list x x))

(fringe3 (deep-reverse (list (list x x) (list x x))))

;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
        (list length structure))

;; a) selectors for components of mobile and branch
(define (left-branch mobile) (car mobile))
 
(define (right-branch mobile) (cadr mobile))
 
(define (branch-length branch) (car branch))
 
(define (branch-structure branch) (cadr branch))

;; b) total-weight of a mobile -- I wasn't sure if the definition had the weights on the right side or
;;                                if every number at the end was a weight (it said (len wt) for each)
;;    Therefore, the first definition only adds up the all the right-values in each tree and the second
;;               adds up all values.
(define (total-weight tree)
  (cond ((null? tree) 0)
        ((pair? tree)
              (cond ((null? (cdr tree)) (total-weight (car tree)))
                    ((number? (cadr tree)) (cadr tree))                   
                    (else (+ (total-weight (car tree)) (total-weight (cdr tree))))))
        (else 0)))

;; i) It seems like I didn't use selectors for the abstraction of the tree, so I'll use the selectors in a)
;;    to replicate my function above and make it mroe elegant.

(define (mobile? tree)
  (pair? tree))

(define (weight? tree)
  (number? (branch-structure tree)))

(define (empty? tree)
  (null? tree))

(define (total-weight-abstract tree)
  (cond ((empty? tree) 0)
        ((mobile? tree)
              (cond ((weight? tree) (branch-structure tree))                    
                    (else (+ (total-weight (left-branch tree))
                             (total-weight (right-branch tree))))))
        (else 0)))


;; testing normal and abstract definitions:

(define tree (make-mobile (make-branch 3 (make-branch 1 2)) (make-branch 4 5)))
(define big-tree (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
(define mobile1
  (make-mobile (make-branch 2 4)
               (make-branch 3
                            (make-mobile (make-branch 2 1)
                                         (make-branch 4 2)))))

(total-weight tree)
(total-weight big-tree)

(total-weight-abstract tree)
(total-weight-abstract mobile1)

;; b extra) Adds up all numbered leaves though I'm pretty sure this isn't the right definition
(define (all-leaves tree)
  (cond ((null? tree) tree)
        ((pair? tree)
         (cond ((null? (cdr tree)) (all-leaves (car tree)))
               (else (+ (all-leaves (car tree)) (all-leaves (cdr tree))))))
        (else tree)))



;; c) balanced?: len * wt of top left is equal to that of the right side and all of submobiles are too
;;               this question assumes binary mobiles -- as in two branches
;;     /\
;;    /  \
;;   /\  /\
;;  /\/\/\/\
;; a bcdefg h --> (and (= (* a b) (* g h)) (= (* c d) (* e f))
(define (len-branch tree)
    (if (not (mobile? tree))
        tree
        (len-branch (left-branch tree))))
(define (wt-branch tree)
    (if (not (mobile? tree))
        tree
        (wt-branch (right-branch tree))))

(define (balanced? tree)
  (cond ((empty? tree) 1)
        ((mobile? tree)
         (let ((l (left-branch tree))) ((r (right-branch tree))))
           
           (= (* (balanced? (left-branch tree)) (balanced? (right-branch tree)))
              (* (balanced? (left-branch tree)) (balanced? (right-branch tree)))))
        (else tree)))
               
(define balanced-tree1 (list (list (list 1 2) (list 3 4)) (list (list 3 4) (list 1 2))))

(balanced? balanced-tree1)


    
          
          
                   
                    
            
