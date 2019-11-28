#lang scheme

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (length items)
  (define (length-iter a count)
    (if (null? a) count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair x)
  (cond ((null? x) x)
        ((= (length x) 1) (car x))
        (else (last-pair (cdr x)))))

(define (reverse x)
  (if (not (pair? x)) x
      (append (reverse (cdr x)) (list (car x)))))

;; if you replace list in the recursive call with append, you return a flattened list
;; for some reason, this function expects every element to be a list
;;     --> it was because the original function had a recursive call to reverse
;;         which expected the (= (len list) 1) for its base case
(define (deep-reverse lst)
  (if (not (pair? lst)) lst
  (cons (deep-reverse (cdr lst)) (deep-reverse (car lst)))))

