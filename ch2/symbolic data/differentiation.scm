#lang scheme

(define (accumulate op init seq)
  (if (null? seq) init
  (op (car seq) (accumulate op init (cdr seq)))))

(define (full-eq? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((or (number? a) (number? b) (variable? a) (variable? b)) (eq? a b))
        ((and (pair? (car a)) (pair? (car b)))
         (and (full-eq? (car a) (car b))
              (full-eq? (cdr a) (cdr b))))
        ((or (pair? (car a)) (pair? (car b))) #f)
        (else (full-eq? (cdr a) (cdr b)))))
;; atomic building blocks for representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s)) ;; second item of sum list

(define (augend s) (caddr s)) ;; third item of sum list

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p)) ;; second item of product list

(define (multiplicand p) (caddr p)) ;; third item of product list

;; make sum and product for breaking down expressions
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (variable? m1) (variable? m2) (eq? m1 m2)) (list '** m1 2))
        (else (list '* m1 m2))))

;; improved deriv with more precise make-sum and make-product
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum
;          (deriv (addend exp) var)
;          (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp) var) (multiplicand exp))))
;        (else
;         (error "unknown expression type: DERIV" exp))))

;; 2.56: Implement power rule in deriv proc
(define (** x y)
  (define (power a n)
    (if (= n 0) a
        (power (* a x) (- n 1))))
  (power 1 y))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation a b)
  (cond ((=number? a 0) 0)
        ((=number? b 1) a)
        ((or (=number? b 0) (=number? a 1)) 1)
        ((and (number? a) (number? b)) (** a b))
        (else (list '** a b))))

;; deriv with power rule
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum
          (deriv (addend exp) var)
          (if (null? (cdddr exp))
              (deriv (augend exp) var)
              (deriv (cons '+ (cddr exp)) var))))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (if (eq? (base exp) var)
             (make-product
              (exponent exp)
              (make-exponentiation (base exp) (make-sum (exponent exp) (- 1))))
             0))
        (else
         (error "unknown expression type: DERIV" exp))))

;; 2.58: Update sum and product predicates if expressions 
;;       were represented normally with infix, not prefix, notation

;; a: Develop predicates assuming all expressions are pairs
(define (addend-infix exp) (car exp))
(define (multiplier-infix exp) (car exp))

(define (sum-infix? exp)
  (eq? (cadr exp) '+))
(define (product-infix? exp)
  (eq? (cadr exp) '*))

;; for assuming expressions are pairs, easy to use 2 args as building blocks
(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)))
        ((full-eq? a1 a2) (list 2 '* a2))
        (else (list a1 '+ a2))))

;((and (pair? (cdr a1)) (pair? (cdr a2)) (product-infix? a1) (product-infix? a2))
;         (make-sum-infix
;          (make-product-infix (car a1) (cddr a1))
;          (make-product-infix (car a2) (cddr a2))))
;        ((and (pair? a1) (product-infix? a1)) (list (make-product-infix (car a1) (cddr a1)) '+ a2))
;        ((and (pair? a2) (product-infix? a2)) (list (make-product-infix (car a2) (cddr a2)) '+ a1))
        
;; b: Normal expression representation ie. (x * 2 + 2 * x + 4 + ...)        
  
(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((null? (cdr exp)) (deriv-infix (car exp) var))
        ((sum-infix? exp)
         (make-sum-infix
          (deriv-infix (addend-infix exp) var)
          (if (and (pair? (cddr exp)) (product-infix? (cddr exp)))
              (deriv-infix
               (make-product-infix
                (multiplicand exp)
                (cddddr exp))
               var)
              (deriv-infix (cddr exp) var))))
        ((product-infix? exp)
          (make-sum-infix
           (make-product-infix
            (deriv-infix (multiplier-infix exp) var)
            (multiplicand exp))
           (make-product-infix
            (deriv-infix (multiplicand exp) var)
            (multiplier-infix exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv-infix '(x + y + x * y) 'x)