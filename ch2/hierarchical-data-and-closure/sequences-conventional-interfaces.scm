#lang scheme

(define mod
  ;; only takes first 2 args
  (lambda (x y) (modulo x y)))

;; own implementation of remove
(define (rm n lst)
  (cond ((null? lst) null)
        ((= n (car lst)) (cdr lst))
        (else (cons (car lst) (rm n (cdr lst))))))

;; traversing a list to go to the nth element
(define (go-through-list a n)
    (define (gta a num k)
      (if (= num k) (car a)
          (gta (cdr a) (+ num 1) k)))
    (gta a 1 n))

(define a (list 1 2 3 4 5))

(define (fib n)
  (define (f count k-th n-th iter)
    (if (> count n-th) iter
        (f (+ count 1) iter n-th (+ iter k-th))))
  (f 0 1 n 0))

(define (enumerate-interval low high)
  (if (> low high) null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(display (list 'accumulate: 'product 'of '1-5:
               (accumulate * 1 (list 1 2 3 4 5))))

(define (filter predicate? lst)
  (cond ((null? lst) lst)
        ((predicate? (car lst))
            (cons (car lst) (filter predicate? (cdr lst))))
        (else (filter predicate? (cdr lst)))))
        

(newline)
(display (list 'filter 'odds 'of '1-5: (filter odd? (list 1 2 3 4 5))))   
(newline)

(define square (lambda (x) (* x x)))

;; 2.33: define map, append, and length using accumulate
(define (map p sequence)
  (accumulate (lambda args (cons (p (car args)) (cadr args))) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;; ex of enumerate-filter-map structure
;;     we create a list from 0 to n, filter it with some predicate, map a function
;;     to that result and accumulate it into a list
(define (even-squares n)
    (accumulate cons null (map square (filter even? (enumerate-interval 0 n)))))
(display (list 'Even 'squares 'from '1-10: (even-squares 10)))
(newline)
         
 
;; 2.34: Horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

(display (list 'Horner 'eval: (horner-eval 2 (list 1 3 0 5 0 1))))
(newline)
                
;; 2.35: redefine count-leaves as an accumulation

(define (flatten tree)
  (cond ((number? tree) (list tree))
        ((null? tree) null)
        (else (append (flatten (car tree)) (flatten (cdr tree))))))

(define b (list (list (list 0 1) 2) (list 3 4)))

(display (list 'flattened b '=> (flatten b)))
(newline)

(define (count-leaves tree)
  (accumulate (lambda (x y) (+ (length (flatten x)) y)) 0 tree))

(define (simple-count-leaves seq)
  (length (flatten seq)))


(display (list 'count-leaves: (count-leaves b)))
(newline)

;; 2.36: accumulate with n sublists

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define l (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(display (list 'accumulating 'each 'element 'of 'sublists: (accumulate-n + 0 l)))
(newline)

;; 2.37: representing row vectors as lists and matrices as lists of row vectors
;;       and including vector operations

(define (make-vector . args) args)

;;creating vectors
(define v (make-vector 1 2 3 4))
(define w (make-vector 4 3 2 1))

(define (make-matrix . vectors)
  (if (or (number? (car vectors)) (= 1 (length (car vectors))))
      (error '(matrices must have vectors greater than length 1))
      vectors))

;; creating matrices
(define m (make-matrix v v v v))
(define n (accumulate-n cons null m))

(define transpose
  (lambda (m) (accumulate-n cons null m)))

;; vector and matrix multiplication methods
(define (dot-product v w)
  (if (not (= (length v) (length w))) (error '(v and w different lengths))
      (accumulate + 0 (accumulate-n * 1 (make-vector v w)))))

(define (matrix-*-vector m x)
  (cond ((null? m) null)
        ((not (= (length (car m)) (length x)))
         (error '(matrix and vector different lengths)))
        (else (cons (dot-product (car m) x) (matrix-*-vector (cdr m) x)))))

(define (matrix-*-matrix p q)
  (define (matrix-mult a b)
    (if (or (null? a) (null? b)) null
        (cons (matrix-*-vector a (car b)) (matrix-mult a (cdr b)))))
  (matrix-mult (transpose q) p))
       
(display (list 'm '* 'v '= (matrix-*-vector m v)))
(newline)

(display (list 'm '* 'm '= (matrix-*-matrix m m)))
(newline)

;; 2.38: we want fold-right to produce the same outputs as fold-left given a
;;       certain property of op

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;; if op reverses the order of accumulate for fold-right and we have a reverse
;;    list, then fold-left and fold-right will evaluate to the same result
(fold-left / 1 (list 5 4 3 2 1))
(fold-right (lambda (x y) (/ y x)) 1 (list 5 4 3 2 1))
(accumulate (lambda (x y) (/ y x)) 1 (list 5 4 3 2 1))

(fold-left list null (list 1 2 3))
(fold-right (lambda (x y) (cons x y)) null (list 1 2 3))
(accumulate (lambda (x y) (cons x y)) null (list 1 2 3))

;; 2.38: reverse with fold-left and fold-right
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse (list 1 2 3 4 5))
(reverse-left (list 1 2 3 4 5))


;; doing operations on nested sequences
;; here, we're taking all nested sequences from 1-5, summing them and filtering
;;       the even sums
(define nested
  (map (lambda (x) (enumerate-interval 1 x)) a)) ;; (def a (list 1 2 3 4 5))

(define nested-sum (map (lambda (x) (accumulate + 0 x)) nested))

(display (list "Mapping sum of evens in nested list\nfrom:" nested
               "\nto:" nested-sum "\nto:" (filter even? nested-sum)))
(newline)
;; in full:
(define (nested-even-sum n)
  (filter even?
       (map (lambda (x) (accumulate + 0 x))
            (map (lambda (x) (enumerate-interval 1 x)) (enumerate-interval 1 n)))))

;; let's make nested enumerated pairs from 1 to n
;; this procedure appends enumerated (i, j) pairs after going through 
;;      sub-intervals from 1-i for each i in 1-n and for each j in 1-i
(define (nested-enumerated n)
  (accumulate append null
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 i)))
                   (enumerate-interval 1 n))))

(display (list "Filtered odds of nested-enumerated pairs up until 5\nunfiltered:"
                (nested-enumerated 5)
                "\nfiltered:"
                (filter odd? (map
                              (lambda (x) (accumulate + 0 x))
                              (nested-enumerated 5)))))
(newline)

;; flatmap: we map procedures to elements in a list and then accumulate the elements
;;          through append

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;; permutations
(define (permutations s)
  (if (null? s)
      (list null)
      (accumulate append null ;; flatmap
                  (map (lambda (x)
                         (map (lambda (p) (cons x p))
                              (permutations (remove x s))))
                       s))))

(define (permutations-flatmap s)
  (if (null? s) (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations-flatmap (remove x s)))) s)))

(display (list "Permutations of (1 2 3):" (permutations (list 1 2 3))))
(newline)
(display (list "Permutations with Flatmap of (1 2 3):" (permutations (list 1 2 3))))
(newline)

;; 2.40: unique pairs of (i, j) where 1 <= j < i <= n
;;       basically, what i did for nested-enumerate but it doesn't hurt to practice
(define (unique-pairs n)
  (accumulate append null
              (map (lambda (i)
                     (map (lambda (j) (list j i))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(display (list "Unique pairs of (i, j), where 1 ≤ i < j ≤ 5:"
               (unique-pairs 5)))
(newline)

;; 2.41: all distinct ordered triples 1 ≤ i < j < k ≤ n that sum to s
(define (ordered-triples n)
  (accumulate append null
              (flatmap
               (lambda (i)
                 (map
                  (lambda (j)
                    (map
                     (lambda (k) (list i j k))
                     (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
               (enumerate-interval 1 n))))
              
(define (sum-ordered-triples n s)
  (filter
   (lambda (x)
     (= (accumulate + 0 x) s))
  (ordered-triples n)))
             
(display (list "Ordered triples up to 5:" (ordered-triples 5)
               "\nFiltered sum of ordered triples equal to 9:"
               (sum-ordered-triples 5 9)))

(define ground-list
  (lambda (x)
    (cond ((null? x) null)
          ((null? (cdr x)) (ground-list (car x)))
          (else x))))

(define (ordered-quartet n)
  (accumulate append null
   (accumulate append null
    (flatmap
     (lambda (i)
       (map
        (lambda (j)
          (map
           (lambda (k)
             (map
              (lambda (l)
                (list i j k l))
              (enumerate-interval 1 (- k 1))))
           (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))))

(newline)
(display (list "Ordered quartet up to 5:" (ordered-quartet 5)))

;; merge k sorted lists
(define (merge a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((< (car a) (car b)) (cons (car a) (merge (cdr a) b)))
        (else (cons (car b) (merge (cdr b) a)))))

(define (merge-k-lists lists)
  (accumulate merge '() lists))

(define a-lists (list a a a a a a))

(newline)
(display (list "Combine 5 lists of (1 2 3 4 5): " (merge-k-lists a-lists)))