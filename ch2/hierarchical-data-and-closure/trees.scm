#lang scheme

(define (make-tree left right)
  (cons left right))

(define (datum tree)
  (car tree))

(define (children tree)
  (cdr tree))

;; map-like procedure for trees where each node of tree has a datum
(define (tree-map fn tree)
  (make-tree (fn (datum tree))
             (map (lambda (node) (tree-map fn node))
                  (children tree))))

(define tree '(1 (2 (3) (4)) (5 (6) (7) (8))))

(define square (lambda (x) (* x x)))
(tree-map square tree)

(define (forest-map fn forest)
  (map (lambda (tree) (tree-map fn tree)) forest))

(forest-map (lambda (x) (+ x x)) (list tree tree tree))

;; abstractions for words and sentences
(define (word? cand)
  (or (symbol? cand) (number? cand)))

(define beatles '((john lennon) (paul mccartney) (george harrison) (ringo starr)))

(define (deep-map fn lsts)
  (if (word? lsts) (fn lsts)
      (map (lambda (element) (deep-map fn element)) lsts)))

(deep-map (lambda (name) 'beatles) beatles)
  