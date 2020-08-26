#lang scheme
(require "sets.scm")

;; Huffman-encoding-tree primitives

;; leaves
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; Extracting left/right branches, weights, and symbols from nodes in huffman-tree
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree) (if (leaf? tree)
(weight-leaf tree) (cadddr tree)))

;; Union of two sets at two given nodes
(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; Decoding algorithm
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)) (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; Sets of symbols and associated weights
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair) (cadr pair))
         (make-leaf-set (cdr pairs))))))

;; 2.67: Define an encoding tree and a sample message
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(displayln (list "decoding sample-message with sample-tree:" (decode sample-message sample-tree)))

;; 2.68: Write encode-symbol, a proc to encode the encoding-tree traversals according
;;       to the following encode proc

(define (symbol-set tree)
  (caddr tree))

(define (encode-symbol symbol tree)
  (let ((symbols (symbol-set tree)))
    (cond ((or (null? symbols) (null? (cdr symbols))) '())
          ((and (= 2 (length symbols)) (eq? symbol (cadr symbols))) (list 1))
          ((eq? symbol (car symbols)) (list 0))
          (else (cons 1 (encode-symbol symbol (right-branch tree)))))))

(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(displayln
 (list "encode-symbol proc should return original bit-message encoding of"
       sample-message "\n"
       "from" (decode sample-message sample-tree) "\n"
       "to" (encode (decode sample-message sample-tree) sample-tree)))

(displayln (list "is encoding equal to original bit-message?"
                 (if
                  (equal?
                   sample-message
                   (encode (decode sample-message sample-tree) sample-tree))
                  "yes"
                  "no")))

;; 2.69: design successive-merge using make-code-tree for generate-huffman-tree
(define pairs '((A 4) (C 1) (D 1) (B 2)))

(define (successive-merge leaves)
  (define (create-tree tree set)
    (if (null? set) tree
        (create-tree (make-code-tree (car set) tree) (cdr set))))
  (create-tree (car leaves) (cdr leaves)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(displayln (list "Generate huffman tree from pairs" pairs "\n"
                 (generate-huffman-tree pairs)))