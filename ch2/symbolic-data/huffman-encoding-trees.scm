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
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
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
;;  /  \ 
;; A  /  \ 
;;   B  /  \
;;     D    C
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(displayln (list "decoding sample-message with sample-tree:" (decode sample-message sample-tree)))

;; 2.68: Write encode-symbol, a proc to encode the encoding-tree traversals according
;;       to the following encode proc

;; Trees have structure:
;;         /              |              \
;; (leaf, sym, wt)  rest-of-tree     symbol-set
(define (symbol-set tree)
  (caddr tree))

(define (encode-symbol symbol tree)
  (let ((symbols (symbol-set tree)))
    (define (enc-sym set)
      (cond ((or (null? set) (null? (cdr set))) '())
            ((eq? symbol (car set)) (list 0))
            (else (cons 1 (enc-sym (cdr set))))))
    (enc-sym symbols)))
    

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

;; 2.70: Generate huffman-tree and encode lyrics of rock song using symbol alphabet
; Made sure capitalizations of symbols in alphabet matched lyrics
(define rock-alphabet '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9)))

; Huffman-encoding-tree
(define rock-huffman-tree (generate-huffman-tree rock-alphabet))

; Message
(define rock-lyrics
  '(
    Get a job
        Sha na na na na na na na na
        Get a job
        Sha na na na na na na na na
        Wah yip yip yip yip yip yip yip yip yip
        Sha boom
        ))
(define encoded-lyrics (encode rock-lyrics rock-huffman-tree))
(displayln (list "number of bits variable length:"
                 (length encoded-lyrics)
                 "\nnumber of bits fixed length:"
                 (* (length rock-lyrics) (log (length rock-alphabet) 2))))


;; 2.71: Suppose alphabet of 1...n symbols and frequencies of 1...2^(n-1)
;; How many bits required to encode most frequent symbol? Least frequent?

;; If we have the same kind of weighted tree as previous questions, 1 bit to encode
;;    the most frequent symbol.
;; For the least frequent symbol, log(2^(n-1)) bits.

;; 2.72: Order of growth in encode-symbol proc is O(N)

;; Since we use a weighted tree, there are about n/2 levels
;;    In the worst case, we encode a message that takes n/2 time for a message of length
;;    n. So that would suggest an O(n**2) but if we consider the average case where we access
;;    elements that are near the top of the tree more often, like in the unique case of
;;    elements having 1...2^(n-1) frequencies, we only need to find the general proportion
;;    of symbol frequency to total symbol appearances (sum(1...2^(n-1))) and then sum the
;;    corresponding time it takes to traverse the huffman-tree in order to find the element.
;;    In the case of the most frequently occurring symbol, it only takes O(c) time since
;;    it is the first left-branch we need to access.