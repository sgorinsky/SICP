#lang scheme

(define (pigl word)
  (if (vowel? (list-ref word 0)) (cons word 'ay)
      (pigl (cons (cdr word) (car word))))) ;;<-- bug here

(define (vowel? l)
  (or (eq? l 'a) (eq? l 'e) (eq? l 'i) (eq? l 'o) (eq? l 'u)))

(define (last-pair l)
  (if (empty? l) l
      (list-ref l (- (length l) 1))))

(define (reverse l)
  (if (= 1 (length l)) l
      (cons (reverse (cdr l)) (car l))))

(substring "a fox jumped" 2 5)