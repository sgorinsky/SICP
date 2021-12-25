#lang sicp

; amb is a special form that "ambiguously" returns a value
; ie. (list (amb 1 2 3) (amb 'a 'b)) has 6 different possibilities

; if predicate is not satisfied, search amb tree
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
(amb n (an-integer-starting-from (+ n 1))))

;; 4.35: an-integer-between
(define (an-integer-between begin end)
  (if (= begin end) end
  (amb begin (an-integer-between (+ begin 1) end))))

; appends to amb tree a bunch of pythagorean triples
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k))) (list i j k)))))


;; 4.36: Explain why simply replacing an-integer-between with an-integer-starting-from doesn't work and
;;       reimplement a-pythagorean-triple-starting-from

; Doesn't work because there is no termination for k, it checks infinite triples
; Thus, we can increment i infinitely but ensure we have some termination conditions for j and k
;    so we aren't checking bad pairs forever
(define (all-pythagorean-triples-starting-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-between (+ i 1) (* i 2))))
      (let ((k (an-integer-between (+ j 1) (* j 2))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; retrying to find all infinite pairs of pythag triples
(define try-again
  (lambda () (amb)))

;; 4.37: Is Ben Bitdiddle's implementation of a-pythagorean-triple-between more efficient than the one in 4.35?

; Yes, it actually is, because it prunes approaches k-high when the sum of two squares, i^2 + j^2, exceeds
;    the square of the hypotenuse
(define (ben-a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k)) (list i j k))))))
  