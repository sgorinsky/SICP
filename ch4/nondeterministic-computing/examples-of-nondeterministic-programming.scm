#lang sicp

(define (require p)
  (if (not p) (amb)))

(define (reduce fn base lst)
  (if (null? lst) base
      (fn (car lst) (reduce fn base (cdr lst)))))
   
(define (distinct? elements)
  (define (check element lst)
    (cond ((null? lst) #t)
          ((= element (car lst)) #f)
          (else (check element (cdr lst)))))
  (define (has-false lst)
    (cond ((null? lst) #t)
          ((not (car lst)) #f)
          (else (has-false (cdr lst)))))
  (define (get-truth-values lst)
    (if (null? lst) '()
        (cons (check (car lst) (cdr lst)) (get-truth-values (cdr lst)))))
  (has-false (get-truth-values elements)))
        
        
  
;;; Logic Puzzles
; multiple dwelling puzzle
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 4.38: Modify multiple-dwellings to omit the requirement that smith and fletcher do not live on adjacent floors. How many
;;       different solutions are there now?
(define (updated-multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))