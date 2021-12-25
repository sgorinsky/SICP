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

;; 4.39: Does order of restrictions matter for runtime of application?

; Yes, distinct is by far the longest proc to run since it compares each element against all others, so if all the other restrictions
;    make it through, distinct should be the last check

;; 4.40: How many assignments of people to floors are there both before and after the distinct? check? Write a more efficient
;;       nondeterministic multiple-dwelling proc based upon possibilities that are not already ruled out by previous restrictions.

; There are 5 for each of the 5 people so there are 5^5 combinations,
;    but we only care about the ones that meet the distinct conditions
; After the distinct? check, there are 5! possible combinations since one person cannot occupy another person's floor

; I could either nest lets to include requires that prune tree or I could just include conditions in the initializations for
;    each person's set of possibilities in amb
(define (effictient-multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 4))
        (fletcher (amb 2 4))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (distinct? (list baker cooper fletcher miller smith)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 4.41: Write an ordinary scheme proc to solve the multiple-dwelling problem
(define (ordinary-multiple-dwelling)
  (define (remove x s)
    (cond ((null? s) '())
          ((eq? x (car s)) (remove x (cdr s)))
          (else (cons (car s) (remove x (cdr s))))))

  (define (permutations s)
    (if (null? s) (list '())
        (reduce
         append
         '() ;; flatmap
         (map (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
              s))))

  (define (check-cands perms-list)
    (if (null? perms-list) '()
        (let ((perm (car perms-list)))
          (let ((baker (car perm)) (cooper (cadr perm)) (fletcher (caddr perm)) (miller (cadddr perm)) (smith (cadddr (cdr perm))))
            (if (or (= baker 5) (= cooper 1)
                    (= fletcher 5) (= fletcher 1)
                    (<= miller cooper)
                    (= (abs (- fletcher cooper)) 1)
                    (= (abs (- smith fletcher)) 1)
                    (not (distinct? (list baker cooper fletcher miller smith))))
                (check-cands (cdr perms-list))
                (append (list
                         (list 'baker baker)
                         (list 'cooper cooper)
                         (list 'fletcher fletcher)
                         (list 'miller miller)
                         (list 'smith smith))
                        (check-cands (cdr perms-list))))))))

  (check-cands (permutations (list 1 2 3 4 5))))
  
    
;; 4.42: Write program to solve liars puzzle
(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))

    (require (or (and (not (= betty 3)) (= kitty 2))
                 (and (= betty 3) (not (= kitty 2)))))
    (require (or (and (not (= ethel 1)) (= joan 2))
                 (and (= ethel 1) (not (= joan 2)))))
    (require (or (and (not (= joan 3)) (= ethel 5))
                 (and (= joan 3) (not (= ethel 5)))))
    (require (or (and (not (= kitty 2)) (= mary 4))
                 (and (= kitty 2) (not (= mary 4)))))
    (require (or (and (not (= mary 4)) (= betty 1))
                 (and (= mary 4) (not (= betty 1)))))
    (require (distinct? (list betty ethel joan kitty mary)))

    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
             
  