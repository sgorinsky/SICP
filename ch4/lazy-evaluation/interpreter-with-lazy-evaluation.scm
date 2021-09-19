#lang sicp

;; application clause of eval becomes:
;((application? exp)
;(apply (actual-value (operator exp) env)
;        (operands exp)
;        env))
;; whenever we need actual value of expression, force eval
(define (actual-value exp env)
  (force-it (eval exp env)))

;; strict and lazy abstractions of list-of-values
(define (list-of-arg-values exps env)
  (if (no-operands? exps) '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps) '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))


;; change apply from prev definitions with list-of-values to include lazy evaluations
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; lazy evaluator prompt
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; thunks - procs whose values are contained in proc and only want when we force them
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (delay-it exp env) (list 'thunk exp env))
;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj)) obj))

; memoized thunk -- since values don't change (in theory), we can keep and call when needed
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '()) ; forget unneeded env ((evaluated-thunk? obj) (thunk-value obj))
         result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

; consider removing procs after thinks evaluated and values stored to avoid excess memory storage
; ie. (set! proc '())

;; 4.27 Suppose we define count and id in lazy interpreter, give missing values and explain
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))
;;; L-Eval input: count
;;; L-Eval value: ⟨response⟩ ; -> 1 ;; w not fully evaluated yet, only inner id called
;;; L-Eval input: w
;;; L-Eval value: ⟨response⟩ ; -> 10 ;; evaluates w and returns 10
;;; L-Eval input: count
;;; L-Eval value: ⟨response⟩ ; -> 2 ;; since (set! count (+ count 1)) in body of id evaluated twice when w was called

;; 4.28: Provide example where actual-value is needed to force proc instead of eval
; Needed whenever a thunk hasn't been evaluated yet so if a proc is passed as a param into another function, then it will be
;    represented as a thunk
; ie. (define (f g h x) (g h x))

;; 4.29: Offer example program that would run much faster with memoization than without
(define (fib n)
  ((cond ((= k 0) 0)
         ((= k 1) 1)
         (else (+ (fib (- k 1)) (fib (- k 2)))))))

; offer outputs for when following program memoizes and doesn't

; memo
; (define (square x) (* x x)) -> (* (force-it (id 10)) (force-it (id 10))) -> (* (unmemoized (id 10)) (memoized (id 10)))

;;; L-Eval input: (square (id 10))
;;; L-Eval value: ⟨response⟩ -> 100
;;; L-Eval input: count
;;; L-Eval value: ⟨response⟩ -> 1

; no memo
; (define (square x) (* x x)) -> (* (unmemoized (id 10)) (unmemoized (id 10)))
;;; L-Eval input: (square (id 10))
;;; L-Eval value: ⟨response⟩ -> 100
;;; L-Eval input: count
;;; L-Eval value: ⟨response⟩ -> 2

;; 4.30: Cy provides defintion of eval-sequence b/c he's worried no side effects will take place if exps aren't forced
;(define (eval-sequence exps env)
;  (cond ((last-exp? exps) (eval (first-exp exps) env))
;        (else (actual-value (first-exp exps) env) 
;              (eval-sequence (rest-exps exps) env))))



;(define (for-each proc items)
;  (if (null? items)
;      'done
;      (begin (proc (car items))
;             (for-each proc (cdr items)))))

;(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

; a. The force-it in actual-value is redundant in Bitdiddle's for-each where each element is displayed b/c display is a primitive
;    proc that prints to stdout and doesn't return anything

; b. Cy creates two procs. What are the outputs like with original eval-sequence and Cy's eval-sequence?

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(p1 1)
; original -> (1 (2)) 
; Cy's -> (1 (2))


(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p2 1)
; original -> (1 (2)) ; sets on the way to returning x since set! exp is evaluated
; Cy's -> 1 ; b/c it doesn't force set! expression and just returns x

; c. Changing eval-sequence does not affect behavior of for-each in a. Why?
; Because if eval-sequence evaluates forces something that isn't a thunk, it just returns the value of that evaluated expression
;    and doesn't force it.

; d. Sequences in the evaluator ought to be treated with Cy's approach b/c we should only expect the last expression to be a thunk
;        if necessary, not everything in between b/c then definitions, for instance, won't get properly evaluated.