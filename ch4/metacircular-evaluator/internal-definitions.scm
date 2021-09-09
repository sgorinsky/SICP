#lang sicp

;; 4.16: Implement method for interpreting internal definitions
; a. Change lookup-variable-value in 4.1.3 to signal error if it is symbol *unassigned*

; env in this def is pair of lists
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals)) ; signal error if *unassigned*
                 (error "Unassigned var: "(car vals))
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; env in this def is list of pairs
(define (lookup-var-val var env)
  (let ((pair (search-envs var env)))
    (if pair
        (if
         (eq? '*unassigned* (cadr pair)) ; *unassigned* error
         (error "Unassigned var: " (cadr pair))
         pair)
        (error "Unbound global var: " var))))

; b. Write a proc scan-out-defines that transforms a proc body into one w/o internal proc defines as laid out in 4.1.6

; Transform:
;(lambda ⟨vars⟩
;  (define u ⟨e1⟩)
;  (define v ⟨e2⟩)
;  ⟨e3⟩)

; into:
;(lambda ⟨vars⟩
;  (let ((u '*unassigned*)
;        (v '*unassigned*))
;    (set! u ⟨e1⟩)
;    (set! v ⟨e2⟩)
;    ⟨e3⟩))

; assume assignment, define, lambda, let procs from representing-expressions 4.1.2
; scan-out-defines: create a let expression where the define vars are created as params in a let expression and the let's body
;                   has the other expressions appended to the list of assignments of the define-vars to define-vals
(define (scan-out-defines proc-body)
  (let ((defines (filter define? proc-body))
        (no-defines (filter (lambda (exp) (not (define? exp))) proc-body)))
    (make-let
     (map (lambda (exp) (list (define-var exp) '*unassigned*)) defines)
     (append
      (map (lambda (exp) (make-assignment (define-var exp) (define-val exp))) defines)
      no-defines))))

; c. Install scan-out-defines either in make-procedure or procedure-body. Which is better? Why?

; Recall:
; (define (procedure-body p) (caddr p))
; vs.
; (define (make-procedure parameters body env) (list 'procedure parameters body env))

; Better to install scan-out-defines in make-procedure so every call to make-procedure, including derived procs, would
;     transform bodies. This keeps effects intentional. In order not to break abstraction barriers as well, 
;     procedure-body is selector, and selecting while transforming proc body would have unintended consequences.

;; 4.17: What does env model look like for evaluation of ⟨e3⟩? Why will extra frame created not matter?

; So the body of the original proc is the lambda expression, but now the ⟨e3⟩ expression will be invoked in a separate env
;    where the ⟨e3⟩ expression will be invoked from that one
; There is an extra env b/c of the let expression but it doesn't matter b/c the defines that were scanned out were local to
;    the proc's original instantiation anyway


;; Design a way to make the interpreter implement the “simultaneous” scope rule for internal definitions
;;    without constructing the extra frame.

; So basically, w/o let... just replace defines in original w/ set! instead of introducing let expression and make each var
;    (define var '*unassigned*) at top of so set! doesn't access unbound vars