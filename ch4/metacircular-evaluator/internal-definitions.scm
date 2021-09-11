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


;; 4.18: Will following representation of internal procs work for solve below?
;(lambda ⟨vars⟩
;  (let ((u '*unassigned*) (v '*unassigned*))
;    (let ((a ⟨e1⟩) (b ⟨e2⟩))
;      (set! u a)
;      (set! v b))
;    ⟨e3⟩))

;(define (solve f y0 dt)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)

; Recall definition for integral from ex 3.77
;(define (integral delayed-integrand initial-value dt)
;  (cons-stream
;   initial-value
;   (let ((integrand (force delayed-integrand)))
;     (if (stream-null? integrand)
;         the-empty-stream
;         (integral (delay (stream-cdr integrand))
;                   (+ (* dt (stream-car integrand)) initial-value)
;                   dt)))))

; So with the solve proc, we're delaying the evaluation of dy until y is called which it is on define dy's (stream-map f y).
;    This means that because (delay dy) is a promise, it must be forced on the function init to throw an error,
;    which we can see happens in the inegral proc's let clause. Therefore, even though the integral proc still expects a
;    promise as its first arg, it will force it on the function's definition and thus,
;    the internal procs fail when they're defined with the new representation of internal defines

; 4.19: What is the result of the following expression? Ben Bitdiddle says 16, Alyssa P Hacker says error, Eva Lu Ator says 20

;(let ((a 1))
;  (define (f x)
;    (define b (+ a x))
;    (define a 5)
;    (+ a b))
;  (f 10))

; a. Who is right?
; If we take this at face-value and assume there is some way that internal procs are simultaneously defined, then Eva is right,
;    the result would be 20 since b would take a's value when it is defined and then sub that value into the body of f which would
;    be (+ a b) -> (+ a (+ a 10)) -> (+ 5 (5 10)) -> 20
; But we know that in a way, Ben is right because of the sequential eval of the body of the proc, which will look for a when it is
;    evaluated. That actually leads to the issue that Alyssa brings up though with our scan-out-defines implementation from 4.16
;    Since b expects a value for a and not some proc, it passes the value for that would be defined within the
;    body of f's scanned-out let procedure, which would be '*unassigned*, and since that value can't be
;    evaluated in (+ '*unassigned 10), the proc would throw an error


; b. How can we implement Eva's ideal?
; If we define a first or delay the evaluation of b's body, then force it when (+ a b) is invoked, then Eva's approach would work.
;    But that is left to the implementation of the procedure, not the implementation of handling internal definitions, so it
;    would work in this case, but likely not others.

; simultaneous:
(let ((a 1))
  (define (f x)
    (define b (delay (+ a x)))
    (define a 5)
    (+ a (force b)))
  (f 10))

; sequential:
(let ((a 1))
  (define (f x)
    (define a 5)
    (define b (+ a x))
    (+ a b))
  (f 10))
