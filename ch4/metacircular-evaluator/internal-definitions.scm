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


 