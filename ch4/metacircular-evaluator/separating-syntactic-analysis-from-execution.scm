#lang sicp

;; separate out analysis in eval proc so its performed once
(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp))) ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

; ignores env, returns exp
(define (analyze-self-evaluating exp) (lambda (env) exp))

; extract text of quotation in analysis phase
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

; lookup variable 
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

; assignment
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

; definition
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env) 'ok)))

; if
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

; lambda
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

; sequence
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

; application
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc) args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

;; 4.22: analyze-let
; we can just transform a let to a lambda and analyze that i believe
(define (analyze-let exp)
  (analyze-lambda (let->combinations exp)))

; let clause in analyze:
; ((let? exp) (analyze-let exp))

;; 4.23: Compare Alyssa Hacker's implementation of analyze-sequence and the one above

; Alyssa's: 
;(define (analyze-sequence exps)
;  (define (execute-sequence procs env)
;    (cond ((null? (cdr procs)) ((car procs) env))
;          (else ((car procs) env) (execute-sequence (cdr procs) env))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs)
;        (error "Empty sequence: ANALYZE"))
;    (lambda (env)
;      (execute-sequence procs env))))

; text's:
;(define (analyze-sequence exps)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;        first-proc
;        (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs) (error "Empty sequence: ANALYZE"))
;    (loop (car procs) (cdr procs))))

; Text's implementation handles two procs at once w/ sequentially and is a little more complicated as a result while Alyssa's
;    is a lot simpler and merely loops through all the procs and only executes the first proc in the sequence of procs at a time.
; For an expression with only one procedure, they both execute the first-proc similarly, but for an expression with two or more
;    procs, the text's execution kind of accumulates the procs in the env and executes them in the expressions.
; The outcome of this "accumulation" are avoiding the unnecessary calls to execute-sequence in Alyssa's proc. In the text's
;    implementation, as the program loops through the procs, it evaluates them and saves the proc in place in the first arg instead
;    of needing to execute each proc again ang again like in Alyssa's implementation.
