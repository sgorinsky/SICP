#lang sicp
;;; implementation of metacircular-evaluator

;; abstractions for expressions
; only numbers and strings are self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true) (else false)))

;; atoms
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; assignments
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var val)
  (list 'set! var val))

;; define procs
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters 
                   (cddr exp)))) ; body

;; lambda expressions
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; make-lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp)
      'false))

;; if constructor to be used by cond->if
(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; sequence->exp turns seq into single exp w begin if only one exp in seq
(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

;; application car is operator while cdr is operands
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; handling predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; handling procs
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; handling envs
; represent an env as a list of frames where enclosing env is cdr of list
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; each env represented as a pair of lists where first list is vars and second is vals
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; adjoin frame of lists of vars and vals to new env
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; scan all envs to lookup var
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; same thing for changing a val in env
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; to define, check first frame for binding and if doesn't exist, add binding to frame
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;;; 4.11: instead of representing frames as pairs of lists, can represent frames as list of bindings -- rewrite env operations
;(define (create-frame vars vals)
;  (map car vars vals))
;
;(define (frame-vars frame)
;  (map car frame))
;
;(define (frame-vals frame)
;  (map cadr frame))
;
;(define (add-binding-to-frame!! var val frame)
;  (set-car! frame (cons (list var val) frame)))
;
;(define (first-frame envs)
;  (car envs))
(define (rest-frames envs)
  (cdr envs))
;(define (first-pair frame)
;  (car frame))
;(define (rest-pairs frame)
;  (cdr frame))
;
;; extend-env is abstracted out
;; lookups are too, just replace frame-variables and frame-values w frame-vars and frame-vals
;;     there are probably more efficient lookups that don't include the call to map in frame-vars and frame-vals but we only need
;;     replace primitives and can keep our env handling working

;; 4.12: Create abstractions for lookup-variable-value, set-variable-value!, define-variable!
;        --> really just need to abstract out env-loop and scan
(define (search-envs var env)
  (define (scan frame) ; could use assq for implementation but would look cluttered w/ if clauses
    (cond ((null? frame) (search-envs var (rest-frames env)))
          ((eq? var (car (first-pair frame))) (first-pair frame))
          (else (scan (rest-pairs frame)))))
  (if (null? env)
      #f ; soft erroring
      (scan (first-frame env))))

; look, much more concise
(define (lookup-var-val var env)
  (let ((pair (search-envs var env)))
    (if pair
        pair
        (error "Unbound global var: " var))))

(define (set-var-val! var val env)
  (let ((pair (search-envs var env)))
    (if pair
        (set-cdr! pair val)
        (error "Unbound variable: SET!" var))))

(define (define-var! var val env)
  (let ((pair (search-envs var env)))
    (if pair
        (set-cdr! pair val)
        (add-binding-to-frame! var val (first-frame env)))))

;; 4.13: make-unbound! proc to unset vars in env
(define (make-unbound! var env)
  (let ((found-var #f))
    (define (unbind-frame-var var frame)
      (cond ((null? frame) (make-unbound! var (cdr env)))
            (else
             (let ((first (first-pair frame)))
               (if (eq? var (car first))
                   (begin
                     (if (null? (rest-pairs frame))
                         (set-car! frame '())
                         (let ((next (first-pair (rest-pairs frame))))
                           (begin
                             (set-car! frame next)
                             (set-cdr! frame (cddr frame)))))
                     (set! found-var #t)
                     (make-unbound! var (rest-frames env)))
                   (unbind-frame-var var (rest-pairs frame)))))))
    (if (null? env)
        (if found-var
            'done
            (error "Unbound var: " var))
        (unbind-frame-var var (first-frame env)))))

;; 4.4: eval-and and eval-or + show as derived expressions
(define (first-predicate exps)
  (car exps))
(define (rest-predicates exps)
  (cdr exps))
(define (empty-predicates exps)
  (null? exps))

; and
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-predicates exps)
  (cdr exp))

(define (eval-and exps env)
  (cond ((empty-predicates exps) #t)
        ((false? (eval (first-predicate exps) env)) #f)
        (else (eval-and (rest-predicates exps) env))))

; or
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-predicates exps)
  (cdr exp))

(define (eval-or exps env)
  (cond ((empty-predicates exps) #f)
        ((true? (eval (first-predicate exps) env)) #t)
        (else (eval-or (rest-predicates exps) env))))

; derived expressions for 'and' and 'or' from if
(define (no-predicates? predicates)
  (null? predicates))

(define (and->if exp) 
  (expand-and-predicates (and-predicates exp))) 
(define (expand-and-predicates predicates) 
  (if (no-predicates? predicates) 
      'true 
      (make-if (first-predicate predicates) 
               (expand-and-predicates (rest-predicates predicates)) 
               'false))) 
  
(define (or->if exp) 
  (expand-or-predicates (or-predicates exp))) 
(define (expand-or-predicates predicates) 
  (if (no-predicates? predicates) 
      'false 
      (make-if (first-predicate predicates) 
               'true
               (expand-or-predicates (rest-predicates predicates))))) 

;; 4.5: Modify syntax of cond to support (⟨test⟩ => ⟨recipient⟩) syntax
; ie. (cond ((assoc 'b '((a 1) (b 2))) => cadr)) returns 2
(define (arrow clause)
  (cadr clause))
(define (arrow-clause? clause)
  (eq? '=> (arrow clause)))

(define (arrow-clause-predicate clause)
  (car clause))
(define (arrow-clause-proc clause)
  (caddr clause))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

; expand-clauses
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses)) (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))

            (if (arrow-clause? first) ; include check for => syntax
                (make-if (arrow-clause-predicate first)
                         ((arrow-clause-proc first) (arrow-clause-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

;; lets
;; 4.6: lets are derived expressions: implement let->combination that reduces let expressions to proper lambdas then
;;      add appropriate eval clause
(define (let? exp)
  (tagged-list? exp 'let))

(define (make-let pairs-list body)
  (list 'let pairs-list body))

(define (let-params exp)
  (map car (cadr exp))) ; same thing as commented out code below
;  (define (iter-params list-of-assignments)
;    (if (null? list-of-assignments)
;        '()
;        (cons (caar list-of-assignments) (iter-params (cdr list-of-assignments)))))
;  (iter-params (cadr exp)))

(define (let-values exp)
  (map cadr (cadr exp))) ; same as commented out code below
;  (define (iter-values list-of-assignments)
;    (if (null? list-of-assignments)
;        '()
;        (cons (cadar list-of-assignments) (iter-values (cdr list-of-assignments)))))
;  (iter-values (cadr exp)))
         
(define (let-body exp)
  (cddr exp))

; transforming let expression into callable lambda expression with values for args
(define (let->combinations exp)
  (cons (make-lambda (let-params exp) (let-body exp)) (let-values exp)))

; include let clause in eval
; ((let? exp) (eval (let-combinations exp) env))

;; 4.7: let* as a derived expression of nested lets
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-pairs exp)
  (cadr exp))
(define (let*-body exp)
  (cddr exp))


(define (first-pair pairs)
  (car pairs))
(define (rest-pairs pairs)
  (cdr pairs))

(define (let*->nested-lets exp)
  (define (nested-lets pairs body)
    (if (null? pairs)
        body
        (make-let (list (first-pair pairs)) (nested-lets (rest-pairs pairs) body))))
  (nested-lets (let*-pairs exp) (let*-body exp)))

; is adding (eval (let*->nested-lets exp) env) clause to eval enough?
; Yes b/c initial call to let*->nested-lets requires that we evaluate let* as nested-lets.
;    First call to let* will look like ('let (param-val-pairs) (...))
;    So we basically convert let* to a let expression on that initial call and then recursively eval lets whose bodies are lets
;        from let*->nested-lets conversion proc
  

; 4.8: Handle (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩) in amended let->combinations
(define (named-let? exp)
  (and (let? exp) (symbol? (cadr exp))))
(define (named-let-var exp)
  (cadr exp))
(define (named-let-bindings exp)
  (caddr exp))
(define (named-let-params exp)
  (map car (named-let-bindings exp)))
(define (named-let-vals exp)
  (map cadr (named-let-bindings exp)))
(define (named-let-body exp)
  (cdddr exp))

(define (make-define var-name params body)
  (if params
      (list 'define (list var-name params) body)
      (list 'define var-name body)))

(define (let->combos exp)
  (if (named-let? exp)
      (cons ((make-define
              (named-let-var exp) (named-let-params exp) (named-let-body exp))
             (named-let-vals exp)))
      (cons (make-lambda
             (let-params exp) (let-body exp))
            (let-values exp))))

;; produces list of args to which proc is applied when eval is processing procedure application
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; eval-if evaluates predicate part of expression in given environment
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env) (eval (if-alternative exp) env)))

;; eval-sequence is used by apply to evaluate sequence of expressions in begin expression
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; handles assignments to variables, calls eval to find value to be assigned and transmits value
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; definitions to variables handled similarly
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                       (eval (definition-value exp) env)
                       env)
  'ok)

;; 4.1: Rewrite list-of-values proc twice to evaluate operands from left-to-right and right-to-left
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first-exp (eval (first-operand exps) env))) ; evaluates this before anything in cons
        (cons first-exp
              (list-of-values-left-to-right (rest-exps exps) env)))))

(define (list-of-values-right-to-left exps env)
  (list-of-values-left-to-right (reverse exps) env))


;; handling primitives
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

; primitives in global env
(define primitive-procedures
  (list
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)
   (list '() '())
   (list 'list list)
   ; ⟨more primitives⟩
   ))

; extract names and bodies of primitives
(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

;; setup env
(define (setup-environment)
  (let ((initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

; apply proc
; (define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (primitive-implementation proc) args)

;; REPL
(define (user-print output)
  (display output)
  (newline))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))


; eval checks for primitive expressions and special forms
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


; apply applies arguments to procedure in two cases, primitive and compound procedures
; compound proc case extends new environment from base environment of applied proc and binds parameters of proc to args 
;    of appropriate proc that should be applied
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure) arguments
           (procedure-environment procedure))))
        (else (error
               "Unknown procedure type: APPLY" procedure))))


