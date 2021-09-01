#lang sicp

; only numbers and strings are self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true) (else false)))


; atoms
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; assignments
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; define procs
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters 
                   (cddr exp)))) ; body

; lambda expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; make-lambda
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp)
      'false))

; if constructor to be used by cond->if
(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; sequence->exp turns seq into single exp w begin if only one exp in seq
(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

; application car is operator while cdr is operands
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses)) (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first)) (expand-clauses rest))))))


;; 4.2: Louis Reasoner wants to reorder application clause in eval before assignments clause
; recall original eval
;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
;        ((begin? exp) (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type: EVAL" exp))))

; a. What is wrong with this?
; Since all assignments are pairs that start with set! and definitions are pairs that start with define
;    reordering will completely misinterpret other clauses as applications and will error at applying things like 'set! or 'define
;    to the list-of-values of the operands

; b. How can louis reasoner get his ordering to work?
; He needs to redefine what application means. Instead of a pair, it should have some tag that calls the expression
;    The question gives the answer of 'call as the tag, so the application check would need to check if exp has tag 'call and if
;    so, call the operator as cadr of expression on operands which would be cddr exp

;; With this check, application clause doesn't mistake other tagged clause checks and applies operator to operands correctly

;(define (application? exp)
;  (tagged-list? exp 'call))
;(define (operator exp)
;  (cadr exp))
;(define (operands exp)
;  (cddr exp))

;; 4.3: Rewrite eval so dispatch is done in data-directed style
(define operation-table make-table) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc)) 

; put tagged operations in table
(put 'op 'quote text-of-quotation) 
(put 'op 'set! eval-assignment) 
(put 'op 'define eval-definition) 
(put 'op 'if eval-if) 
(put 'op 'lambda (lambda (x y)  
                   (make-procedure (lambda-parameters x) (lambda-body x) y))) 
(put 'op 'begin (lambda (x y)  
                  (eval-sequence (begin-sequence x) y))) 
(put 'op 'cond (lambda (x y)  
                 (evaln (cond->if x) y)))

(define (eval expr env)  
  (cond ((self-evaluating? expr) expr)  
        ((variable? expr) (lookup-variable-value expr env))  
        ((get 'op (operator expr)) ((get 'op (operator expr)) expr env))  
        ((application? expr)   
         (apply (eval (operator expr) env)   
                (list-of-values (operands expr) env)))  
        (else (error "Unknown expression type -- EVAL" expr))))   

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
        ((false? (eval (first-predicate exps) env) #f))
        (else (eval-and (rest-predicates exps) env))))

; or
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-predicates exps)
  (cdr exp))

(define (eval-or exps env)
  (cond ((empty-predicates exps) #f)
        ((true? (eval (first-predicate exps) env) #t))
        (else (eval-or (rest-predicates exps) env))))

; derived expressions for 'and' and 'or' from if 
(define (and->if exp) 
  (expand-and-predicates (and-predicates exp))) 
(define (expand-and-predicates predicates) 
  (if (no-predicates? predicates) 
      'true 
      (make-if (first-predicate predicates) 
               (expand-predicates (rest-predicates predicates)) 
               'false))) 
  
(define (or->if exp) 
  (expand-or-predicates (or-predicates exp))) 
(define (expand-or-predicates predicates) 
  (if (no-predicate? predicates) 
      'false 
      (make-if (first-predicate predicates) 
               'true
               (expand-predicates (rest-predicates predicates))))) 

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
                         (expand-clause rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

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
  (tagged-list exp 'let*))

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
  
            