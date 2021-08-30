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

(define (and->if

; or
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-predicates exps)
  (cdr exp))

(define (eval-or exps env)
  (cond ((empty-predicates exps) #f)
        ((true? (eval (first-predicate exps) env) #t))
        (else (eval-or (rest-predicates exps) env))))

; derived expressions from if 
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


            