#lang sicp

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
