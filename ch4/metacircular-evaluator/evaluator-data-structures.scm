#lang sicp

;; handling predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; handling procs
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p)) (define (procedure-body p) (caddr p))
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

;; 4.11: instead of representing frames as pairs of lists, can represent frames as list of bindings -- rewrite env operations
(define (create-frame vars vals)
  (map car vars vals))

(define (frame-vars frame)
  (map car frame))

(define (frame-vals frame)
  (map cadr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (list var val) frame)))

; extend-env is abstracted out
; lookups are too, just replace frame-variables and frame-values w frame-vars and frame-vals
;     there are probably more efficient lookups that don't include the call to map in frame-vars and frame-vals but we only need
;     replace primitives and can keep our env handling working
