#lang scheme
;; We can use type tag to distinguish between rectangular and polar coords
(define (type-tag datum)
  (if (pair? datum) (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; Method of modularizing system design and creating packages for others to interface with
; ie. Representing complex-numbers in different ways
; And note: although both packages have some colliding names for certain procedures,
;    they operate within each package's namespace

;; At the moment, we don't have put and get selectors

;; Rectangular-package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar-package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; apply-generic looks in table for name of given operation, and applies procedure if found
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

;; For example, with apply-generic, we can define our selectors as follows...
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; 2.73: In section 2.3.2 when we made a symbolic differentiation program, we used
;;       predicates to determine the type of a given expression (product?, sum?, ...)
;;       But we can get the type tag in in another way...

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a: What is the new proc doing? Why aren't we assimilating number? and variable?
;;    It's finding the necessary 'deriv operation associated with a particular type
;;    with the given expression in the package/op table and applying it as necessary.
;;    Since the derivative of a number/same vars are the building blocks of derivatives
;;    and have no "type" associated with them (that can be gotten from '+, '*, etc) b/c
;;    there are none, we just apply the basic operations for derivs of numbers/vars.

;; b: Procedures for derivs of sums and products
(define (install-sum-package) ;; copy-paste procs from earlier deriv and put procs in table
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s)) ;; second item of sum list
  (define (augend s) (caddr s)) ;; third item of sum list
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp)
    (make-sum
     (deriv (addend exp))
     (deriv (augend exp))))

  (define (tag x) (attach '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (install-product-package)
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p)) ;; second item of product list
  (define (multiplicand p) (caddr p)) ;; third item of product list
 
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
 
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          ((and (variable? m1) (variable? m2) (eq? m1 m2)) (list '** m1 2))
          (else (list '* m1 m2))))

  (define (deriv-product exp)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (define (tag x) (attach '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

;; c: Create another deriv package other than sum or product
(define (install-exponentiation-package)
  (define (** x y)
  (define (power a n)
    (if (= n 0) a
        (power (* a x) (- n 1))))
  (power 1 y))

  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

  (define (base exp)
    (cadr exp))

  (define (exponent exp)
    (caddr exp))

  (define (make-exponentiation a b)
    (cond ((=number? a 0) 0)
          ((=number? b 1) a)
          ((or (=number? b 0) (=number? a 1)) 1)
          ((and (number? a) (number? b)) (** a b))
          (else (list '** a b))))

  (define (deriv-exp exp)
    (if (eq? (base exp) var)
             (make-product
              (exponent exp)
              (make-exponentiation (base exp) (make-sum (exponent exp) (- 1))))
             0))

  (define (tag x) (attach '** x))
  (put 'deriv '(**) deriv-exp)
  (put 'make-exponentiation '**
       (lambda (x y) (tag (make-exponentiation x y))))
  'done)

;; d: Suppose we indexed deriv procs such that ((get (operator exp) 'deriv) (operands exp) var) was our
;;    get proc, how would that affect our above implementation of packages?

;; It would require we tagged and indexed each package's internal procs within our put procs in reverse

;; 2.74: We have a file-structure with employee information keyed under 'address' and 'salary'
;; a: Implement a get-record proc that is applicable to any division file; also explain how each division's
;;    files should be structured

;; Each division's files should be a list of lists, where each list in the division represents an employee
;; Should we want to get a particular employee's record, we have to find them by name
;; Maybe something like '((Sam (address (182 Hampton St)) (salary 189000)) ...)
(define (get-employee name files) ;; we will iterate through the employee and check against key
  (if (eq? (caar files) name) (cdar files)
      (get-employee name (cdr files))))

(define (get-record key file)
  (if (eq? (car file) key) (cadr file) ;; k, v pairs look like '(address (182 Hampton St)) or '(salary 189000)
      (get-record key (cdr file))))


;; b: Implement a get-salary procedure. How should files be structured?
;; We can implement a get-salary procedure as a specific case of our get-record and get-employee procs
(define (get-salary name files)
  (get-record 'salary (get-employee name files)))

;; c: Implement find-employee-record
;; We already did this in part a but we can just repurpose our get-employee proc
(define find-employee-record
  (lambda (name files) (get-employee name files)))

;; d: When the Insatiable company takes over a new company, how must new employees be assimilated into existing
;;    file structure?

;; They must follow format of name as car of employee record for find-employee-record proc, and each bit of
;; info must be associated with a given key where the key is the car of the list and the value is the cadr

;; 2.75: Implement make-from-mag-ang in message passing style
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          (else (error "Unknown op MAKE_FROM_MAG_ANG: " op))))
  dispatch)

;; 2.76: Describe differences between generic operations w/ explicit dispatch vs data-directed vs message-passing
;;       Which style would be most appropriate for a new system in which new operations are often added?

;; For generic operations, we build up modules that have an explicit type associated with them to call procs.
;    Though procs in different modules, if used in the same environment, can't be named the same.
;; Data-directed modularizes even further and puts and gets procs in and from a table associated with module
;; Message-passing evokes certain behavior from a procedure via the message passed to it, dispatching
;    procedures on a given operation.

;; In a system where new operations are being added, data-directed may be best since it directly deals with
;;    adding a new operation for each procedure, modularizing system design.
;; Message-passing requires that new types have dispatch procedures that implement its operations. When a new
;;    operation is added in this case, the dispatch procedure must also be modified to accommodate the new operations

;; 2.5.1 Generic Arithmetic Operators
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Package for handling ordinary numbers
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x))) 'done)

;; Users of scheme-number package will create (tagged) ordinary numbers:
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; Now, we can create an arithmetic-operator package that operates on rational-numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Can create a complex pkg in the same way: by tagging complex procs with 'complex tag
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages (define (make-from-real-imag x y)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
 (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-angle
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-angle
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 2.77: apply-generic throws error no method magnitude on types (complex) when evaluating (magnitude z). Why?
; Those procs from rectangular and polar packages hadn't been put in complex package.
; apply-generic finds type-tags from given operation and applies the relevant proc as the tag is found
; recall that we have a table or pkg with our procs already defined and apply-generic helps a newly-named proc
;    check table to find which operation in the table to use
; apply-generic therefore is invoked twice here: to call the complex package and the rectangle pkg
;    associated with magnitude

;; 2.78: Modify attach-tag, type-tag, and contents to represent numbers using scheme's internal type system
(define (attach-tag type-tag contents)
  (if (number? contents) contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))