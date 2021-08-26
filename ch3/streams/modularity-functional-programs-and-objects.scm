#lang sicp

(define square (lambda (x) (* x x)))

; stream boilerplate
(define the-empty-stream '())

(define (create-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (create-stream (cdr lst)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . streams)
  (cons-stream
   (apply proc (map stream-car streams))
   (apply stream-map (cons proc (map stream-cdr streams)))))

(define (stream-ref S n)
  (if (= n 0)
      (display (stream-car S))
      (stream-ref (stream-cdr S) (- n 1))))

(define (show-stream S n)
  (if (= n 0)
      (display '...)
      (begin
        (display (stream-car S))
        (newline)
        (show-stream (stream-cdr S) (- n 1)))))

; rand primitives
(define (rand-update x)
    (let ((a 7901) (b 7907) (m 7909))
      (modulo (+ (* a x) b) m)))

(define (seed-generator)
  (random 7910))

(define random-init
  (seed-generator))

; ex of basic stream implementation random number generator
(define random-nums
  (cons-stream
   random-init
   (stream-map rand-update random-nums)))

;; 3.81: Random stream generate new random number or reset sequence to a specified random value
(define (random-number-generator input-stream)  
  (define (extract-command command-pair)
    (car command-pair))

  (define (get-seed command-pair)
    (if (not (pair? command-pair))
        (error "Not a command-pair")
        (cadr command-pair)))
  
  (define random-numbers
    (cons-stream
     random-init
     (stream-map
      (lambda (command-pair number)
        (cond ((eq? (extract-command command-pair) 'generate) (rand-update number))
              ((eq? (extract-command command-pair) 'reset) (get-seed command-pair))
              (else (error "Incorrect command"))))
      input-stream
      random-numbers)))
  
  (stream-cdr random-numbers))
      
(define inputs (create-stream
                (list '(reset 10) '(generate) '(generate) '(generate) '(reset 40) '(generate) '(generate) '(generate)
                      '(reset 10) '(generate) '(generate) '(generate) '(reset 40) '(generate) '(generate) '(generate)
                      '(reset 10) '(generate) '(generate) '(generate) '(reset 40) '(generate) '(generate) '(generate)
                      '(reset 10) '(generate) '(generate) '(generate) '(reset 40) '(generate) '(generate) '(generate))))

(define random-generated-inputs (random-number-generator inputs))

;; 3.82: Create stream version of estimate-integral from ex 3.5
; foundation for stream monte-carlo version of estimate-integral
; point prims
(define (make-point x y)
  (list x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cadr point))

; rect prims
(define (make-rectangle p1 p2)
  (list p1 p2))
(define (get-bottom-left rect)
  (car rect))
(define (get-top-right rect)
  (cadr rect))

; circle prims + pred
(define (make-circle x y rad)
  (list (list x y) rad))
(define (get-center circle)
  (car circle))
(define (get-radius circle)
  (cadr circle))

(define (circle-predicate x y r)
  (let ((circle (make-circle x y r)))
    (let ((center (get-center circle)) (radius (get-radius circle)))
      (define (test-point point)
        (let ((x (get-x point)) (y (get-y point)) (cx (get-x center)) (cy (get-y center)))
          (<= (+ (square (- x cx))
                 (square (- y cy)))
              (square radius))))
      test-point)))

; rand-in-range for estimate-integral
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; iterative stream basis for monte-carlo experiments
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

; will create an infinite stream of experiments
(define (estimate-integral circle-pred rect)
  (define (random-point-in-rect rect)
    (list (random-in-range (get-x (get-bottom-left rect)) (get-x (get-top-right rect)))
          (random-in-range (get-y (get-bottom-left rect)) (get-y (get-top-right rect)))))
  (define (experiments-stream)
      (cons-stream (random-point-in-rect rect) (experiments-stream)))
  (monte-carlo (stream-map circle-pred (experiments-stream)) 0 0))

(define enclosed (estimate-integral (circle-predicate 0 0 1) (make-rectangle (make-point -2 -2) (make-point 2 2)))) 