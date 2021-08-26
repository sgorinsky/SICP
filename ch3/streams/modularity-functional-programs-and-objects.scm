#lang sicp

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
  