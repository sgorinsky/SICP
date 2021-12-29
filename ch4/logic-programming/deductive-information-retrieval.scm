#lang sicp

;;; Query input:
(job ?x (computer programmer))

;;; Query results:
(job (Hacker Alyssa P) (computer programmer))
(job (Fect Cy D) (computer programmer))

;; 4.55: Simple queries that retrieve following
; a. All people supervised by Ben Bitdiddle
(supervisor ?who (Bitdiddle Ben))

; b. Names and jobs of all people in the accounting division
(job ?who (accounting . ?accounting-job))

; c. Names and addresses of all people who live in Slumerville
(address ?who (Slumerville . ?address))