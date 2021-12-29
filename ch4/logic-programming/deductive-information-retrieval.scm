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

;; 4.56: Compound Queries that retrieve the following

; a. Names of all people supervised by Ben Bitdiddle, together with their addresses
(and (supervisor ?who (Ben Bitdiddle))
     (address ?who ?where))

; b. All people whose salary is less than Ben Bitdiddle's together with their salaries
;    and Ben Bitdiddle's
(and (salary (Ben Bitdiddle) ?amount)
     (salary ?person ?salary)
     (lisp-value > ?salary ?amount))

; c. All people supervised by someone who is not in the computer division, together
;    with the person's name and job
(and (supervisor ?person ?boss)
     (not (job ?boss (computer . ?rest-of-job)))
     (job ?person ?job))