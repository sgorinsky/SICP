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

;; 4.57: Define a rule that p1 can replace p2 if either p1 has the same job as p2
;;       or someone who does p1's job can also do p2's job and if p1 and p2 are not
;;       the same person

(rule (can-replace ?p1 ?p2)
      (or (and (job ?p1 ?j1)
               (job ?p2 ?j2)
               (same ?j1 ?j2))
          (and (can-do-job ?j1 (. ?j2))
               (not (same ?p1 ?p2)))))
       
; a. All people who can replace Cy D Fect
(can-replace ?p1 (Fect Cy D))

; b. All people who can replace someone who is being paid more than they are,
;    together with the two salaries
(and (can-replace ?p1 ?p2)
     (salary ?p1 ?a1)
     (salary ?p2 ?a2)
     (lisp-value > ?p2 ?p1))

;; 4.58: Define a rule that a person is a "big shot" in a division if the person works in
;;       a division but doesn't have a supervisor in that division
(rule (big-shot ?person)
      (and (supervisor ?person ?boss)
           (job ?person (?division . ?title))
           (not (job ?boss (?division . ?boss-role))))) ; shouldn't find match for ?division here

;; 4.59: Finding meetings for people in ord
; assertions Ben made to add meetings to database
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

; a. Query database for all meetings that occur on Firday
(meeting ?division (Friday ?time))

; b. Alyssa P. Hacker thinks it'd be more useful to ask for person-specific meetings
;    Finish Alyssa's rule
(rule (meeting-time ?person ?day-and-time)
      (and (meeting ?division ?day-and-time)
           (job ?person (?division . ?title))))

; c. Using rule, find all Wednesday meetings for Alyssa P. Hacker
(meeting-time (Hacker Alyssa P) (Wednesday . ?time))

;; 4.60: Why are people listed twice when querying lives-near?
; ie.
(lives-near ?person-1 ?person-2)
; -> (lives-near (Hacker Alyssa P) (Fect Cy D))
; -> (lives-near (Fect Cy D) (Hacker Alyssa P))

; Why does this happen?
; This happens because the query finds all assertions of some p1 living near p2, it isn't
;     checking the results for consistency

; Is there a way to find a list of people who live near each other, in which each pair
;     appears only once?
; Maybe if we defined a rule of sorted retrieval and then make sure our queries were unique,
;     that would make our results unique