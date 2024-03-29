#lang sicp

(define null '())
(define (require p)
  (if (not p) (amb)))

(define (reduce fn base lst)
  (if (null? lst) base
      (fn (car lst) (reduce fn base (cdr lst)))))
   
(define (distinct? elements)
  (define (check element lst)
    (cond ((null? lst) #t)
          ((equal? element (car lst)) #f)
          (else (check element (cdr lst)))))
  (define (has-false lst)
    (cond ((null? lst) #t)
          ((not (car lst)) #f)
          (else (has-false (cdr lst)))))
  (define (get-truth-values lst)
    (if (null? lst) '()
        (cons (check (car lst) (cdr lst)) (get-truth-values (cdr lst)))))
  (has-false (get-truth-values elements)))
        
        
  
;;; Logic Puzzles
; multiple dwelling puzzle
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 4.38: Modify multiple-dwellings to omit the requirement that smith and fletcher do not live on adjacent floors. How many
;;       different solutions are there now?
(define (updated-multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 4.39: Does order of restrictions matter for runtime of application?

; Yes, distinct is by far the longest proc to run since it compares each element against all others, so if all the other restrictions
;    make it through, distinct should be the last check

;; 4.40: How many assignments of people to floors are there both before and after the distinct? check? Write a more efficient
;;       nondeterministic multiple-dwelling proc based upon possibilities that are not already ruled out by previous restrictions.

; There are 5 for each of the 5 people so there are 5^5 combinations,
;    but we only care about the ones that meet the distinct conditions
; After the distinct? check, there are 5! possible combinations since one person cannot occupy another person's floor

; I could either nest lets to include requires that prune tree or I could just include conditions in the initializations for
;    each person's set of possibilities in amb
(define (effictient-multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 4))
        (fletcher (amb 2 4))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))

    ; conditions for multiple dwellings
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (distinct? (list baker cooper fletcher miller smith)))

    ; result
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 4.41: Write an ordinary scheme proc to solve the multiple-dwelling problem
(define (ordinary-multiple-dwelling)
  (define (remove x s)
    (cond ((null? s) '())
          ((eq? x (car s)) (remove x (cdr s)))
          (else (cons (car s) (remove x (cdr s))))))

  (define (permutations s)
    (if (null? s) (list '())
        (reduce
         append
         '() ;; flatmap
         (map (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
              s))))

  (define (check-cands perms-list)
    (if (null? perms-list) '()
        (let ((perm (car perms-list)))
          (let ((baker (car perm)) (cooper (cadr perm)) (fletcher (caddr perm)) (miller (cadddr perm)) (smith (cadddr (cdr perm))))
            (if (or (= baker 5) (= cooper 1)
                    (= fletcher 5) (= fletcher 1)
                    (<= miller cooper)
                    (= (abs (- fletcher cooper)) 1)
                    (= (abs (- smith fletcher)) 1)
                    (not (distinct? (list baker cooper fletcher miller smith))))
                (check-cands (cdr perms-list))
                (append (list
                         (list 'baker baker)
                         (list 'cooper cooper)
                         (list 'fletcher fletcher)
                         (list 'miller miller)
                         (list 'smith smith))
                        (check-cands (cdr perms-list))))))))

  (check-cands (permutations (list 1 2 3 4 5))))
  
    
;; 4.42: Write program to solve liars puzzle
(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))

    (require (or (and (not (= betty 3)) (= kitty 2))
                 (and (= betty 3) (not (= kitty 2)))))
    (require (or (and (not (= ethel 1)) (= joan 2))
                 (and (= ethel 1) (not (= joan 2)))))
    (require (or (and (not (= joan 3)) (= ethel 5))
                 (and (= joan 3) (not (= ethel 5)))))
    (require (or (and (not (= kitty 2)) (= mary 4))
                 (and (= kitty 2) (not (= mary 4)))))
    (require (or (and (not (= mary 4)) (= betty 1))
                 (and (= mary 4) (not (= betty 1)))))
    (require (distinct? (list betty ethel joan kitty mary)))

    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))


;; 4.43: Yacht Puzzle
(define (yacht)
  (let ((moore 'mary)
        (downing (amb 'gabrielle 'lorna 'rosalind))
        (hall (amb 'gabrielle 'lorna))
        (barnacle 'melissa)
        (parker (amb 'lorna 'rosalind)))

    (require (and (not (eq? barnacle 'gabrielle))
                  (not (eq? moore 'lorna))
                  (not (eq? hall 'rosalind))
                  (not (eq? downing 'melissa))))
    
    (require (distinct? (list moore downing hall barnacle parker)))

    (list (list 'moore moore)
          (list 'downing downing)
          (list 'hall hall)
          (list 'barnacle barnacle)
          (list 'parker parker))))
                  
             
    
;; 4.44: eight-queens
(define (eight-queens)
  (define (check-prior-positions coordinate positions)
    (if (null? positions) #t
        (let* ((xc (car coordinate))
               (yc (cadr coordinate))
               (position (car positions))
               (xp (car position))
               (yp (cadr position)))
          (if (or (= xc xp) (= yc yp) (= (abs (- xc xp)) (abs (- yc yp)))) #f
              (check-prior-positions coordinate (cdr positions))))))
  (define (randomize-position)
    (list (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8)))
          
  (let ((a (randomize-position)) (b (randomize-position)))
    (require (check-prior-positions b (list a)))
    (let ((c (randomize-position)))
      (require (check-prior-positions c (list a b)))
      (let ((d (randomize-position)))
        (require (check-prior-positions d (list a b c)))
        (let ((e (randomize-position)))
          (require (check-prior-positions e (list a b c d)))
          (let ((f (randomize-position)))
            (require (check-prior-positions f (list a b c d e)))
            (let ((g (randomize-position)))
              (require (check-prior-positions g (list a b c d e f)))
              (let ((h (randomize-position)))
                (require (check-prior-positions h (list a b c d e f g)))
                (list a b c d e f g h)))))))))


;;; Parsing Natural Language
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

; old parsing procs
;(define (parse-noun-phrase)
;  (list 'noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))

;(define (parse-sentence)
;  (list 'sentence
;        (parse-noun-phrase)
;        (parse-word verbs)))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions) (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;; 4.45: Five different ways to parse '(the professor lectures to the student with the cat)
;;       What are the different meanings?

;; Starting a new problem 
;; Amb-Eval output: 
;(sentence  
; (noun-phrase (articles the) (nouns professor))  
; (verb-phrase (verb-phrase  
;               (verb-phrase (verb lectures)  
;                            (pre-phrase (prep to) (noun-phrase (articles the) (nouns student)))) 
;               (pre-phrase (prep in) (noun-phrase (articles the) (nouns class)))) 
;              (pre-phrase (prep with) (noun-phrase (articles the) (nouns cat))))) 
;  
;; Amb-Eval input: 
;try-again 
;  
;; Amb-Eval output: 
;(sentence (noun-phrase (articles the) (nouns professor)) 
;          (verb-phrase (verb-phrase (verb lectures)  
;                                    (pre-phrase (prep to)  
;                                                (noun-phrase (articles the)  
;                                                             (nouns student)))) 
;                       (pre-phrase (prep in)  
;                                   (noun-phrase (noun-phrase (articles the) (nouns class)) 
;                                                (pre-phrase (prep with)  
;                                                            (noun-phrase (articles the) (nouns cat))))))) 
;  
;; Amb-Eval input: 
;try-again 
;  
;; Amb-Eval output: 
;(sentence (noun-phrase (articles the) (nouns professor)) 
;          (verb-phrase (verb-phrase (verb lectures)  
;                                    (pre-phrase (prep to)  
;                                                (noun-phrase (noun-phrase (articles the) (nouns student))  
;                                                             (pre-phrase (prep in)  
;                                                                         (noun-phrase (articles the) (nouns class)))))) 
;                       (pre-phrase (prep with) (noun-phrase (articles the) (nouns cat))))) 
;  
;; Amb-Eval input: 
;try-again 
;  
;; Amb-Eval output: 
;(sentence (noun-phrase (articles the) (nouns professor))  
;          (verb-phrase (verb lectures)  
;                       (pre-phrase (prep to)  
;                                   (noun-phrase (noun-phrase (noun-phrase (articles the) 
;                                                                          (nouns student)) 
;                                                             (pre-phrase (prep in)  
;                                                                         (noun-phrase (articles the)  
;                                                                                      (nouns class)))) 
;                                                (pre-phrase (prep with) (noun-phrase (articles the) (nouns cat))))))) 
;  
;; Amb-Eval input: 
;try-again 
;  
;; Amb-Eval output: 
;(sentence (noun-phrase (articles the) (nouns professor)) 
;          (verb-phrase (verb lectures)  
;                       (pre-phrase (prep to) 
;                                   (noun-phrase (noun-phrase (articles the)  
;                                                             (nouns student))  
;                                                (pre-phrase (prep in) 
;                                                            (noun-phrase (noun-phrase (articles the) 
;                                                                                      (nouns class))  
;                                                                         (pre-phrase (prep with) 
;                                                                                     (noun-phrase (articles the) 
;                                                                                                  (nouns cat)))))))))

;; 4.46: Why is it important that the amb evaluator evaluates from left-right?
; *unparsed* is created left to right so the evaluator must do so as well

;; 4.47: Does this parse-verb-phrase simplify parsing?

; Doesn't work b/c parse-verb-phrase will call itself infinitely

;(define (parse-verb-phrase)
;  (amb (parse-word verbs)
;       (list 'verb-phrase
;             (parse-verb-phrase)
;             (parse-prepositional-phrase))))

;; 4.48: Include parser to handle additional building blocks to make more complex sentences

; adjectives
(define adjectives '(adjective smart witty thoughtful diligent handsome tall)) 
(define (complex-parse-simple-noun-phrase)       
  (amb (list 'simple-noun-phrase 
             (parse-word articles) 
             (parse-word nouns)) 
       (list 'simple-noun-phrase 
             (parse-word articles) 
             (parse-word adjectives) 
             (parse-word nouns))))

; adverbs
(define adverbs '(adverb quickly slowly lazily eagerly)) 
  
;; new function, not in book 
(define (complex-parse-simple-verb) 
  (amb (list 'simple-verb 
             (parse-word adverbs) 
             (parse-word verbs)) 
       (parse-word verbs))) 
  
;; replaces definition from book 
;; only difference is the call to parse-simple-verb 
(define (complex-parse-verb-phrase) 
  (define (maybe-extend verb-phrase) 
    (amb verb-phrase 
         (maybe-extend
          (list 'verb-phrase 
                verb-phrase 
                (parse-prepositional-phrase))))) 
  (maybe-extend (complex-parse-simple-verb))) 

;; 4.49: Instead of parsing words, generate words by ignoring input sentence and succeeding
;;       to generate the appropriate word
(define (list-amb li) 
  (if (null? li) 
      (amb) 
      (amb (car li) (list-amb (cdr li))))) 
  
(define (generate-parse-word word-list) 
  (require (not (null? *unparsed*))) 
  (require (memq (car *unparsed*) (cdr word-list))) 
  (let ((found-word (car *unparsed*))) 
    (set! *unparsed* (cdr *unparsed*)) 
    (list-amb (cdr word-list))))   ;; change

;(parse '(the student for the student studies for the student)