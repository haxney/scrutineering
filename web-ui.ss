;;; Scrutineering Tool User Interface, using the PLT Web Server
;;; Author: Greg Cooper (greg@cs.brown.edu)

#lang scheme

(require web-server/servlet)
(require web-server/servlet-env)

;; TODO: allow editing of registrations (maybe?)
;; TODO: show how many sets of judges' marks have been recorded for each dance in the round
;; TODO: error-checking

;; The skating system logic lives here:
(require (file "scrutineering.ss"))

;; Files with judges' marks and recalls go here:
(define db-path "/Users/ghc/scrutineering/")

;; Marks and recalls for each event go in a separate "event-??" directory
(define file-pattern "event-~a/round-~a-~a-judge-~a.marks")

;; Patterns for matching sets of marks:
(define file-pattern-regexp #rx"round-([0-9]+)-([a-z]+)-judge-([0-9]+).marks")
(define file-pattern-partial-regexp "round-~a-([a-z]+)-judge-([0-9]+).marks")

;; Events in this competition
(define events
  (list 
   ;; Rhythm
   (make-event 1 'Newcomer 'American '(c))
   (make-event 2 'Newcomer 'American '(r))
   (make-event 3 'Newcomer 'American '(sw))
   (make-event 4 'Bronze 'American '(c r))
   (make-event 5 'Bronze 'American '(sw))
   (make-event 6 'Silver 'American '(c r))
   (make-event 7 'Silver 'American '(sw))
   (make-event 8 'Gold 'American '(c r sw))
   (make-event 9 'Open 'American '(c r sw m b))
   ;; Standard
   (make-event 10 'Newcomer 'International '(w))
   (make-event 11 'Bronze 'International '(w))
   (make-event 12 'Bronze 'International '(q))
   (make-event 13 'Silver 'International '(w q))
   (make-event 14 'Silver 'International '(t))
   (make-event 15 'Gold 'International '(w q))
   (make-event 16 'Gold 'International '(f t))
   (make-event 17 'Open 'International '(w q f t v))
   ;; Smooth
   (make-event 18 'Newcomer 'International '(w))
   (make-event 19 'Newcomer 'International '(t))
   (make-event 20 'Newcomer 'International '(f))
   (make-event 21 'Bronze 'International '(w t))
   (make-event 22 'Bronze 'International '(f))
   (make-event 23 'Silver 'International '(w t))
   (make-event 24 'Silver 'International '(f))
   (make-event 25 'Gold 'International '(w t f))
   (make-event 26 'Open 'International '(w t f v))
   ;; Latin
   (make-event 27 'Newcomer 'International '(r))
   (make-event 28 'Bronze 'International '(c r))
   (make-event 29 'Silver 'International '(c r))
   (make-event 30 'Silver 'International '(j))
   (make-event 31 'Gold 'International '(c r))
   (make-event 32 'Gold 'International '(sa j))
   (make-event 33 'Open 'International '(c r sa j p))))

;; Check that all the event directories exist, and that there are registrations for each event.
(for-each
 (lambda (event)
   (let ([path (string-append db-path "event-" (number->string (event-number event)))])
     (unless (directory-exists? path)
       (error 'startup "~a does not exist.  Please either create it or fix the db-path variable to point to the correct location." path)))
   (let ([path (string-append db-path "event-" (number->string (event-number event)) "/round-1.couples")])
     (unless (file-exists? path)
       (error 'startup "~a does not exist.  Please either create it or fix the db-path variable to point to the correct location." path))))
 events)

;; Judges by name (judge numbers are assumed to go from 1 to n).
(define judges
  (list "Boris Brevde"
        "Istvan Cserven"
        "Randy Deats"
        "David Hannigan"
        "Tibor Kerekes"
        "Emil Ioukhnikov"
        "Ruta Ioukhnikov"
        "Janka Kisucky"
        "Kathy St. Jean"
        "Patrik Kisucky"
        "Christopher Plumley"
        "Helle Rusholt"
        "Peter Zaccone"))

;; Returns the number of a given judge (the judge's position in the judges' list, starting from 1).
(define (judge-number judge)
  (let loop ([judges judges] [index 1])
    (cond
      [(empty? judges) (error "unknown judge: ~a" judge)]
      [(string=? judge (first judges)) index]
      [else (loop (rest judges) (add1 index))])))

;; Given an event and a filename, returns a mark-set representing the marks in the file
;; (which are assumed to be for the given event).
(define ((filename->mark-set event) filename)
  (let* ([matches (regexp-match file-pattern-regexp filename)]
         [round (string->number (second matches))]
         [dance (string->symbol (third matches))]
         [judge (string->number (fourth matches))])
    (with-input-from-file filename
      (lambda ()
        (let* ([marks (rest (read))])
          (make-mark-set event round dance judge marks))))))

;; Given an event and a filename, returns a final-msrk-set representing the final-round marks
;; from the file (which are assumed to be for the given event).
(define ((filename->final-mark-set event) filename)
  (let* ([matches (regexp-match file-pattern-regexp filename)]
         [round (string->number (second matches))]
         [dance (string->symbol (third matches))]
         [judge (string->number (fourth matches))])
    (with-input-from-file filename
      (lambda ()
        (let* ([ranked-couples (rest (read))])
          (make-final-mark-set event round dance judge ranked-couples))))))

;; A lock that ensures serial access to the file system.
(define mutex (make-semaphore 1))

;; Returns the list of couples in the given round of the given event.
(define (couples-in-round event round)
  (let ([filename (string-append db-path (format "event-~a/round-~a.couples" (event-number event) round))])
    (if (file-exists? filename)
        (let ([couples (with-input-from-file filename read)])
          (if (eof-object? couples)
              empty
              couples))
        empty)))

;; Returns two values: the highest round number (so far) for the given event,
;; and the number of couples in that round.
(define (num-couples-in-latest-round event)
  (let loop ([round 1] [last-num-couples 0])
    (let ([couples (couples-in-round event round)])
      (if (zero? (length couples))
          (values (max 1 (sub1 round)) last-num-couples)
          (loop (add1 round) (length couples))))))

;; Assuming the recalls for the given event / round have already been computed, generates
;; an HTML table showing the marks and recalled couples.
(define (generate-recalls-table event round)
  (let* ([pattern (format file-pattern-partial-regexp round)]
         [path (format "~aevent-~a/" db-path (event-number event))]
         [all-files (map path->string (directory-list path))]
         [matching-files (map (lambda (fn) (string-append path fn))
                              (filter (lambda (fn) (regexp-match pattern fn))
                                      all-files))]
         [mark-sets (map (filename->mark-set event) matching-files)]
         [marks (map mark-set-recalled mark-sets)]
         [mark-counts (sort (count-occurrences (apply append marks)) > #:key second)]
         [couples (couples-in-round event round)]
         [all-couples (sort (remove-duplicates (append couples (apply append marks))) <)]
         [couples-to-recall (length (couples-in-round event (add1 round)))])
    (let-values ([(recalled page) (generate-sxml-report event round all-couples mark-sets couples-to-recall)])
      page)))

;; Determines whether all the elements of a list are equal.
(define (all-equal lsts)
  (andmap (lambda (lst) (equal? lst (first lsts))) (rest lsts)))

;; Infers whether the given round of the given event could be a final (assuming the given
;; judges' marks have been provided).
(define could-be-final?
  (case-lambda
    [(event round)
     (let-values ([(latest-round _) (num-couples-in-latest-round event)])
       (and (= round latest-round)
            (or (= round 1)
                (and (> round 1) (< (length (couples-in-round event round)) 9)))))]
    [(event round marks)
     (and
      (could-be-final? event round)
      (all-equal (map (lambda (lst) (sort lst <)) marks))
      (or (and (= round 1) (< (length (first marks)) 9))
          (and (> round 1) (= (length (first marks)) (length (couples-in-round event round))))))]))

;; Infers whether the given event and round could be a preliminary round (assuming the given
;; judges' marks have been provided).
(define (could-be-preliminary? event round marks)
  (or (and (= round 1) (> (length (first marks)) 5))
      (and (> round 1) (< (length (first marks)) (length (couples-in-round event round))))))      

;; Map of username:password combinations to "privilege levels".
;; bbdt is intended for normal use; it can edit marks for the current round, but not previous rounds.
;; admin can make changes to previous as well as current rounds (though this shouldn't need to happen).
;; guest (no password) can view marks and results but cannot make any changes.
(define auth-map
  #hash(("admin:jlttrm09" . admin)
        ("bbdt:sayles" . bbdt)))

;; Displays the initial login page.
(define (login request)
  (send/suspend/dispatch
   (lambda (make-url)
     `(html
       (head (title "Brown Comp 2009"))
       (body
        (h3 "Log in to continue:")
        (form ((action ,(make-url process-login))
               (method "post"))
              (div "Username: " (input ((name "username") (type "text"))))
              (div "Password: " (input ((name "password") (type "password"))))
              (div (input ((type "submit") (value "Continue")))))
        (div (a ((href ,(make-url (all-events 'guest)))) "Continue as guest") " (read-only)"))))))

;; Displays the initial login page.
(define (process-login request)
  (let* ([username (extract-binding/single 'username (request-bindings request))]
         [password (extract-binding/single 'password (request-bindings request))])
    ((all-events (hash-ref auth-map (format "~a:~a" username password) 'guest)) request)))

;; Main page listing all events

(define ((all-events privilege-level) request)
  (send/suspend/dispatch
   (lambda (make-url)
     `(html
       (head (title "Brown Comp 2009"))
       (body
        (h5 (div ,(format "Logged in as ~a. " privilege-level) (a ((href "/servlets/comp")) "Back to start page.")))
        (h1 "Brown Ballroom Competition 2009")
        (ul ,@(map (lambda (event)
                     (let-values ([(round num-couples) (num-couples-in-latest-round event)])
                       `(li (a ((href ,(make-url (process-event privilege-level event))))
                               ,(format "~a (~a couples in round ~a)" (event->string event) num-couples round)))))
                   events)))))))

(define (mode lon)
  (let ([sorted (sort lon <)])
    (let loop ([n (first sorted)] [lst (rest sorted)] [count 1] [max-count 1])
      (void))))

;; Returns the average of a list of numbers (rounded up).
(define (average lon)
  (ceiling (/ (apply + lon) (length lon))))

;; Serves a page for the given event (including all sub-pages).

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                    ;     
;                                                                                                    ;     
;   ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;;;;   ;;;;;           ;;;;  ;;;  ;;;  ;;;;   ;; ;;   ;;;;;; 
;    ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;    ;   ;;  ;   ;     
;    ;   ;   ;      ;    ;  ;       ;;;;;;   ;;;;    ;;;;           ;;;;;;   ;  ;   ;;;;;;   ;   ;   ;     
;    ;   ;   ;      ;    ;  ;       ;            ;       ;          ;        ;  ;   ;        ;   ;   ;     
;    ;   ;   ;      ;    ;  ;    ;  ;       ;    ;  ;    ;          ;         ;;    ;        ;   ;   ;   ; 
;    ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;  ;;;;;   ;;;;;            ;;;;;    ;;     ;;;;;  ;;; ;;;   ;;;  
;    ;                                                                                                     
;    ;                                                                                                     
;   ;;;                                                                                                    
;                                                                                                          
(define ((process-event privilege-level event) request)
  (send/suspend/dispatch
   (lambda (make-url)
     (local
       [(define event-string (event->string event))
        (define event-links
          `((h1 (a ((href ,(make-url (all-events privilege-level)))) "Brown Ballroom Competition 2009"))
            (h5 (div ,(format "Logged in as ~a. " privilege-level) (a ((href "/servlets/comp")) "Back to start page.")))))
        (define event-header `(h2 ,event-string))
        (define (generate-event-page)
          `(html
            (head (title ,event-string))
            (body
             ,@(reverse event-links)
             ,event-header
             (p)
             ;; Scan for files with results.  List them for viewing, plus one more for editing.
             ,@(let loop ([round 1])
                 (let ([filename (string-append db-path (format "event-~a/round-~a.couples" (event-number event) round))])
                   (cond
                     [(file-exists? filename)
                      (let ([couples (with-input-from-file filename read)])
                        (cons `(div (a ((href ,(make-url (process-round round couples))))
                                       ,(format "Round ~a (~a couples)" round (length couples))))
                              (loop (add1 round))))]
                     [else empty])))
             (p))))
        ;                                                                                                          
        ;                                                                                                          
        ;                                                                                                          
        ;                                                                                                       ;; 
        ;                                                                                                        ; 
        ;                                                                                                        ; 
        ;   ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;;;;   ;;;;;          ;; ;;;   ;;;;   ;;  ;;  ;; ;;    ;;; ; 
        ;    ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;    ;  ;    ;  ;;;;;;   ;;     ;    ;   ;   ;   ;;  ;  ;   ;; 
        ;    ;   ;   ;      ;    ;  ;       ;;;;;;   ;;;;    ;;;;            ;      ;    ;   ;   ;   ;   ;  ;    ; 
        ;    ;   ;   ;      ;    ;  ;       ;            ;       ;           ;      ;    ;   ;   ;   ;   ;  ;    ; 
        ;    ;   ;   ;      ;    ;  ;    ;  ;       ;    ;  ;    ;           ;      ;    ;   ;  ;;   ;   ;  ;   ;; 
        ;    ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;  ;;;;;   ;;;;;           ;;;;;    ;;;;     ;; ;; ;;; ;;;  ;;; ;;
        ;    ;                                                                                                     
        ;    ;                                                                                                     
        ;   ;;;                                                                                                    
        ;                                                                                                          
        (define ((process-round round couples) request)
          (local
            [(define round-links (cons `(h2 (a ((href ,(make-url (process-event privilege-level event)))) ,event-string)) event-links))
             (define round-header `(h3 ,(format "Round ~a" round)))
             (define pattern (format file-pattern-partial-regexp round))
             (define path (format "~aevent-~a/" db-path (event-number event)))
             (define all-files (map path->string (directory-list path)))
             (define matching-files (map (lambda (fn) (string-append path fn))
                                         (filter (lambda (fn) (regexp-match pattern fn))
                                                 all-files)))
             (define marks-count (length matching-files))
             (define mark-sets (map (filename->mark-set event) matching-files))
             (define marks (map mark-set-recalled mark-sets))
             (define chosen (length (couples-in-round event (add1 round))))
             (define (generate-round-page)
               `(html
                 (head (title ,event-string))
                 (body
                  ,@(reverse round-links)
                  ,round-header
                  ,(format "(~a couples ~a)" (length couples) (if (= round 1) "registered" "recalled"))
                  (p)
                  ,@(map (lambda (dance) `(div (a ((href ,(make-url (process-dance dance))))
                                                  ,(format "~a" (dance->string dance)))))
                         (event-dances event))
                  (p)
                  (div ,(format "~a sets of judge marks have been recorded. " marks-count)
                       ,(if (positive? chosen) (format "~a couples were recalled to round ~a." chosen (add1 round)) ""))
                  ,(if (positive? chosen)
                       `(div (a ((href ,(make-url display-recalls))) "[Show recalls]"))
                       "")
                  ;; if results not yet computed, then bbdt or admin can compute
                  ;; if results computed, then only admin can recompute
                  ,@(if (and (>= marks-count (* (length (event-dances event)) 3))
                             (could-be-preliminary? event round marks)
                             (or (eq? privilege-level 'admin)
                                 (and (eq? privilege-level 'bbdt) (zero? chosen))))
                        (let* ([mark-counts (sort (count-occurrences (apply append marks)) > #:key second)]
                               [all-couples (sort (remove-duplicates (append couples (apply append marks))) <)]
                               [couples-to-recall (average (map length marks))]
                               [possibilities (recalled mark-counts couples-to-recall)])
                          (append
                           (map (lambda (n)
                                  `(div (a ((href ,(make-url (compute-results n))))
                                           ,(format "[~a recalls for ~a]"
                                                    (if (zero? chosen) "Compute" "Recompute")
                                                    n))))
                                (map length possibilities))
                           (list `(form ((action ,(make-url (compute-results)))
                                         (method "post"))
                                        "Or enter a different recall target: " (input ((type "text") (name "count")))
                                        (input ((type "submit")))))))
                        empty)
                  ,(if (and (>= marks-count (* (length (event-dances event)) 3))
                            (could-be-final? event round marks))
                       `(div (a ((href ,(make-url (compute-results 'final)))) "[Compute / Display Final Results]"))
                       '(div))
                  (p))))
             (define (display-recalls request)
               (let ([recalled (couples-in-round event (add1 round))])
                 `(html
                   (head (title ,event-string))
                   (body
                    ,@(reverse round-links)
                    (h3 (a ((href ,(make-url (process-round round couples)))) ,(format "Round ~a" round)))
                    (h3 ,(format "~a couples recalled to " (length recalled))
                        (a ((href ,(make-url (process-round (add1 round) recalled)))) ,(format "Round ~a" (add1 round)))
                        ":")
                    (ol ,@(map (lambda (couple) `(li ,(number->string couple))) recalled))))))
             ;                                                                                                                          
             ;                                                                                                                          
             ;                                                                                                                          
             ;                                                                                                     ;;                   
             ;                                            ;                                                         ;     ;             
             ;                                            ;                                                         ;     ;             
             ;    ;;; ;   ;;;;  ;; ;  ;  ;; ;;   ;;  ;;  ;;;;;;   ;;;;           ;; ;;;   ;;;;    ;;;;;  ;;  ;;     ;    ;;;;;;   ;;;;; 
             ;   ;   ;;  ;    ;  ;; ;; ;  ;;  ;   ;   ;   ;      ;    ;  ;;;;;;   ;;     ;    ;  ;    ;   ;   ;     ;     ;      ;    ; 
             ;   ;       ;    ;  ;  ;  ;  ;   ;   ;   ;   ;      ;;;;;;           ;      ;;;;;;   ;;;;    ;   ;     ;     ;       ;;;;  
             ;   ;       ;    ;  ;  ;  ;  ;   ;   ;   ;   ;      ;                ;      ;            ;   ;   ;     ;     ;           ; 
             ;   ;    ;  ;    ;  ;  ;  ;  ;   ;   ;  ;;   ;   ;  ;                ;      ;       ;    ;   ;  ;;     ;     ;   ;  ;    ; 
             ;    ;;;;    ;;;;  ;;; ;; ;  ;;;;     ;; ;;   ;;;    ;;;;;          ;;;;;    ;;;;;  ;;;;;     ;; ;; ;;;;;;;   ;;;   ;;;;;  
             ;                            ;                                                                                             
             ;                            ;                                                                                             
             ;                           ;;;                                                                                            
             ;                                                                                                                          
             (define ((compute-results [count #f]) request)
               (local
                 [(define couples-to-recall (or count (string->number (extract-binding/single 'count (request-bindings request)))))]
                 (cond
                   [(eq? count 'final)  ;; indicates that this is a final
                    (let* ([final-mark-sets (map (filename->final-mark-set event) matching-files)]
                           [final-mark-sets-by-dance (map (lambda (dance)
                                                            (filter (lambda (final-mark-set)
                                                                      (eq? dance (final-mark-set-dance final-mark-set)))
                                                                    final-mark-sets))
                                                          (event-dances event))]
                           [preliminary-results (build-list (sub1 round) (lambda (r) (generate-recalls-table event (add1 r))))]
                           [single-dance-results/bodies
                            (map (lambda (dance-final-mark-set) (generate-sxml-final-report event dance-final-mark-set))
                                 final-mark-sets-by-dance)]
                           [single-dance-results (map first single-dance-results/bodies)]
                           [bodies (map second single-dance-results/bodies)]
                           [linked (if (> (length single-dance-results) 1)
                                       (let ([linked-finalists (single-dance-results->linked-finalists single-dance-results)])
                                         (generate-sxml-linked-final-report event linked-finalists))
                                       '())])
                      `(html
                        (head (title ,event-string))
                        (body
                         ,@(reverse round-links)
                         ,@(apply append preliminary-results)
                         ,round-header
                         ,@(apply append bodies)
                         ,@linked)))]
                   [else
                    (let* ([mark-sets (map (filename->mark-set event) matching-files)]
                           [marks (map mark-set-recalled mark-sets)]
                           [mark-counts (sort (count-occurrences (apply append marks)) > #:key second)]
                           [all-couples (sort (remove-duplicates (append couples (apply append marks))) <)]
                           [possibilities (recalled mark-counts couples-to-recall)])
                      (if (= 1 (length possibilities))
                          (let-values ([(recalled page) (generate-sxml-report event round all-couples mark-sets couples-to-recall)])
                            (with-output-to-file (string-append db-path (format "event-~a/round-~a.couples" (event-number event) (add1 round)))
                              #:mode 'text #:exists 'replace
                              (lambda () (write recalled)))
                            (display-recalls request)
                            #;`(html
                              (head (title ,event-string))
                              (body
                               ,@(reverse round-links)
                               ,round-header
                               (h3 ,(format "~a couples recalled to round ~a:" (length recalled) (add1 round)))
                               (ol ,@(map (lambda (couple) `(li ,(number->string couple))) recalled)))))
                          (let ([count-1 (length (first possibilities))]
                                [count-2 (length (second possibilities))])
                            `(html (head (title ,event-string))
                                   (body
                                    ,@(reverse round-links)
                                    ,round-header
                                    (p)
                                    ,(format "Cannot recall ~a couples.  Please choose one of the options below."
                                             couples-to-recall)
                                    (div (a ((href ,(make-url (compute-results count-1))))
                                            ,(format "[Recall ~a]" count-1)))
                                    (div (a ((href ,(make-url (compute-results count-2))))
                                            ,(format "[Recall ~a]" count-2))))))))])))
             ;                                                                                                          
             ;                                                                                                          
             ;                                                                                                          
             ;                                                                       ;;                                 
             ;                                                                        ;                                 
             ;                                                                        ;                                 
             ;   ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;;;;   ;;;;;           ;;; ;   ;;;;   ;; ;;    ;;; ;   ;;;;  
             ;    ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;    ;  ;    ;  ;;;;;;  ;   ;;  ;    ;   ;;  ;  ;   ;;  ;    ; 
             ;    ;   ;   ;      ;    ;  ;       ;;;;;;   ;;;;    ;;;;           ;    ;   ;;;;;   ;   ;  ;       ;;;;;; 
             ;    ;   ;   ;      ;    ;  ;       ;            ;       ;          ;    ;  ;    ;   ;   ;  ;       ;      
             ;    ;   ;   ;      ;    ;  ;    ;  ;       ;    ;  ;    ;          ;   ;;  ;   ;;   ;   ;  ;    ;  ;      
             ;    ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;  ;;;;;   ;;;;;            ;;; ;;  ;;; ;; ;;; ;;;  ;;;;    ;;;;; 
             ;    ;                                                                                                     
             ;    ;                                                                                                     
             ;   ;;;                                                                                                    
             ;                                                                                                          
             (define ((process-dance dance) request)
               (local
                 [(define dance-string (dance->string dance))
                  (define dance-links (cons `(h3 (a ((href ,(make-url (process-round round couples))))
                                                    ,(format "Round ~a" round))) round-links))
                  (define dance-header `(h4 ,dance-string))
                  (define (generate-dance-page)
                    `(html
                      (head (title ,event-string))
                      (body
                       ,@(reverse dance-links)
                       ,dance-header
                       (p)
                       ,@(map (lambda (judge number)
                                (let* ([filename (string-append
                                                  db-path
                                                  (format file-pattern (event-number event) round dance (judge-number judge)))]
                                       [content
                                        (if (file-exists? filename)
                                            (with-input-from-file filename read)
                                            (list 0))]
                                       [version (first content)]
                                       [marks (rest content)])
                                  `(div (a ((href ,(make-url (process-judge judge (string-join (map number->string marks) " ") version))))
                                           ,(cond
                                              [(empty? marks) (format "~a. ~a" number judge)]
                                              [else (format "~a. ~a (~a marks)" number judge (length marks))])))))
                              judges
                              (build-list (length judges) add1))
                       (p))))
                  ;                                                                                                          
                  ;                                                                                                          
                  ;                                                                                                          
                  ;                                                                                       ;;                 
                  ;                                                                      ;                 ;                 
                  ;                                                                                        ;                 
                  ;   ;; ;;   ;; ;;;   ;;;;    ;;; ;   ;;;;    ;;;;;   ;;;;;          ;;;;;   ;;  ;;   ;;; ;   ;;; ;;  ;;;;  
                  ;    ;;  ;   ;;     ;    ;  ;   ;;  ;    ;  ;    ;  ;    ;  ;;;;;;      ;    ;   ;  ;   ;;  ;   ;;  ;    ; 
                  ;    ;   ;   ;      ;    ;  ;       ;;;;;;   ;;;;    ;;;;               ;    ;   ;  ;    ;  ;    ;  ;;;;;; 
                  ;    ;   ;   ;      ;    ;  ;       ;            ;       ;              ;    ;   ;  ;    ;  ;    ;  ;      
                  ;    ;   ;   ;      ;    ;  ;    ;  ;       ;    ;  ;    ;              ;    ;  ;;  ;   ;;  ;   ;;  ;      
                  ;    ;;;;   ;;;;;    ;;;;    ;;;;    ;;;;;  ;;;;;   ;;;;;               ;     ;; ;;  ;;; ;;  ;;; ;   ;;;;; 
                  ;    ;                                                                  ;                        ;         
                  ;    ;                                                                  ;                        ;         
                  ;   ;;;                                                             ;;;;                     ;;;;          
                  ;                                                                                                          
                  (define ((process-judge judge marks version (error "")) request)
                    (local
                      [(define judge-links (cons `(h4 (a ((href ,(make-url (process-dance dance))))
                                                         ,dance-string)) dance-links))
                       (define judge-header `(h5 ,(format "~a. ~a" (judge-number judge) judge)))
                       (define filename
                         (string-append
                          db-path
                          (format file-pattern (event-number event) round dance (judge-number judge))))
                       (define (generate-judge-page)
                         `(html
                           (head (title ,event-string))
                           (body
                            ,@(reverse judge-links)
                            ,judge-header
                            (p)
                            ,error
                            (p)
                            "All couples: " ,(string-join (map number->string couples) " ")
                            (div ,(format "Couples marked by judge ~a (~a)~a:" (judge-number judge) judge
                                          (if (could-be-final? event round) " (in order 1st, 2nd, ... if this is a final)" "")))
                            ,(let-values ([(latest-round _) (num-couples-in-latest-round event)])
                               (cond
                                 [(or (eq? privilege-level 'admin)
                                      (and (eq? privilege-level 'bbdt) (= latest-round round)))
                                  `(form ((action ,(make-url check-marks))
                                          (method "post"))
                                         (div (textarea ((name "marks") (cols "40") (rows "8")) ,(format "~a" marks)))
                                         (input ((type "submit") (value "Save"))))]
                                 [else `(div ,(format "~a" marks))])))))
                       ;                                                                                  
                       ;                                                                                  
                       ;                                                                                  
                       ;                                                                   ;;             
                       ;                                                                    ;             
                       ;                                                                    ;             
                       ;    ;;;;;   ;;;;  ;;;  ;;;  ;;;;          ;; ;  ;   ;;;;   ;; ;;;   ; ;;;   ;;;;; 
                       ;   ;    ;  ;    ;  ;    ;  ;    ;  ;;;;;;  ;; ;; ; ;    ;   ;;      ; ;    ;    ; 
                       ;    ;;;;    ;;;;;   ;  ;   ;;;;;;          ;  ;  ;  ;;;;;   ;       ;;      ;;;;  
                       ;        ;  ;    ;   ;  ;   ;               ;  ;  ; ;    ;   ;       ; ;         ; 
                       ;   ;    ;  ;   ;;    ;;    ;               ;  ;  ; ;   ;;   ;       ;  ;   ;    ; 
                       ;   ;;;;;    ;;; ;;   ;;     ;;;;;         ;;; ;; ;  ;;; ;; ;;;;;   ;;  ;;; ;;;;;  
                       ;                                                                                  
                       ;                                                                                  
                       ;                                                                                  
                       ;                                                                                  
                       (define ((save-marks accepted) request)
                         (call-with-semaphore
                          mutex
                          (lambda ()
                            (let* ([exists? (file-exists? filename)]
                                   [old-content (if exists? (with-input-from-file filename read) (list 0))]
                                   [old-version (first old-content)]
                                   [old-marks (rest old-content)])
                              (cond
                                [(not (= old-version version))
                                 ;; OCC check failed
                                 ((process-judge judge (string-join (map number->string old-marks) " ") old-version
                                                 "Error: someone else has edited these marks concurrently.  Please try again.")
                                  request)]
                                [(and (empty? accepted) exists?)
                                 (delete-file filename)
                                 ((process-dance dance) request)]
                                [(empty? accepted)
                                 ((process-dance dance) request)]
                                [else
                                 (with-output-to-file filename
                                   (lambda () (write (cons (add1 version) accepted)))
                                   #:mode 'text #:exists 'replace)
                                 ((process-dance dance) request)])))))
                       ;                                                                                          
                       ;                                                                                          
                       ;                                                                                          
                       ;           ;;                      ;;                                      ;;             
                       ;            ;                       ;                                       ;             
                       ;            ;                       ;                                       ;             
                       ;    ;;; ;   ; ;;    ;;;;    ;;; ;   ; ;;;         ;; ;  ;   ;;;;   ;; ;;;   ; ;;;   ;;;;; 
                       ;   ;   ;;   ;;  ;  ;    ;  ;   ;;   ; ;    ;;;;;;  ;; ;; ; ;    ;   ;;      ; ;    ;    ; 
                       ;   ;        ;   ;  ;;;;;;  ;        ;;             ;  ;  ;  ;;;;;   ;       ;;      ;;;;  
                       ;   ;        ;   ;  ;       ;        ; ;            ;  ;  ; ;    ;   ;       ; ;         ; 
                       ;   ;    ;   ;   ;  ;       ;    ;   ;  ;           ;  ;  ; ;   ;;   ;       ;  ;   ;    ; 
                       ;    ;;;;   ;;; ;;;  ;;;;;   ;;;;   ;;  ;;;        ;;; ;; ;  ;;; ;; ;;;;;   ;;  ;;; ;;;;;  
                       ;                                                                                          
                       ;                                                                                          
                       ;                                                                                          
                       ;                                                                                          
                       (define (check-marks request)
                         (local
                           [(define user-input (extract-binding/single 'marks (request-bindings request)))
                            (define allow-unknown? (= round 1))
                            (define result (validate-recalls user-input couples allow-unknown?))
                            (define accepted (validate-recalls-result-accepted result))
                            (define duplicates (validate-recalls-result-duplicates result))
                            (define warnings (validate-recalls-result-warnings result))
                            (define errors (validate-recalls-result-errors result))
                            (define any-anomalies?
                              (not (and (empty? duplicates) (empty? warnings) (empty? errors))))
                            (define message
                              (filter
                               (lambda (x) x)
                               (list
                                (and (not (empty? duplicates))
                                     `(div ((class "warning"))
                                           "The following couples were entered more than once (will be counted once): "
                                           ,(string-join (map number->string duplicates) " ")))
                                (and (not (empty? warnings))
                                     `(div ((class "warning"))
                                           "The following couples were not expected in this round (will still be counted): "
                                           ,(string-join (map number->string warnings) " ")))
                                (and (not (empty? errors))
                                     `(div "The following numbers are invalid (will be ignored): "
                                           ,(string-join (map number->string errors) " "))))))
                            ;                                                                                                          
                            ;                                                                                                          
                            ;                                                                                                          
                            ;                              ;;;                                                          ;;             
                            ;                             ;        ;                                                     ;             
                            ;                             ;                                                              ;             
                            ;    ;;; ;   ;;;;   ;; ;;   ;;;;;;   ;;;    ;; ;;; ;; ;  ;         ;; ;  ;   ;;;;   ;; ;;;   ; ;;;   ;;;;; 
                            ;   ;   ;;  ;    ;   ;;  ;    ;        ;     ;;     ;; ;; ; ;;;;;;  ;; ;; ; ;    ;   ;;      ; ;    ;    ; 
                            ;   ;       ;    ;   ;   ;    ;        ;     ;      ;  ;  ;         ;  ;  ;  ;;;;;   ;       ;;      ;;;;  
                            ;   ;       ;    ;   ;   ;    ;        ;     ;      ;  ;  ;         ;  ;  ; ;    ;   ;       ; ;         ; 
                            ;   ;    ;  ;    ;   ;   ;    ;        ;     ;      ;  ;  ;         ;  ;  ; ;   ;;   ;       ;  ;   ;    ; 
                            ;    ;;;;    ;;;;   ;;; ;;;  ;;;;   ;;;;;;; ;;;;;  ;;; ;; ;        ;;; ;; ;  ;;; ;; ;;;;;   ;;  ;;; ;;;;;  
                            ;                                                                                                          
                            ;                                                                                                          
                            ;                                                                                                          
                            ;                                                                                                          
                            (define (confirm-marks)
                              `(html
                                (head (title ,event-string))
                                (body
                                 ,@(reverse judge-links)
                                 ,judge-header
                                 (p)
                                 ,@message
                                 (p)
                                 (div (a ((href ,(make-url (save-marks accepted)))) "[Save Marks]")
                                      " (" ,(string-join (map number->string accepted) " ") ")")
                                 (div (a ((href ,(make-url (process-judge judge user-input version)))) "[Edit Marks Again]")))))]
                           ;; body of check-judge-marks
                           (if any-anomalies?
                               (confirm-marks)
                               ((save-marks accepted) request))))]
                      ;; body of process-judge
                      (generate-judge-page)))]
                 ;; body of process-dance
                 (generate-dance-page)))]
            ;; body of process-round
            (generate-round-page)))]
       ;; body of process-event
       (generate-event-page)))))

(serve/servlet login #:port 8080 #:listen-ip #f #:servlet-path "/servlets/comp")
