#lang scheme

;; TODO: error-checking, look for "all couples" in a file

(require mred (file "scrutineering.ss"))

(define events
  (list (make-event 1 'Silver 'International '(t))
        (make-event 2 'Silver 'International '(w q))))

(define judges
  (list "Russell Monk" "Judge 2" "judge 3" "judge 4" "judge 5" "judge 6"
        "judge 7" "judge 8" "judge 9" "judge 10" "judge 11" "judge 12" "judge 13" "judge 14"))

(define bronze-international-waltz
  (make-event 1 'Bronze 'International '(w)))

(define silver-international-waltz-quickstep
  (make-event 2 'Silver 'International '(w q)))

(define silver-international-wqt (make-event 3 'Silver 'International '(w q t)))

(define silver-int-cr (make-event 4 'Silver 'International '(c r)))

#;(define events
  (list bronze-international-waltz silver-international-waltz-quickstep silver-international-wqt silver-int-cr))

(define frame (new frame% [label "Judge Marks"]))

(define text-edited? #f)
(define (set-text-edited?! b)
  (set! text-edited? b)
  (send save-button enable text-edited?)
  (send revert-button enable text-edited?))

(define file-pattern "event-~a-round-~a-~a-judge-~a.marks")
(define file-pattern-regexp #rx"event-([0-9]+)-round-([0-9]+)-([a-z]+)-judge-([0-9]+).marks")
(define file-pattern-partial-regexp "event-~a-round-~a-([a-z]+)-judge-([0-9]+).marks")

(define selected-event-num 0)
(define selected-event (first events))
(define selected-round 1)
(define final? #f)
(define couples-to-recall #f)
(define selected-dance-num 0)
(define selected-judge 0)

(define (set-final?! b)
  (set! final? b)
  (send number-to-recall-text enable (not b))
  (send results-button enable (or b (and couples-to-recall (> couples-to-recall 4))))
  (send marks-message set-label (if b "Couples in order (1st, 2nd, 3rd, ...):" "Couples marks:")))

(define (check-discard)
  (or (not text-edited?)
      (case (message-box "Warning" "Discard changes?" frame '(yes-no))
        [(yes) (set-text-edited?! #f) #t]
        [(no) #f])))

(define event-selector
  (new choice%
       [label "Event:"]
       [choices (map event->string events)]
       [parent frame]
       [callback (lambda (this _)
                   (cond
                     [(check-discard)
                      (set! selected-event-num (send this get-selection))
                      (set! selected-event (list-ref events selected-event-num))
                      (send dance-selector clear)
                      (for-each
                       (lambda (dance) (send dance-selector append (dance->string dance)))
                       (event-dances selected-event))
                      (send dance-selector enable ((length (event-dances selected-event)) . > . 1))
                      (send judge-selector set-selection 0)
                      (send marks-text enable #f)
                      (try-load-text)]
                     [else
                      (send this set-selection selected-event-num)]))]))

(define round-pane
  (new horizontal-pane%
       [alignment '(center center)]
       [parent frame]))

(define round-number-text
  (new choice%
       [label "Round number:"]
       [parent round-pane]
       [choices (map number->string (list 1 2 3 4 5 6 7 8 9 10 11 12))]
       [callback (lambda (this _)
                   (cond
                     [(check-discard)
                      (set! selected-round (add1 (send this get-selection)))
                      (send judge-selector set-selection 0)
                      (send marks-text enable #f)
                      (try-load-text)]
                     [else
                      (send this set-selection (sub1 selected-round))]))]))

(define final-box
  (new check-box%
       [label "Final"]
       [parent round-pane]
       [callback (lambda (this _)
                   (set-final?! (send this get-value)))]))

(define number-to-recall-text
  (new text-field%
       [parent frame]
       [label "# of couples to recall:"]
       [callback (lambda (this _)
                   (set! couples-to-recall (string->number (send this get-value)))
                   (send results-button enable (or final? (and couples-to-recall (> couples-to-recall 4)))))]))

(define recalls-pane
  (new horizontal-pane%
       [alignment '(center center)]
       [parent frame]))

(define results-button
  (new button%
       [parent recalls-pane]
       [label "Compute Results"]
       [enabled #f]
       [callback
        (lambda _
          (let/ec esc
            (let* ([pattern (format file-pattern-partial-regexp
                                    (add1 selected-event-num) selected-round)]
                   [all-files (map path->string (directory-list))]
                   [matching-files (filter (lambda (fn) (regexp-match pattern fn))
                                           all-files)]
                   [output-file (format "event-~a-round-~a.results" (add1 selected-event-num) selected-round)]
                   [results-frame (new frame% [label "Results"])]
                   [results-text (new editor-canvas%
                                      [parent results-frame]
                                      [min-height 640]
                                      [min-width 640]
                                      [enabled #t]
                                      [editor (new text%)])])
              (with-output-to-file output-file #:mode 'text #:exists 'replace
                (lambda ()
                  (cond
                    [final?
                     (let* ([final-mark-sets (map filename->final-mark-set matching-files)]
                            [final-mark-sets-by-dance (map (lambda (dance)
                                                             (filter (lambda (final-mark-set)
                                                                       (eq? dance (final-mark-set-dance final-mark-set)))
                                                                     final-mark-sets))
                                                           (event-dances selected-event))]
                            [single-dance-results (map (lambda (dance-final-mark-set) (generate-final-report selected-event dance-final-mark-set))
                                                       final-mark-sets-by-dance)])
                       (when (> (length single-dance-results) 1)
                         (let ([linked-finalists (single-dance-results->linked-finalists single-dance-results)])
                           (generate-linked-final-report selected-event linked-finalists))))]
                    [else
                     (let* ([mark-sets (map filename->mark-set matching-files)]
                            [marks (map mark-set-recalled mark-sets)]
                            [mark-counts (sort (count-occurrences (apply append marks)) > #:key second)]
                            [all-couples (sort (remove-duplicates (append #;(validate-recalls (send (send all-couples-text get-editor) get-text))
                                                                          (apply append marks))) <)]
                            [possibilities (recalled mark-counts couples-to-recall)])
                       (if (= 1 (length possibilities))
                           (generate-report selected-event
                                            all-couples
                                            mark-sets couples-to-recall)
                           (esc (message-box "Error" (format "Cannot recall ~a couples.  Please choose either ~a or ~a."
                                                             couples-to-recall (length (first possibilities)) (length (second possibilities)))
                                             frame '(ok)))))])))
              (send (send results-text get-editor) change-style (make-object style-delta% 'change-family 'modern))
              (with-input-from-file output-file
                (lambda ()
                  (let loop ([str (read-line)])
                    (unless (eof-object? str)
                      (send (send results-text get-editor) insert str)                    
                      (send (send results-text get-editor) insert #\newline)
                      (loop (read-line))))))
              (send results-frame show #t))))]))

(define or-edit-marks-message
  (new message%
       [parent recalls-pane]
       [label "or edit marks below:"]))

(define dance-selector
  (new choice%
       [label "Dance:"]
       [stretchable-width true]
       [choices (map dance->string (event-dances (first events)))]
       [enabled ((length (event-dances (first events))) . > . 1)]
       [parent frame]
       [callback (lambda (this _)
                   (cond
                     [(check-discard)
                      (set! selected-dance-num (send this get-selection))
                      (send judge-selector set-selection 0)
                      (send marks-text enable #f)
                      (try-load-text)]
                     [else
                      (send this set-selection selected-dance-num)]))]))

(define judge-selector
  (new choice%
       [label "Judge:"]
       [parent frame]
       [stretchable-width true]
       [choices (cons "Select judge..." judges)]
       [callback (lambda (this _)
                   (cond
                     [(check-discard)
                      (set! selected-judge (send this get-selection))
                      (send marks-text enable (> selected-judge 0))
                      (send marks-text focus)
                      (try-load-text)]
                     [else
                      (send this set-selection selected-judge)]))]))

(define marks-message
  (new message%
       [parent frame]
       [label "Couples marked:"]
       [stretchable-width true]))

(define marks-text
  (new editor-canvas%
       [parent frame]
       [min-height 150]
       [enabled #f]
       [editor (new (class text%
                      (super-new)
                      (define/augment (after-delete start len)
                        (inner (void) after-delete)
                        (set-text-edited?! #t))
                      (define/augment (after-insert start len)
                        (inner (void) after-insert)
                        (set-text-edited?! #t))))]))

(define button-pane
  (new horizontal-pane%
       [alignment '(center center)]
       [parent frame]))

(define revert-button
  (new button%
       [parent button-pane]
       [enabled #f]
       [label "Revert"]
       [callback (lambda _
                   (when (check-discard)
                     (try-load-text)))]))

(define save-button
  (new button%
       [parent button-pane]
       [label "Save"]
       [enabled #f]
       [callback (lambda _
                   (let* ([marks-str (send (send marks-text get-editor) get-text)]
                          [marks (validate-recalls marks-str)])
                     (and marks round
                          (with-output-to-file
                              (let ([event-number (send event-selector get-selection)])
                                (format file-pattern
                                        (add1 event-number)
                                        selected-round
                                        (list-ref (event-dances (list-ref events event-number)) (send dance-selector get-selection))
                                        (send judge-selector get-selection)))
                            (lambda () (write marks-str))
                            #:mode 'text
                            #:exists 'replace)
                          (set-text-edited?! #f))))]))

(send frame show #t)

(define (reset-window)
  (send dance-selector set-selection 0)
  (send event-selector set-selection 0)
  (send round-number-text set-value "")
  (send judge-selector set-selection 0)
  (let ([marks-editor (send marks-text get-editor)])
    (send marks-editor delete 0 (send marks-editor last--position)))
  (set-text-edited?! #f))

(define (try-load-text)
  (let* ([dance (list-ref (event-dances (list-ref events selected-event-num)) (send dance-selector get-selection))]
         [judge-number (send judge-selector get-selection)]
         [marks-editor (send marks-text get-editor)]
         [proceed? (or (not text-edited?)
                       (case (message-box "Warning" "Abandon changes?" frame '(yes-no))
                         [(yes) #t]
                         [(no) #f]))])
    (when proceed?
      (send marks-editor delete 0 (send marks-editor last-position))
      (set-text-edited?! #f)
      (with-handlers
          ([exn:fail:filesystem? (lambda (e) (void) #;(fprintf (current-error-port) "~a~n" e))])
        (with-input-from-file
            (format file-pattern
                    (add1 selected-event-num)
                    selected-round
                    dance
                    judge-number)
          (lambda ()
            (let ([v (read)])
              (unless (eof-object? v)
                (send marks-editor insert (if (list? v) (string-join (map number->string v) " ") v))
                (set-text-edited?! #f)))))))))

(try-load-text)

(define (validate-recalls str)
  (let* ([couples (filter (lambda (s) ((string-length s) . > . 0))
                          (regexp-split #px"[^0-9]+" str))]
         [numbers (map string->number couples)])
    (cond
      [(empty? numbers)
       (message-box "Error" "Please enter at least one couple." frame '(ok))
       #f]
      [(not (andmap (lambda (x) ((number? x) (x . >= . 100) . and . (x . <= . 999))) numbers))
       (message-box "Error" "The couples must all have numbers between 101 and 999." frame '(ok))
       #f]
      [else numbers])))

(define (filename->mark-set filename)
  (let* ([matches (regexp-match file-pattern-regexp filename)]
         [event (string->number (second matches))]
         [round (string->number (third matches))]
         [dance (string->symbol (fourth matches))]
         [judge (string->number (fifth matches))])
    (with-input-from-file filename
      (lambda ()
        (let* ([marks (read)]
               [marks (if (list? marks) marks (validate-recalls marks))])
          (make-mark-set event round dance judge marks))))))
         
(define (filename->final-mark-set filename)
  (let* ([matches (regexp-match file-pattern-regexp filename)]
         [event (string->number (second matches))]
         [round (string->number (third matches))]
         [dance (string->symbol (fourth matches))]
         [judge (string->number (fifth matches))])
    (with-input-from-file filename
      (lambda ()
        (let* ([ranked-couples (read)]
               [ranked-couples (if (list? ranked-couples) ranked-couples (validate-recalls ranked-couples))])
          (make-final-mark-set event round dance judge ranked-couples))))))

;; Event, Round, Dance, Judge, Marks
