;;; An implementation of the skating system.
;;; Author: Greg Cooper (greg@cs.brown.edu)

#lang scheme

(require scheme/base scheme/list scheme/match (only-in srfi/1 find))

;; makes it easy to see inside all of our structures
(define inspector (make-inspector))

;; a dance is one of w, q, t, f, v, c, r, sa, j, p, sw, m, b
(define all-dances '(w q f t v c r sa j p sw m b))

;; returns a two-character abbreviation of the dance name
(define (dance->short-string dance)
  (case dance
    [(w)  "W "]
    [(q)  "Q "]
    [(t)  "T "]
    [(f)  "F "]
    [(v)  "V "]
    [(c)  "C "]
    [(r)  "R "]
    [(sa) "Sa"]
    [(j)  "J "]
    [(p)  "P "]
    [(sw) "Sw"]
    [(m)  "M "]
    [(b)  "B "]))

;; returns the full dance name
(define (dance->string dance)
  (case dance
    [(w)  "Waltz"]
    [(q)  "Quickstep"]
    [(t)  "Tango"]
    [(f)  "Foxtrot"]
    [(v)  "Viennese Waltz"]
    [(c)  "Cha-cha"]
    [(r)  "Rumba"]
    [(sa) "Samba"]
    [(j)  "Jive"]
    [(p)  "Paso Doble"]
    [(sw) "Swing"]
    [(m)  "Mambo"]
    [(b)  "Bolero"]))

;; adds a space to the beginning of a one-digit number
(define (pad2 num)
  (format (if (< num 10) " ~a" "~a") num))

;; number is the number of the event
;; level is like 'newcomer, 'bronze, 'silver, 'gold, 'open
;; style is one of 'smooth, 'standard, 'rhythm, 'latin
;; dances is a list of dances
(define-struct event (number level style dances))

(define-struct comp (events judges))

;; number is the couple's number
;; cumulative marks is a list where the value at position i (starting from 1)
;;   is the number of judges who marked this couple at ith or better
;; cumulative sums is a list where the value at position i (starting from 1)
;;   is the sum of the couple's places at ith or better
(define-struct finalist (number cumulative-marks cumulative-sums) #:inspector inspector)

;; inherited fields treat individual dances as marks
;; places is a list, ordered by dance, of the place the finalist received for that dance
;; sum is the sum of the finalist's places
;; as-single-dance is another (non-linked) "finalist" structure treating the marks from all dances
;;   as a single dance
(define-struct (linked-finalist finalist) (places sum as-single-dance [r10 #:mutable #:auto] [r11 #:mutable #:auto]) #:inspector inspector)

;; a mark set is one judge's marks for a dance in a round of an event
;; event is the event number
;; round is the round number
;; dance is the dance
;; judge is the judge's number
;; recalled is the list of couples (numbers) the judge called back
(define-struct mark-set (event round dance judge recalled) #:inspector inspector)

;; event is the event number
;; round in the round number
;; dance is the dance
;; judge is the judge's number
;; ranked-couples is the list of couples, ordered by this judge's placement (1st, 2nd, ...)
(define-struct final-mark-set (event round dance judge ranked-couples) #:inspector inspector)

;; result of parsing a single judge's marks from a string
;;
;; all fields are lists of numbers:
;; accepted are couples with numbers in the range [100, 999]
;; duplicates are those that appear more than once
;; warnings are those not expected (registered for round 1, or recalled from a previous round)
;; errors are those not in the range [100, 999]
(define-struct validate-recalls-result (accepted duplicates warnings errors))

;; validate-recalls : string list[number] boolean -> validate-recalls-results
(define (validate-recalls str all-couples [allow-unknown? #t])
  (let* ([split-str (filter (lambda (s) ((string-length s) . > . 0))
                          (regexp-split #px"[^0-9]+" str))]
         [numbers (map string->number split-str)])
    (let loop ([couples (reverse numbers)] [accepted empty] [duplicates empty] [warnings empty] [errors empty])
      (if (cons? couples)
          (let ([couple (first couples)])
            (cond
              [(memq couple (rest couples))
               (loop (rest couples) accepted (cons couple duplicates) warnings errors)]
              [(memq couple all-couples)
               (loop (rest couples) (cons couple accepted) duplicates warnings errors)]
              [(allow-unknown? (number? couple) (couple . >= . 100) . and . (couple . <= . 999))
               (loop (rest couples) (cons couple accepted) duplicates (cons couple warnings) errors)]
              [else
               (loop (rest couples) accepted duplicates warnings (cons couple errors))]))
          (make-validate-recalls-result accepted (sort (remove-duplicates duplicates) <)
                                        (sort (remove-duplicates warnings) <) (sort (remove-duplicates errors) <))))))

;; validate-final-marks : string list[number] -> list[number] OR false
(define (validate-final-marks str all-couples)
  (let* ([split-str (filter (lambda (s) ((string-length s) . > . 0))
                            (regexp-split #px"[^0-9]+" str))]
         [numbers (map string->number split-str)])
    (and (equal? (sort numbers <) (sort all-couples <))
         numbers)))

;; returns the position of ELT in LST
(define (position elt lst)
  (cond
    [(empty? lst) (error "~a not found" elt)]
    [(eq? elt (first lst)) 0]
    [else (add1 (position elt (rest lst)))]))

(define (format-rank rank)
  (format (if (integer? rank) " ~a " "~a.5") (pad2 (floor rank))))

;; returns the cumulative marks at PLACE or above for FINALIST
(define (cumulative-marks finalist place)
  (list-ref (finalist-cumulative-marks finalist) (sub1 place)))

;; returns the sum of the cumulative marks at PLACE or above for FINALIST
(define (cumulative-sum finalist place)
  (list-ref (finalist-cumulative-sums finalist) (sub1 place)))

;; returns a list of two-element lists, each containing an element from LST
;; and the number of times it appears in LST.
;; ex. (count-occurrences '(a b a d b c b c)) => '((a 2) (b 3) (c 2) (d 1))
(define (count-occurrences lst)
  (hash-map
   (foldl (lambda (elt groups) (hash-set groups elt (add1 (hash-ref groups elt 0))))
          (make-immutable-hash empty)
          lst)
   list))

;; given a list of two-element lists, (competitor mark-count), and
;; a desired number of competitors to recall, returns either one or two
;; lists of (competitor mark-count) lists
(define (recalled mark-counts target)
  (let loop ([prev-set mark-counts] [cutoff 1] [set mark-counts])
    (let ([len (length set)])
      (cond
        [(= len target) (list set)]
        [(< len target) (list prev-set set)]
        [else ; (> len target)
         (let ([new-cutoff (add1 cutoff)])
           (loop set new-cutoff (filter (lambda (x) (>= (second x) new-cutoff)) set)))]))))

;; returns a list of length LEN, where the element at position i is the result
;; of applying OP to all of the marks less than i + 1.
(define (build-list-from-marks op len marks)
  (build-list len (lambda (i) (op (filter (lambda (elt) (<= elt (add1 i))) marks)))))

;; judge-markings is of the form (number place place ...)
;; result is a list of finalist structures
(define ((judge-markings->finalist num-places) judge-markings)
  (let ([num-judges (length judge-markings)]
        [marks (rest judge-markings)])
    (make-finalist (first judge-markings)
                   ;; counts of cumulative marks
                   (build-list-from-marks length num-places marks)
                   ;; sums of cumulative marks
                   (build-list-from-marks (lambda (lst) (apply + lst)) num-places marks))))

;; finds the place-to-assign-th place competitor from finalists for a single dance event,
;; using rules 5 through 8 from the skating system.
(define (rules5-8 finalists place-to-assign)
  (let ([majority (add1 (quotient (first (last-pair (finalist-cumulative-marks (first finalists)))) 2))]
        [max-place (length (finalist-cumulative-marks (first finalists)))])
    (let loop ([place 1] [contenders finalists])
      (let* ([contenders (sort (filter (lambda (f)
                                         (>= (cumulative-marks f place) majority))
                                       contenders)
                               > #:key (lambda (f) (cumulative-marks f place)))])
      (cond
        [(empty? contenders) (loop (add1 place) finalists)] ; rule 8
        [(empty? (rest contenders)) (list (cons place-to-assign contenders))] ; rule 5
        [else ; need to break ties
         ;; rule 6
         (let ([highest-marks (filter (lambda (c)
                                        (= (cumulative-marks c place) (cumulative-marks (first contenders) place)))
                                      contenders)])
           (if (empty? (rest highest-marks))
               (list (cons place-to-assign highest-marks)) ; unique highest majority
               ;; rule 7
               (let* ([sorted-by-sum (sort highest-marks < #:key (lambda (f) (cumulative-sum f place)))]
                      [lowest-sum (filter (lambda (f) (= (cumulative-sum f place) (cumulative-sum (first sorted-by-sum) place)))
                                           sorted-by-sum)])
                 (cond
                   [(empty? (rest lowest-sum)) (list (cons place-to-assign lowest-sum))] ; unique lowest sum
                   [(< place max-place)
                    (loop (add1 place) lowest-sum)] ; still tied: advance to next column
                   [else ; genuine tie
                    (let ([score (+ place-to-assign (/ (sub1 (length lowest-sum)) 2))])
                      (map (lambda (c) (list score c)) lowest-sum))]))))])))))

(define (compute-all-places finalists compute-next)
  (let loop ([placed empty] [unplaced finalists])
    (if (empty? unplaced)
        (reverse placed)
        (let ([newly-placed (compute-next unplaced (add1 (length placed)))])
          (loop (append newly-placed placed)
                (foldl (lambda (p u) (remove (second p) u)) unplaced newly-placed))))))

;; FINALISTS is a list of finalist structures
;; return value is a list of lists (place finalist), sorted by increasing place
(define (place-one-dance finalists)
  (compute-all-places finalists rules5-8))

(define (find-in-single-dance number single-dance-result)
  (first (filter (lambda (place/finalist) (= number (finalist-number (second place/finalist))))
                 single-dance-result)))

(define (place-of number single-dance-result)
  (first (find-in-single-dance number single-dance-result)))

;; SINGLE-DANCE-RESULTS is a list, each element of which is a return value from
;; PLACE-ONE-DANCE.  Combines all of the single-dance results into linked-finalist structures.
(define (single-dance-results->linked-finalists single-dance-results)
  (let ([num-finalists (length (first single-dance-results))])
    (map (lambda (f)
           (let* ([number (finalist-number f)]
                  [places (map (lambda (sdr) (place-of number sdr)) single-dance-results)])
             (make-linked-finalist number
                                   (build-list-from-marks length num-finalists places)
                                   (build-list-from-marks (lambda (lst) (apply + lst)) num-finalists places)
                                   places (apply + places)
                                   (let ([fs-single-dances (map (lambda (sdr)
                                                                  (second (find-in-single-dance number sdr)))
                                                                single-dance-results)])
                                     (make-finalist number
                                                    (apply map + (map finalist-cumulative-marks fs-single-dances))
                                                    (apply map + (map finalist-cumulative-sums fs-single-dances)))))))
         (map second (first single-dance-results)))))

;; Determines the PLACEth linked finalist, starting with rule 9 of the skating system.
(define (rule9 linked-finalists place)
  (let* ([sorted (sort linked-finalists < #:key linked-finalist-sum)]
         [contenders (filter (lambda (lf) (= (linked-finalist-sum lf)
                                             (linked-finalist-sum (first sorted))))
                             sorted)])
    (if (empty? (rest contenders))
        (list (cons place contenders)) ; rule 9
        (rule10 contenders place))))

;; Determines the PLACEth linked finalist, starting with rule 10 of the skating system.
(define (rule10 contenders place)
  (for-each (lambda (lf) (set-linked-finalist-r10! lf 'R10)) contenders)
  (let* ([by-count (sort contenders > #:key (lambda (lf) (cumulative-marks lf place)))]
         [highest-count (filter (lambda (lf) (= (cumulative-marks lf place)
                                                (cumulative-marks (first by-count) place)))
                                by-count)])
    (if (empty? (rest highest-count))
        (list (cons place highest-count)) ; rule 10, part 1
        (let* ([by-sum (sort highest-count < #:key (lambda (lf) (cumulative-sum lf place)))]
               [lowest-sum (filter (lambda (lf) (= (cumulative-sum lf place)
                                                   (cumulative-sum (first by-sum) place)))
                                   by-sum)])
          (if (empty? (rest lowest-sum))
              (list (cons place lowest-sum)) ; rule 10, part 2
              (rule11 lowest-sum place))))))

;; Determines the PLACEth linked finalist, using rule 11 (the final tie-breaker) of the skating system.
(define (rule11 lowest-sum place)
  (for-each (lambda (lf) (set-linked-finalist-r11! lf 'Rll)) lowest-sum)
  (let* ([results (rules5-8 (map linked-finalist-as-single-dance lowest-sum) place)]
         [placed (map (lambda (f)
                        (find-in-single-dance (finalist-number f) (map (lambda (x) (list place x)) lowest-sum)))
                      (map second results))]
         [unplaced (foldl (lambda (p u) (remove (second p) u)) lowest-sum placed)])
    (cond
      [(empty? unplaced) (when (> (length placed) 1)
                           (for-each (lambda (p) (set-linked-finalist-r11! (second p) #f)) placed))
                         placed]
      [else (append (rule10 unplaced (+ place (length placed))) placed)])))

(define (place-linked-dances linked-finalists)
  (compute-all-places linked-finalists rule9))

;; event is an event structure
;; couples is a list of numbers (the couples who danced in the event)
;; mark-sets is a list of mark-set structures, one per dance/judge pair
;; target is the ideal number of competitors to recall
;; generates a report from the mark-sets, showing, for each couple, which judge(s) marked them
;; and whether they were called back.
(define (generate-report event couples mark-sets target)
  (let* ([all-marks (map mark-set-recalled mark-sets)]
         [mark-counts (sort (count-occurrences (apply append all-marks)) > #:key second)]
         [callback-options (recalled mark-counts target)]
         [called-back (sort (map first (first callback-options)) <)]
         [dances (event-dances event)]
         [all-judges (sort (remove-duplicates (map mark-set-judge mark-sets)) <)])
    (case (length callback-options)
      [(1)
       
       ;; print page heading
       (printf "Event ~a, Round ~a: ~a ~a ~a"
               (event-number event) (mark-set-round (first mark-sets))
               (event-level event) (event-style event) (dance->string (first dances)))
       (for-each (lambda (dance) (printf "/~a" (dance->string dance))) (rest dances))
       (printf "~n~n")
       
       ;; print recalls
       (printf "recalled: ~a~n~n" (string-join (map number->string called-back) " "))
       
       ;; print table heading
       (printf "|     |")
       (for-each
        (lambda (dance)
          (printf " ~a: |" (dance->short-string dance))
          (for-each (lambda (judge) (printf "~a|" (pad2 judge))) all-judges))
        dances)
       (printf "~n")
       
       (printf "+-----+")
       (for-each
        (lambda (dance)
          (printf "-----+")
          (for-each (lambda (judge) (printf "--+")) all-judges))
        dances)
       (printf "~n")

       ;; for each couple, print a row with:
       ;; for each dance and each judge, whether the judge marked them in that dance
       ;; then, finally, whether they're recalled or not
       (for-each
        (lambda (couple)
          (printf "| ~a |" couple)
          (for-each
           (lambda (dance)
             (printf "     |")
             (for-each
              (lambda (judge)
                (printf
                 (if (member couple (mark-set-recalled
                                     (find (lambda (mark-set)
                                             (and (eq? (mark-set-dance mark-set) dance)
                                                  (eq? (mark-set-judge mark-set) judge)))
                                           mark-sets)))
                     " X|"
                     "  |")))
              all-judges))
           dances)
          (printf (if (member couple called-back) "R~n" "~n")))
        couples)]
      [(2)
       (printf "cannot match target of ~a; options are ~a or ~a"
               target (length (first callback-options)) (length (second callback-options)))]
      [else (error "something went seriously wrong: 'recalled' returned the following options: ~a" callback-options)])))

;; event is an event structure
;; couples is a list of numbers (the couples who danced in the event)
;; mark-sets is a list of mark-set structures, one per dance/judge pair
;; target is the ideal number of competitors to recall
;; generates a report from the mark-sets, showing, for each couple, which judge(s) marked them
;; and whether they were called back.
(define (generate-sxml-report event round couples mark-sets target)
  (let* ([all-marks (map mark-set-recalled mark-sets)]
         [mark-counts (sort (count-occurrences (apply append all-marks)) > #:key second)]
         [callback-options (recalled mark-counts target)]
         [called-back (sort (map first (first callback-options)) <)]
         [dances (event-dances event)]
         [all-judges (sort (remove-duplicates (map mark-set-judge mark-sets)) <)])
    (case (length callback-options)
      [(1)
       
       (values
        called-back
        `((h3 ,(format "Round ~a" round))
          
          ;; display the table of marks
          (table
           ((border "1"))

           (tr (td) ,@(map
                       (lambda (dance)
                         `(td ((colspan ,(number->string (length all-judges)))) ,(dance->string dance)))
                       dances))

           (tr
            (td)
            ,@(apply append (map (lambda (dance) (map (lambda (judge) `(td ,(pad2 judge))) all-judges))
                                 dances)))
                  
           ;; for each couple, print a row with:
           ;; for each dance and each judge, whether the judge marked them in that dance
           ;; then, finally, whether they're recalled or not
           ,@(map
              (lambda (couple)
                `(tr
                  (td ,(format "~a" couple))
                  ,@(apply
                     append
                     (map
                      (lambda (dance)
                        (map
                         (lambda (judge)
                           (if (member couple (mark-set-recalled
                                               (find (lambda (mark-set)
                                                       (and (eq? (mark-set-dance mark-set) dance)
                                                            (eq? (mark-set-judge mark-set) judge)))
                                                     mark-sets)))
                               '(td "X")
                               '(td)))
                         all-judges))
                      dances))
                  ,(if (member couple called-back) '(td "R") '(td))))
              couples))))]
      [(2)
       (printf "cannot match target of ~a; options are ~a or ~a"
               target (length (first callback-options)) (length (second callback-options)))]
      [else (error "something went seriously wrong: 'recalled' returned the following options: ~a" callback-options)])))

;; event->string : event -> string
(define (event->string e)
  (match e
    [(struct event (number level style dances))
     (format "Event #~a: ~a ~a ~a" number level style (string-join (map dance->string dances) "/"))]))

;; returns a single-dance result
(define (generate-final-report event final-mark-sets)
  (let* ([inverse-judge-marks (map final-mark-set-ranked-couples final-mark-sets)]
         [all-judges (sort (map final-mark-set-judge final-mark-sets) <)]
         [couples (sort (final-mark-set-ranked-couples (first final-mark-sets)) <)]
         [judge-markings (map (lambda (num)
                                (cons num (build-list (length all-judges)
                                                      (lambda (i) (add1 (position num (list-ref inverse-judge-marks i)))))))
                              couples)]
         [dances (event-dances event)]
         [finalists (map (judge-markings->finalist (length couples)) judge-markings)]
         [rankings (place-one-dance finalists)])
    ;; print page heading
    (printf "Event ~a, Round ~a (final): ~a ~a ~a"
            (event-number event) (final-mark-set-round (first final-mark-sets))
            (event-level event) (event-style event) (dance->string (first dances)))
    (for-each (lambda (dance) (printf "/~a" (dance->string dance))) (rest dances))
    (when (> (length dances) 1)
      (printf " (~a)" (dance->string (final-mark-set-dance (first final-mark-sets)))))
    (printf "~n~n")
    
    ;; print table heading
    (printf "| #\\J |")
    (for-each (lambda (judge) (printf "~a|" (pad2 judge))) all-judges)
    (printf " Place | Cumulative~n")
       
    (printf "+-----+")
    (for-each (lambda (judge) (printf "--+")) all-judges)
    (printf "-------+")
    (for-each
     (lambda (score) (printf "-+"))
     (finalist-cumulative-marks (first finalists)))
    (printf "~n")
    
    ;; for each couple, print a row with:
    ;; for each dance and each judge, whether the judge marked them in that dance
    ;; then, finally, whether they're recalled or not
    (for-each
     (lambda (couple)
       (printf "| ~a |" couple)
       (for-each
        (lambda (judge)
          (printf " ~a|" (add1 (position couple (final-mark-set-ranked-couples
                                                 (find (lambda (mark-set)
                                                         (eq? (final-mark-set-judge mark-set) judge))
                                                       final-mark-sets))))))
        all-judges)
       (let* ([place/finalist (find-in-single-dance couple rankings)]
              [rank (first place/finalist)]
              [finalist (second place/finalist)])
         (printf " ~a  |" (format-rank rank))
         (for-each
          (lambda (score) (printf "~a|" (if (zero? score) '- score)))
          (finalist-cumulative-marks finalist)))
       (printf "~n"))
     couples)
    (printf "~n")
    rankings))

(define (generate-sxml-final-report event final-mark-sets)
  (let* ([inverse-judge-marks (map final-mark-set-ranked-couples final-mark-sets)]
         [all-judges (sort (remove-duplicates (map final-mark-set-judge final-mark-sets)) <)]
         [couples (sort (final-mark-set-ranked-couples (first final-mark-sets)) <)]
         [judge-markings (map (lambda (num)
                                (cons num (build-list (length all-judges)
                                                      (lambda (i) (add1 (position num (list-ref inverse-judge-marks i)))))))
                              couples)]
         [dances (event-dances event)]
         [finalists (map (judge-markings->finalist (length couples)) judge-markings)]
         [rankings (place-one-dance finalists)]
         [event-string (event->string event)])
    (list
     rankings
     `(,@(if (> (length dances) 1)
             `((h4 ,(dance->string (final-mark-set-dance (first final-mark-sets)))))
             empty)
    
       (table
        ((border "1"))
        (tr
         (td "#\\J")
         ,@(map (lambda (judge) `(td ,(number->string judge))) all-judges)
         (td "Place")
         (td ((colspan ,(number->string (length couples)))) "Cumulative"))
        
        ;; for each couple, print a row with:
        ;; for each dance and each judge, whether the judge marked them in that dance
        ;; then, finally, whether they're recalled or not
        ,@(map
           (lambda (couple)
             `(tr
               (td ,(number->string couple))
               ,@(map
                  (lambda (judge)
                    `(td ,(number->string (add1 (position couple (final-mark-set-ranked-couples
                                                                  (find (lambda (mark-set)
                                                                          (eq? (final-mark-set-judge mark-set) judge))
                                                                        final-mark-sets)))))))
                  all-judges)
               ,@(let* ([place/finalist (find-in-single-dance couple rankings)]
                        [rank (first place/finalist)]
                        [finalist (second place/finalist)])
                   `((td ,(format-rank rank))
                     ,@(map
                        (lambda (score) `(td ,(if (zero? score) "-" (number->string score))))
                        (finalist-cumulative-marks finalist))))))
           couples))
       (p)
       (table
        ((border "1"))
         
        (tr (td "Place") (td "Num"))
         
        ,@(map
           (lambda (ranking)
             (let ([rank (first ranking)]
                   [couple (finalist-number (second ranking))])
               `(tr (td ,(number->string rank)) (td ,(number->string couple)))))
           rankings))))))

(define (generate-linked-final-report event linked-finalists)
  (let* ([linked-finalists (sort linked-finalists < #:key finalist-number)]
         [rankings (place-linked-dances linked-finalists)]
         [dances (event-dances event)])
    
    ;; print page heading
    (printf "Event ~a, Overall Results: ~a ~a ~a"
            (event-number event) (event-level event) (event-style event) (dance->string (first dances)))
    (for-each (lambda (dance) (printf "/~a" (dance->string dance))) (rest dances))
    (printf "~n~n")
    
    (printf "|     | Place | Sum  |")
    (for-each (lambda (dance) (printf " ~a |" (dance->short-string dance))) dances)
    (printf "~n")
    
    (printf "+-----+-------+------+")
    (for-each (lambda (dance) (printf "----+")) dances)
    (printf "~n")
    
    (for-each
     (lambda (linked-finalist)
       (let* ([number (finalist-number linked-finalist)]
              [ranking (place-of number rankings)]
              [sum (linked-finalist-sum linked-finalist)])
         (printf "| ~a |" number)
         (printf "   ~a   |" ranking)
         (printf " ~a |" (format-rank sum))
         (for-each
          (lambda (place)
            (printf "~a|" (format-rank place)))
          (linked-finalist-places linked-finalist))
         (cond [(linked-finalist-r11 linked-finalist) => (lambda (x) (printf "~a" x))]
               [(linked-finalist-r10 linked-finalist) => (lambda (x) (printf "~a" x))]
               [else (void)])
         (printf "~n")))
       linked-finalists)))

(define (generate-sxml-linked-final-report event linked-finalists)
  (let* ([linked-finalists (sort linked-finalists < #:key finalist-number)]
         [rankings (place-linked-dances linked-finalists)]
         [need-r11-column? (or (ormap linked-finalist-r10 linked-finalists)
                               (ormap linked-finalist-r11 linked-finalists))]
         [dances (event-dances event)])
    
    ;; print page heading
    `((p)
      
      (h4 "Final")
      
      (table
       ((border "1"))
     
       (tr (td) (td "Place") (td "Sum")
           ,@(map (lambda (dance) `(td ,(dance->short-string dance))) dances)
           ,(if need-r11-column? '(td) ""))
    
       ,@(map
          (lambda (linked-finalist)
            (let* ([number (finalist-number linked-finalist)]
                   [ranking (place-of number rankings)]
                   [sum (linked-finalist-sum linked-finalist)])
              `(tr
                (td ,(number->string number))
                (td ,(number->string ranking))
                (td ,(format-rank sum))
                ,@(map
                   (lambda (place)
                     `(td ,(format-rank place)))
                   (linked-finalist-places linked-finalist))
                ,(if need-r11-column?
                     `(td ,(cond [(linked-finalist-r11 linked-finalist) => (lambda (x) (format "~a" x))]
                                 [(linked-finalist-r10 linked-finalist) => (lambda (x) (format "~a" x))]
                                 [else ""]))
                     ""))))
          linked-finalists)))))

(provide (all-defined-out))
