(require (file "scrutineering.ss") scheme/base)

;;; For testing:

;; interesting cases: 1021971331 1025054504 625104625 123448584 122366323 1000684960 998890808
(define cms (abs (current-milliseconds)))
(random-seed cms)

;; selects COUNT elements of SET at random
(define (select-at-random count set)
  (if (or (empty? set) (zero? count))
      empty
      (let ([elt (list-ref set (random (length set)))])
        (cons elt (select-at-random (sub1 count) (remove elt set))))))

;; generates COUNT unique random integers between MIN (inclusive) and MAX (exclusive)
(define (random-integers min max count)
  (select-at-random count (build-list (- max min) (lambda (i) (+ i min)))))

;; returns a list of JUDGE-COUNT-element lists, each containing a
;; MARK-COUNT-element subset of COMPETITOR-NUMS
(define (random-marks judge-count mark-count competitor-nums)
  (build-list judge-count (lambda (i) (select-at-random mark-count competitor-nums))))

(define (marks->mark-sets judge-numbers marks event round dance)
  (map (lambda (judge-number marks)
         (make-mark-set event round dance judge-number marks))
       judge-numbers
       marks))

;; single-dance quarterfinal

(define bronze-international-waltz
  (make-event 1 'Bronze 'International '(w)))

'random-quarterfinal:
(define competitors (sort (random-integers 101 400 24) <))
'competitors: competitors
(define marks (random-marks 5 12 competitors))
'sorted-marks-by-judge: (map (lambda (l) (sort l <)) marks)
(define mark-counts (sort (count-occurrences (apply append marks)) > #:key second))
'recall-possibilities:
(apply values (map (lambda (set) (let ([competitors (map first (sort set < #:key first))])
                                   (list (length competitors) ': competitors))) (recalled mark-counts 12)))

(define mark-sets (marks->mark-sets '(1 2 3 4 5) marks bronze-international-waltz 1 'w))

(define possibilities (recalled mark-counts 12))
(if (= 1 (length possibilities))
    (generate-report bronze-international-waltz competitors mark-sets 12)
    (let* ([option1 (length (first possibilities))]
           [option2 (length (second possibilities))]
           [delta1 (abs (- option1 12))]
           [delta2 (abs (- option2 12))]
           [selected (if (<= delta1 delta2) option1 option2)])
      (generate-report bronze-international-waltz competitors mark-sets selected)))

;; linked quarter-final

(define silver-international-waltz-quickstep
  (make-event 2 'Silver 'International '(w q)))

'random-quarterfinal:
(define competitors (sort (random-integers 101 400 24) <))
'competitors: competitors
(define waltz-marks (random-marks 5 12 competitors))
(define quickstep-marks (random-marks 5 12 competitors))
(define marks (append waltz-marks quickstep-marks))

'sorted-marks-by-judge: (map (lambda (l) (sort l <)) marks)
(define mark-counts (sort (count-occurrences (apply append marks)) > #:key second))
'recall-possibilities:
(apply values (map (lambda (set) (let ([competitors (map first (sort set < #:key first))])
                                   (list (length competitors) ': competitors))) (recalled mark-counts 12)))

(define waltz-mark-sets (marks->mark-sets '(1 2 3 4 5) waltz-marks silver-international-waltz-quickstep 1 'w))
(define quickstep-mark-sets (marks->mark-sets '(1 2 3 4 5) quickstep-marks silver-international-waltz-quickstep 1 'q))
(define mark-sets (append waltz-mark-sets quickstep-mark-sets))

(define possibilities (recalled mark-counts 12))
(if (= 1 (length possibilities))
    (generate-report silver-international-waltz-quickstep competitors mark-sets 12)
    (let* ([option1 (length (first possibilities))]
           [option2 (length (second possibilities))]
           [delta1 (abs (- option1 12))]
           [delta2 (abs (- option2 12))]
           [selected (if (<= delta1 delta2) option1 option2)])
      (generate-report silver-international-waltz-quickstep competitors mark-sets selected)))

'random-final:
(define final-numbers (sort (select-at-random 6 competitors) <))
'competitors: final-numbers
(define inverse-judge-marks (build-list 5 (lambda (_) (select-at-random 6 final-numbers))))
'marks-by-judge:
(define judge-markings ;'((102 6 6 2 1 2) (129 1 2 6 2 4) (183 3 4 1 3 1) (190 5 1 5 5 6) (289 2 3 4 4 5) (369 4 5 3 6 3))
  (map (lambda (num)
         (cons num (build-list 5 (lambda (i) (add1 (position num (list-ref inverse-judge-marks i)))))))
       final-numbers))
judge-markings
(define finalists (map (judge-markings->finalist 6) judge-markings))
(define waltz (place-one-dance finalists))
'waltz:
waltz
'random-final:
(define inverse-judge-marks (build-list 5 (lambda (_) (select-at-random 6 final-numbers))))
'marks-by-judge:
(define judge-markings ;'((102 6 6 2 1 2) (129 1 2 6 2 4) (183 3 4 1 3 1) (190 5 1 5 5 6) (289 2 3 4 4 5) (369 4 5 3 6 3))
  (map (lambda (num)
         (cons num (build-list 5 (lambda (i) (add1 (position num (list-ref inverse-judge-marks i)))))))
       final-numbers))
judge-markings
(define finalists (map (judge-markings->finalist 6) judge-markings))
(define quickstep (place-one-dance finalists))
'quicktep:
quickstep
'random-final:
(define inverse-judge-marks (build-list 5 (lambda (_) (select-at-random 6 final-numbers))))
'marks-by-judge:
(define judge-markings ;'((102 6 6 2 1 2) (129 1 2 6 2 4) (183 3 4 1 3 1) (190 5 1 5 5 6) (289 2 3 4 4 5) (369 4 5 3 6 3))
  (map (lambda (num)
         (cons num (build-list 5 (lambda (i) (add1 (position num (list-ref inverse-judge-marks i)))))))
       final-numbers))
judge-markings
(define finalists (map (judge-markings->finalist 6) judge-markings))
(define tango (place-one-dance finalists))
'tango:
tango
(define linked-finalists (single-dance-results->linked-finalists (list waltz tango quickstep)))
linked-finalists
(place-linked-dances linked-finalists)

(define final-mark-sets (map (lambda (marks judge) (make-final-mark-set bronze-international-waltz 3 'w judge marks))
                             inverse-judge-marks '(1 2 3 4 5)))
(generate-final-report silver-international-waltz-quickstep final-mark-sets)

(define silver-international-wqt (make-event 3 'Silver 'International '(w q t)))

(printf "~n")
(generate-linked-final-report silver-international-wqt linked-finalists)

(define silver-int-cr (make-event 4 'Silver 'International '(c r)))

(define inverse-judge-marks:cha
  '((415 584 275 512 317 226)
    (584 226 275 317 512 415)
    (584 226 275 317 415 512)
    (584 275 317 415 512 226)
    (415 584 275 226 512 317)
    (275 415 226 512 317 584)
    (584 226 512 415 275 317)))

(define inverse-judge-marks:rumba
  '((226 512 584 415 317 275)
    (275 584 317 226 512 415)
    (584 226 275 512 415 317)
    (584 275 512 317 415 226)
    (584 275 226 415 512 317)
    (275 584 512 415 226 317)
    (584 226 512 415 275 317)))

(define final-mark-sets:cha
  (map (lambda (marks judge) (make-final-mark-set silver-int-cr 3 'c judge marks))
       inverse-judge-marks:cha '(5 6 9 13 14 18 21)))

(define final-mark-sets:rumba
  (map (lambda (marks judge) (make-final-mark-set silver-int-cr 3 'r judge marks))
       inverse-judge-marks:rumba '(5 6 9 13 14 18 21)))

(define cha-ranks (generate-final-report silver-int-cr final-mark-sets:cha))
(define rumba-ranks (generate-final-report silver-int-cr final-mark-sets:rumba))
(generate-linked-final-report silver-int-cr (single-dance-results->linked-finalists (list cha-ranks rumba-ranks)))

(define competitors '(101 112 123 134 145 156 =))
(define validated
  (validate-recalls " 101,111 112  101 134 101 76xyz123" competitors))
'accepted: (validate-recalls-result-accepted validated)
'duplicates: (validate-recalls-result-duplicates validated)
'warnings: (validate-recalls-result-warnings validated)
'errors: (validate-recalls-result-errors validated)
