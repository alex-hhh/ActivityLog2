#lang racket/base
;; al-widgets.rkt -- specific widgets to the ActivityLog2 application
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

(require db
         racket/class
         racket/gui/base
         racket/math
         "activity-util.rkt"
         "fmt-util.rkt"
         "icon-resources.rkt"
         "sport-charms.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide sport-selector%)
(provide label-input-field%)
(provide equipment-input-field%)
(provide lap-view%)
(provide mini-lap-view%)
(provide swim-lengths-view%)
(provide get-sql-export-dialog)

(define identity (lambda (x) x))


;;....................................................... sport-selector ....

(define sport-selector%
  (class object%
    (init parent [label "Sport: "])
    (init-field [callback #f] [sports-in-use-only? #t])
    (super-new)

    (define sports 
      (if sports-in-use-only?
          (get-sport-names-in-use)
          (get-sport-names)))

    (define (get-sport-ids selection)
      (let ((sport (list-ref sports selection)))
        (cons (vector-ref sport 1) (vector-ref sport 2))))

    (define (choice-cb choice event)
      (when callback
        (callback (get-sport-ids (send choice get-selection)))))

    (define the-selector
      (new choice%
           [parent parent]
           [label label]
           [callback choice-cb]
           [choices (map (lambda (s)
                           (string-append
                            (if (vector-ref s 2) "     " "") (vector-ref s 0)))
                         sports)]))

    (define/public (get-selection)
      (get-sport-ids (send the-selector get-selection)))

    (define/public (set-selected-sport sport sub-sport)
      (let loop ((sports sports) (index 0))
        (when (pair? sports)
          (if (and (eqv? sport (vector-ref (car sports) 1))
                   (or (eqv? sub-sport (vector-ref (car sports) 2))
                       (and (eqv? sub-sport 0)
                            (eq? (vector-ref (car sports) 2) #f))))
              (send the-selector set-selection index)
              (loop (cdr sports) (+ index 1))))))

    ))


;;................................................... dbtag-input-field% ....

;; Editor for tags stored in the database (labels and equipment)
(define dbtag-input-field%
  (class tag-input-field%
    (init)

    (super-new)

    (inherit set-available-tags set-contents get-contents)

    (define the-database #f)
    (define all-tags '())

    (define/override (clear-contents)
      (set! all-tags '())
      (super clear-contents))

    (define/public (setup-for-session database session-id)
      (set! the-database database)
      (clear-contents)
      (set! all-tags (get-available-tags the-database))
      (set-available-tags (map (lambda (row) (vector-ref row 1)) all-tags))
      (when session-id
        (let ((tags (get-session-tags the-database session-id)))
          (set-contents (map (lambda (row) (vector-ref row 1)) tags)))))

    (define/public (get-contents-as-tag-ids)
      (let ((find-tag-id (lambda (tag)
                           (let loop ((tags all-tags))
                             (if (pair? tags)
                                 (if (string=? tag (vector-ref (car tags) 1))
                                     (vector-ref (car tags) 0)
                                     (loop (cdr tags)))
                                 #f)))))
        (filter identity
                (for/list ((tag (in-list (get-contents))))
                  (find-tag-id tag)))))

    (define/public (update-session-tags session-id)
      (set-session-tags the-database session-id (get-contents-as-tag-ids)))

    (define/public (get-available-tags db) '())
    (define/public (get-session-tags db session-id) '())
    (define/public (set-session-tags db session-id tags) #f)

    ))


;;................................................... label-input-field% ....

(define (get-available-labels db)
  (query-rows db "select L.id, L.name from LABEL L order by L.name"))

(define (get-session-labels db session-id)
  (query-rows db "
select distinct L.id, L.name
  from LABEL L, SESSION_LABEL SL
 where SL.label_id = L.id
   and SL.session_id = ?" session-id))

(define (set-session-labels db session-id label-ids)
  ;; NOTE: it is simpler to just remove all the existing labels for the
  ;; session and re-insert the new ones than determine which ones should be
  ;; deleted and which ones inserted.  We expect the number of labels to be
  ;; reasonably small.
  (call-with-transaction
   db
   (lambda ()
     (query-exec db "delete from SESSION_LABEL where session_id = ?" session-id)
     (for-each (lambda (id)
                 (query-exec db "
insert into SESSION_LABEL (session_id, label_id)
values (?, ?)" session-id id))
               label-ids))))

(define label-input-field%
  (class dbtag-input-field%
    (init)
    (super-new [cue-text "Right-click to add labels"])

    (define/override (get-available-tags db)
      (get-available-labels db))
    (define/override (get-session-tags db session-id)
      (get-session-labels db session-id))
    (define/override (set-session-tags db session-id tags)
      (set-session-labels db session-id tags))

    ))


;;............................................... equipment-input-field% ....

(define (get-available-equipment db retired?)
  (query-rows db "
select E.id,
       ifnull(E.name, E.device_name) as ename
  from EQUIPMENT E
 where E.retired < ?
 order by ename" (if retired? 2 1)))

(define (get-session-equipment db session-id)
  (query-rows db "
select distinct E.id, ifnull(E.name, E.device_name) as name
  from EQUIPMENT E, EQUIPMENT_USE EU
 where EU.equipment_id = E.id
   and EU.session_id = ?" session-id))

(define (set-session-equipment db session-id equipment-ids)
  ;; NOTE: it is simpler to just remove all the existing equipment for the
  ;; session and re-insert the new ones than determine which ones should be
  ;; deleted and which ones inserted.  We expect the number of equipment items
  ;; to be reasonably small.
  (call-with-transaction
   db
   (lambda ()
     (query-exec db "delete from EQUIPMENT_USE where session_id = ?" session-id)
     (for-each (lambda (id)
                 (query-exec db "
insert into EQUIPMENT_USE (session_id, equipment_id)
values (?, ?)" session-id id))
               equipment-ids))))

(define equipment-input-field%
  (class dbtag-input-field%
    (init [use-retired-equipment? #f])
    (super-new [cue-text "Right-click to add equipment"])

    (define retired? use-retired-equipment?)

    (define/override (get-available-tags db)
      (get-available-equipment db retired?))
    (define/override (get-session-tags db session-id)
      (get-session-equipment db session-id))
    (define/override (set-session-tags db session-id tags)
      (set-session-equipment db session-id tags))

    ))

;; Implements laps-view% which displays the laps of an activity


;........................................................... lap fields ....

;; Define the fields that show up in the lap view for each sport.  For each
;; field, we define the column name an extracor (which retrieves the value
;; from the lap) and a formatter which converts the value to a string for
;; display.

(define (mk-column-info name extractor formatter)
  (column-info
   name
   (lambda (lap) 
     (let ((value (extractor lap)))
       (if value (formatter (extractor lap)) "")))
   (lambda (lap)
     (or (extractor lap) 0))))

(define (lap-num l)
  (assq1 'lap-num l))

(define (lap-num-fmt num)
  (format "~a" num))

(define (lap-num-pretty l)
  (format "~a" (assq1 'lap-num-pretty l)))


(define *run-lap-fields*
  (list
   (mk-column-info "Lap" lap-num lap-num-fmt)
   (mk-column-info "Time of day" lap-start-time time-of-day->string)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Distance" lap-distance distance->string)
   (mk-column-info "Pace" lap-avg-speed pace->string)
   (mk-column-info "Best Pace" lap-max-speed pace->string)
   (mk-column-info "Speed" lap-avg-speed speed->string)
   (mk-column-info "Max Speed" lap-max-speed speed->string)
   (mk-column-info "HR" lap-avg-hr n->string)
   (mk-column-info "Max HR" lap-max-hr n->string)
   (mk-column-info "Pa:HR" lap-aerobic-decoupling pct->string)
   (mk-column-info "Cadence" lap-avg-cadence n->string)
   (mk-column-info "Max Cadence" lap-max-cadence n->string)
   (mk-column-info "VOSC" lap-avg-vertical-oscillation vosc->string)
   (mk-column-info "VRATIO" lap-avg-vratio pct->string)
   (mk-column-info "GCT" lap-avg-stance-time n->string)
   (mk-column-info "L-R Bal" lap-left-right-balance pct->string)
   (mk-column-info "Stride" lap-avg-stride stride->string)
   (mk-column-info "Calories" lap-calories n->string)
   (mk-column-info "Ascent" lap-total-ascent n->string)
   (mk-column-info "Descent" lap-total-descent n->string)))

(define *run-mini-lap-fields*
  (list
   (mk-column-info "Lap" lap-num lap-num-fmt)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Distance" lap-distance distance->string)
   (mk-column-info "Pace" lap-avg-speed pace->string)))

(define *bike-lap-fields*
  (list
   (mk-column-info "Lap" lap-num lap-num-fmt)
   (mk-column-info "Time of day" lap-start-time time-of-day->string)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Distance" lap-distance distance->string)
   (mk-column-info "Speed" lap-avg-speed speed->string)
   (mk-column-info "Max Speed" lap-max-speed speed->string)
   (mk-column-info "HR" lap-avg-hr n->string)
   (mk-column-info "Max HR" lap-max-hr n->string)
   (mk-column-info "Pw:HR" lap-aerobic-decoupling pct->string)
   (mk-column-info "Cadence" lap-avg-cadence n->string)
   (mk-column-info "Max Cadence" lap-max-cadence n->string)

   (mk-column-info "Power" lap-avg-power n->string)
   (mk-column-info "Max Power" lap-max-power n->string)
   (mk-column-info "Adjusted Power" lap-normalized-power n->string)

   (mk-column-info "L-R Bal" lap-left-right-balance pct->string)
   (mk-column-info "Left TEff" lap-avg-left-torque-effectiveness n->string)
   (mk-column-info "Right TEff" lap-avg-right-torque-effectiveness n->string)
   (mk-column-info "Left PSmth" lap-avg-left-pedal-smoothness n->string)
   (mk-column-info "Right PSmth" lap-avg-right-pedal-smoothness n->string)

   (mk-column-info "Left PCO" lap-avg-left-pco
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Right PCO" lap-avg-right-pco
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Left PP Start" lap-avg-left-pp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Left PP End" lap-avg-left-pp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Right PP Start" lap-avg-right-pp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Right PP End" lap-avg-right-pp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Left Peak PP Start" lap-avg-left-ppp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Left Peak PP End" lap-avg-left-ppp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Right Peak PP Start" lap-avg-right-ppp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-column-info "Right Peak PP End" lap-avg-right-ppp-end
                   (lambda (val) (number->string (exact-round val))))

   (mk-column-info "Calories" lap-calories n->string)
   (mk-column-info "Ascent" lap-total-ascent n->string)
   (mk-column-info "Descent" lap-total-descent n->string)))

(define *bike-mini-lap-fields*
  (list
   (mk-column-info "Lap" lap-num lap-num-fmt)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Distance" lap-distance distance->string)
   (mk-column-info "Speed" lap-avg-speed speed->string)))

(define *swim-lap-fields*
  (list
   ;; (mk-column-info "Lap #" lap-num lap-num-fmt)
   (column-info "Lap" lap-num-pretty lap-num)
   (mk-column-info "Swim Stroke" lap-swim-stroke get-swim-stroke-name)
   ;; (mk-column-info "Time of day" lap-start-time format-time-of-day)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Lengths" lap-num-lengths n->string)
   (mk-column-info "Distance" lap-distance n->string)
   (mk-column-info "Pace" lap-avg-speed swim-pace->string)
   (mk-column-info "Best Pace" lap-max-speed swim-pace->string)
   (mk-column-info "SWOLF" lap-avg-swolf n->string)
   (mk-column-info "Best SWOLF" lap-best-swolf n->string)
   (mk-column-info "Total Strokes" lap-total-cycles n->string)
   (mk-column-info "Avg Strokes" lap-avg-strokes n->string)
   (mk-column-info "Cadence" lap-avg-cadence n->string)
   (mk-column-info "Max Cadence" lap-max-cadence n->string)
   (mk-column-info "Calories" lap-calories n->string)))

(define *swim-mini-lap-fields*
  (list
   ;; (mk-column-info "Lap #" lap-num lap-num-fmt)
   (column-info "Lap" lap-num-pretty lap-num)
   ;; (mk-column-info "Time of day" lap-start-time format-time-of-day)
   (mk-column-info "Duration" lap-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Distance" lap-distance n->string)
   (mk-column-info "Pace" lap-avg-speed swim-pace->string)))

(define (length-num l)
  (assq1 'length-num l))

(define (length-num-fmt num)
  (format "~a" num))

;; For swimming activitites, we display the lengths of the selected lap
(define *swim-length-fields*
  (list
   (mk-column-info "Length" length-num length-num-fmt)
   (mk-column-info "Swim Stroke" length-swim-stroke get-swim-stroke-name)
   (mk-column-info "Duration" length-time (lambda (v) (duration->string v #t)))
   (mk-column-info "Strokes" length-total-cycles n->string)
   (mk-column-info "SWOLF" length-swolf n->string)
   (mk-column-info "Pace" length-avg-speed swim-pace->string)))

(define *sport-lap-definitions*
  (list
   (cons 1 *run-lap-fields*)
   (cons 2 *bike-lap-fields*)
   (cons 5 *swim-lap-fields*)))

(define *default-lap-fields* *bike-lap-fields*)

(define *sport-mini-lap-definitions*
  (list
   (cons 1 *run-mini-lap-fields*)
   (cons 2 *bike-mini-lap-fields*)
   (cons 5 *swim-mini-lap-fields*)))

(define *default-mini-lap-fields* *bike-mini-lap-fields*)

(define (get-lap-field-definitions sport)
  (cond ((assoc sport *sport-lap-definitions*) => cdr)
	(#t *default-lap-fields*)))

(define (get-mini-lap-field-definitions sport)
  (cond ((assoc sport *sport-mini-lap-definitions*) => cdr)
	(#t *default-mini-lap-fields*)))


;............................................................ lap-view% ....

;; Display the laps of a session in a list box.  Provides a call back for
;; notifications when a lap is selected.
(define lap-view%
  (class object%
    (init parent tag callback)
    (super-new)

    ;; The session for which we display the lap info
    (define the-session #f)

    (define lb
      (new (class qresults-list% (init) (super-new)
             (define/override (on-select row-index row-data)
               (when callback
                 (callback row-index row-data))))
           [tag tag]
           [parent parent]))

    (define/public (lap-field-definitions sport)
      (get-lap-field-definitions sport))

    ;; Add lap numbers to the laps, if a lap has 0 distance, label it as
    ;; "Rest"
    (define (number-session-laps session)
      (let ((lap-num 0)
            (lap-num-pretty 0))
        (for/list ((lap (session-laps the-session)))
          (cons 
           (cons 'lap-num
                 (begin (set! lap-num (+ lap-num 1)) lap-num))
           (cons
            (cons 'lap-num-pretty 
                  (if (or (not (lap-distance lap)) (> (lap-distance lap) 0))
                      (begin
                        (set! lap-num-pretty (+ lap-num-pretty 1))
                        lap-num-pretty)
                      "Rest"))
            lap)))))

    (define/public (set-session session [df #f])
      (set! the-session session)
      (let ((sport (session-sport the-session)))
        (send lb setup-column-defs (lap-field-definitions sport))
        (send lb set-data (number-session-laps the-session))))

    (define/public (save-visual-layout)
      (send lb save-visual-layout))

    ))


;;....................................................... mini-lap-view% ....

(define mini-lap-view%
  (class lap-view%
    (init parent callback)
    (super-new [parent parent] [callback callback])

    (define/override (lap-field-definitions sport)
      (get-mini-lap-field-definitions sport))

    ))


;;................................................... swim-lengths-view% ....

;; Display information about swim lengths in a list box.
(define swim-lengths-view%
  (class object%
    (init parent tag)
    (super-new)

    (define parent-panel parent)
    (define deleted? #t)

    (define p (new horizontal-panel% [parent parent-panel] [style '(deleted)]))
    (define lb (new qresults-list% [tag tag] [label "Lengths for lap"] [parent p]))

    (send lb setup-column-defs *swim-length-fields*)

    (define/public (show! flag)
      ;; (display (format "Show ~a (vs ~a)~%" flag deleted?))  Ignore a
      ;; show/hide request that won't do anything
      (unless (eq? deleted? (not flag))
        (set! deleted? (not flag))
        (if deleted?
            (send parent-panel delete-child p)
            (send parent-panel add-child p))))

    ;; Return the lenghts of LAP with a lenght-num tag added to each length
    (define (number-swim-lengths lap)
      (let ((lengths (lap-lengths lap)))
        (for/list ((l (in-list lengths))
                   (n (in-range (length lengths))))
          (cons (cons 'length-num (+ n 1)) l))))

    (define/public (set-lap lap)
      (send lb set-data (number-swim-lengths lap)))
    
    (define/public (save-visual-layout)
      (send lb save-visual-layout))

    ))


;;............................................. sql-query-export-dialog% ....

(define sql-query-export-dialog%
  (class object%
    (init) (super-new)

    (define dialog-width 400)
    (define dialog-height 300)
    (define dialog-title "Export SQL Query")

    (define contents-text-field #f)

    (define (make-toplevel-dialog parent)
      (new
       (class dialog% (init) (super-new)
         (define/augment (on-close) (on-close-dialog)))
       [label dialog-title]
       [min-width dialog-width]
       [min-height dialog-height]
       [parent parent]))

    (define toplevel-window (make-toplevel-dialog #f))

    (define (on-save-to-file)
      (let ((text (send contents-text-field get-value))
            (file (put-file "Select file to export to" #f #f #f "txt" '()
                            '(("Text Files" "*.txt") ("Any" "*.*")))))
        ;; On Windows machines, add back the \r into the text, otherwise the
        ;; text will not save nicely
        (when (eq? 'windows (system-type 'os))
          (set! text (regexp-replace* "\n" text "\r\n")))
        (when file
          (call-with-output-file file 
            (lambda (o) (write-string text o))
            #:mode 'text #:exists 'replace))))

    (define (on-copy-to-clipboard timestamp)
      (let ((text (send contents-text-field get-value)))
        ;; On Windows machines, add back the \r into the text, otherwise the
        ;; text will not paste nicely
        (when (eq? 'windows (system-type 'os))
          (set! text (regexp-replace* "\n" text "\r\n")))
        (send the-clipboard set-clipboard-string text timestamp)))

    (define (on-close-dialog)
      (send toplevel-window show #f))

    (define dialog-panel
      (let ((p (new vertical-panel% [parent toplevel-window]
                    [spacing 10] [border 10]
                    [alignment '(left top)])))
        (let ((p0 (make-horizontal-pane p #f)))
          (new message% [parent p0]
               [label sql-export-icon]
               [stretchable-width #f]
               [stretchable-height #f]))

        (let ((client-pane (new vertical-panel% [parent p]
                                [border 0] [spacing 10]
                                [alignment '(left top)])))
          (set! contents-text-field 
                (new text-field% [parent client-pane] [label ""] [style '(multiple)])))

        (let ((bp (new horizontal-pane% [parent p] [border 0]
                       [stretchable-height #f] [alignment '(right center)])))
          (new button% [label "Save..."] [parent bp]
               [callback (lambda (b e) (on-save-to-file))])
          (new button% [label "Copy to clipboard"] [parent bp]
               [callback (lambda (b e) (on-copy-to-clipboard (send e get-time-stamp)))])
          (new button% [label "Close"] [parent bp]
               [callback (lambda (b e) (on-close-dialog))]))

        p))

    (define (do-dialog-operation parent)
      (let ((old-toplevel toplevel-window))
        (let ((toplevel (if parent (make-toplevel-dialog parent) toplevel-window)))
          (send dialog-panel reparent toplevel)
          (set! toplevel-window toplevel))
        (send toplevel-window show #t) ; will block until finish-dialog is called
        (send dialog-panel reparent old-toplevel)
        (set! toplevel-window old-toplevel)
        #t))

    (define/public (show-dialog parent text)
      (send contents-text-field set-value (regexp-replace* "\r" text ""))
      (do-dialog-operation parent))))

(define the-sql-export-dialog #f)

(define (get-sql-export-dialog)
  (unless the-sql-export-dialog
    (set! the-sql-export-dialog (new sql-query-export-dialog%)))
  the-sql-export-dialog)


