#lang racket/base
;; edit-sz.rkt -- Edit the Sport Zones stored in the database

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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


(require db/base
         racket/class
         racket/gui/base
         racket/list
         racket/string
         racket/match
         racket/math
         racket/format
         "../fmt-util.rkt"
         "../dbutil.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "../time-in-zone.rkt"
         "../utilities.rkt")

(provide get-sz-editor)

;;................................................................. zdef ....

;; Contains definition for each zone field, depending on the zone metric: the
;; cue text, the function to convert the input value into a number and the
;; function to convert a number into the value displayed.
(struct zdef (cue-text str->val val->str))

(define (n->s n) (~r n #:precision 0))

;; Zone field definitions for different zones
(define heart-rate-zdef (zdef "bpm" string->number n->s))
(define power-zdef (zdef "power" string->number n->s))
(define pace-zdef (zdef "mm:ss / km" run-pace-string->mps pace->string))
;; NOTE: run-pace-string->mps, pace->string already consider the measurement
;; system preferences, so all we have to do is change the labels
(define pace-zdef-imperial (zdef "mm:ss / mile" run-pace-string->mps pace->string))

;; Return a zone definition based on SPORT and a zone metric, ZMETRIC.  Return
;; #f if no valid zone definition can be found.
;;
;; NOTE: we only consider the sport, not the sub-sport when looking at zone
;; definitions.  This means, for example, that all types of running (trail,
;; track, etc) share the same heart rate zones.  The database schema would
;; support sub-sport zone types, but we don't support editing them.
(define (get-zdef sport zmetric)
  (cond ((= zmetric 1) heart-rate-zdef)
        ((= sport 1)                    ; running
         (cond ((= zmetric 2)
                (if (eq? (al-pref-measurement-system) 'metric)
                    pace-zdef pace-zdef-imperial))
               (#t #f)))
        ((= sport 2)                    ; cycling
         (cond ((= zmetric 3)
                power-zdef)
               (#t #f)))))


;;............................................. edit-sport-zones-dialog% ....

;; Dialog for editing one set of sport zones (for a specific sport and zone
;; type).
(define edit-one-sz-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Edit Sport Zones"] [icon (edit-icon)])

    (define zinputs '())                ; list of zone input text widgets
    (define zdefinition #f)          ; a zdef strucure for the current zmetric

    ;; list of valid zone definitions for the current sport/sub-sport
    ;; selection
    (define zone-data #f)

    (define pane
      (let ((p (send this get-client-pane)))
        (new grid-pane% [parent p] [columns 2]
             [border 10]  [spacing 5]
             [alignment '(left center)])))

    (new message% [parent pane] [label "Sport"])
    (define sport-message (new message% [parent pane] [label "XXX"] [stretchable-width #t]))
    (new message% [parent pane] [label "Zone Metric"])
    (define zmetric-message (new message% [parent pane] [label "XXX"] [stretchable-width #t]))
    (new message% [parent pane] [label "Valid From"])
    (define valid-from-field
      (new date-input-field% [parent pane] [label ""] [stretchable-width #t]
           [allow-empty? #f]))
    (new message% [parent pane] [label "Number of Zones"])
    (define zcount-input
      (new number-input-field% [parent pane]
           [min-value 0] [max-value 10]
           [stretchable-width #t]
           [valid-value-cb (lambda (v) (unless (eq? v 'empty) (on-zone-count-changed v)))]))

    ;; Panel to hold all the zone input fields.
    (define zinput-panel
      (let* ((p (send this get-client-pane))
             (p1 (new horizontal-pane% [parent p] [alignment '(center center)])))
        (new vertical-panel% [parent p1] [spacing 10] [border 10]
             [stretchable-width #f] [alignment '(right center)])))

    ;; Message to display when no zones are defined
    (define no-zones-message
      (new message%
           [label "No zones are defined"] [parent zinput-panel] [style '(deleted)]))

    ;; Make a zone input field with LABEL.
    (define (make-zinput label)
      (new validating-input-field% [parent zinput-panel]
           [label label] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [cue-text (zdef-cue-text zdefinition)]
           [validate-fn (lambda (v)
                          (or (= (string-length (string-trim v)) 0)
                              ((zdef-str->val zdefinition) v)))]
           [convert-fn (lambda (v)
                         (if (= (string-length (string-trim v)) 0)
                             'empty
                             ((zdef-str->val zdefinition) v)))]))

    ;; Update the number of zone input fields to NEW-COUNT.  This is done by
    ;; either removing or adding new fields, as needed.  Note that NEW-COUNT
    ;; does not include the MIN and MAX input fields, as such (length zinputs)
    ;; will always be (+ NEW-COUNT 2) after this function runs (unless
    ;; NEW-COUNT is 0, in which case the MIN/MAX fields are removed as well).
    (define (on-zone-count-changed new-count)
      (let ((old-count (max 0 (- (length zinputs) 2))))
        (cond ((= old-count new-count) #t) ; nothing to do
              ((> old-count new-count)
               ;; Remove some inputs.  If NEW-COUNT is 0, remove min/max
               ;; values as well, and display the "No zones defined message.
               (if (= new-count 0)
                   (set! zinputs '())
                   (set! zinputs
                         (append
                          (drop-right zinputs (+ 1 (- old-count new-count)))
                          (take-right zinputs 1))))
               (send zinput-panel change-children
                     (lambda (old)
                       (if (> new-count 0) zinputs (list no-zones-message)))))
              ((< old-count new-count)
               ;; Add some inputs. If OLD-COUNT is 0, add the min/max fields
               ;; as well.
               (when (= old-count 0)
                 (set! zinputs (list (make-zinput "Min") (make-zinput "Max"))))
               (set! zinputs
                     (append
                      (drop-right zinputs 1)
                      (for/list ((i (in-range old-count new-count)))
                        (make-zinput (format "Zone ~a" (+ 1 i))))
                      (take-right zinputs 1)))
               (send zinput-panel change-children (lambda (old) zinputs))))))

    ;; Setup the dialog box to display the ZONES: the correct number of zone
    ;; input fields are created and filled in and ZCOUNT-INPUT is also
    ;; updated.
    (define (setup-zones zones)
      (let ((nzones (max 0 (- (length zones) 2))))
        (send zcount-input set-value (format "~a" nzones))
        (on-zone-count-changed nzones)
        (for ((zval (in-list zones))
              (zinput (in-list zinputs)))
          (send zinput set-cue-text (zdef-cue-text zdefinition))
          (send zinput set-value ((zdef-val->str zdefinition) zval)))))

    ;; Return the zone definition from the values in the zone input fields.
    (define (collect-zones)
      (for/list ((input (in-list zinputs)))
        (send input get-converted-value)))

    ;; Return true if ZONES are valid: they must contain only numbers and be
    ;; in ascending order.
    (define (valid-zones? zones)
      (if (> (length zones) 0)
          (andmap (lambda (a b) (and (number? a) (number? b) (< a b)))
                  (drop-right zones 1) (cdr zones))
          #t))

    ;; Return a list of invalid fields.  These are fields that either contain
    ;; an invalid value, or would define non-ascening zones w.r.t their
    ;; neighbouring fields.
    (define (collect-invalid-fields)
      (define invalid-zinputs '())
      (when (> (length zinputs) 0)
        (for ((a (drop-right zinputs 1))
              (b (cdr zinputs)))
          (let ((v1 (send a get-converted-value))
                (v2 (send b get-converted-value)))
            ;; NOTE: don't mark 'empty filds as invalid
            (when (eq? v1 #f) (set! invalid-zinputs (cons a invalid-zinputs)))
            (when (eq? v2 #f) (set! invalid-zinputs (cons b invalid-zinputs)))
            (when (and (number? v1) (number? v2))
              (when (> v1 v2)
                (set! invalid-zinputs (cons a invalid-zinputs)))))))
      invalid-zinputs)

    (define/override (has-valid-data?)
      (define invalid-zinputs (collect-invalid-fields))

      (for ((f (in-list zinputs)))
        (send f mark-valid (not (member f invalid-zinputs))))

      ;; NOTE: the presence of INVALID-ZINPUTS indicates a problem, but the
      ;; absence does not mean all is fine, as we don't mark fields with empty
      ;; values as invalid, yet we don't want to make the entire dialog valid
      ;; if such fields exist.
      (and (= (length invalid-zinputs) 0)
           (valid-zones? (collect-zones))
           (send valid-from-field has-valid-value?)))

    (define/public (show-dialog parent sport zmetric valid-from zones)

      (send sport-message set-label (get-sport-name sport #f))
      (send zmetric-message set-label
            (case zmetric
              ((1) "Heart Rate")
              ((2) "Pace")
              ((3) "Power")
              (else "Unknown")))

      (if valid-from
          (send valid-from-field set-date-value valid-from)
          (send valid-from-field set-value ""))

      (set! zdefinition (get-zdef sport zmetric))
      (setup-zones (or zones '()))

      (if (send this do-edit parent)
          (cons (send valid-from-field get-converted-value) (collect-zones))
          #f))

    ))


(define sz-query "
select VSZ.zone_id, VSZ.valid_from, VSZ.valid_until,
       (select count(*) from V_SPORT_ZONE_FOR_SESSION VSZFS where VSZFS.zone_id = VSZ.zone_id) as session_count
  from V_SPORT_ZONE VSZ
 where sport_id = ?
   and zone_metric_id = ?
 order by valid_from")

(define (sz-id data) (vector-ref data 0))
(define (sz-valid-from data) (vector-ref data 1))
(define (sz-valid-until data) (vector-ref data 2))
(define (sz-session-count data) (vector-ref data 3))
(define (sz-zones data) (vector-ref data 4))

(define (get-sport-zone-values db zone-id)
  (query-list
   db
   "select zone_value from SPORT_ZONE_ITEM where sport_zone_id = ? order by zone_number"
   zone-id))

(define (get-sport-zone-metric db zone-id)
  (query-value
   db
   "select zone_metric_id from SPORT_ZONE where id = ?"
   zone-id))

(define (get-sport-zones-as-string db zone-id)
  (let* ((zmetric (get-sport-zone-metric db zone-id))
         (formatter (case zmetric
                      ((1) (lambda (x) (number->string (exact-round x))))
                      ((2) pace->string) ; NOTE: we don't bother with the speed, only pace
                      ((3) power->string)))
         (label (case zmetric
                  ((1) "bpm ")
                  ((2) (if (eq? (al-pref-measurement-system) 'metric)
                           "min/km "
                           "min/mile "))
                  ((3) "watts ")))
         (zones (get-sport-zone-values db zone-id)))
    (string-join (filter (lambda (x) (> (string-length x) 0))
                         (map formatter zones))
                 ", "
                 #:before-first label)))

(define (get-sport-zone-data db sport metric)
  (let ((rows (query-rows db sz-query sport metric)))
    (for/list ((row (in-list rows)))
      (match-define (vector id vfrom vuntil count) row)
      (vector id vfrom vuntil count (get-sport-zones-as-string db id)))))

(define (make-sz-columns)
  (list
   (qcolumn "Valid From"
            (lambda (row) (date-time->string (sz-valid-from row)))
            sz-valid-from)
   (qcolumn "Valid Until"
            (lambda (row) (date-time->string (sz-valid-until row)))
            sz-valid-from)
   (qcolumn "Session Count"
            (lambda (row)
              (let ((sc (sz-session-count row)))
                (if (sql-null? sc) "" (number->string sc))))
            sz-session-count)
   (qcolumn "Zones" sz-zones sz-zones)))

(define (delete-sport-zone db zone-id)
  (query-exec db "delete from TIME_IN_ZONE where sport_zone_id = ?" zone-id)
  (query-exec db "delete from SPORT_ZONE_ITEM where sport_zone_id = ?" zone-id)
  (query-exec db "delete from SPORT_ZONE where id = ?" zone-id))

(define (put-sport-zone db sport zmetric valid-from zones)
  (when (> (length zones) 3)
    (query-exec db
                "insert into SPORT_ZONE(sport_id, sub_sport_id, zone_metric_id, valid_from)
                    values (?, ?, ?, ?)"
                sport sql-null zmetric valid-from)
    (let ((zid (db-get-last-pk "SPORT_ZONE" db)))
      (for ((zone (in-list zones))
            (znum (in-range (length zones))))
        (query-exec db
                    "insert into SPORT_ZONE_ITEM(sport_zone_id, zone_number, zone_value)
                      values(?, ?, ?)" zid znum zone)))))

;; Return a list of session ID's that had their sport zones changed when we
;; changed validity times for the sport zones.  The TIME_IN_ZONE data for
;; these sessions will have to be updated.
(define (get-modified-sessions db)
  (query-list
   db
   ;; distinct is needed because we might get duplicates when both the HR and
   ;; power/pace zones change for an activity.  Also, we only look for HR and
   ;; POWER zones (1 and 3), as these are the only ones for which we calculate
   ;; TIME_IN_ZONE data
   "select distinct VSZFS.session_id
  from V_SPORT_ZONE_FOR_SESSION VSZFS
 where VSZFS.zone_metric_id in (1, 3)
   and not exists(select * from TIME_IN_ZONE TIZ
                   where TIZ.session_id = VSZFS.session_id
                     and TIZ.sport_zone_id = VSZFS.zone_id)"))

;; Update time in zone data for SESSIONS.  Displays a progress window to keep
;; the user informed.
(define (interactive-update-time-in-zone sessions database parent-window)

  (define frame #f)
  (define message-field #f)
  (define progress-bar #f)
  (define last-msg #f)
  (define title "Updating Sessions")

  (define (cb msg crt max)
    ;; Setting the same message causes it to flicker.  Avoid doing that.
    (when (and msg (not (equal? last-msg msg)))
      (set! last-msg msg)
      (send message-field set-label msg))
    (when (and crt max)
      (let ((new-progress (exact-round (* 100 (/ crt max)))))
        (send progress-bar set-value new-progress))))

  (define (task-thread)
    (with-handlers
      (((lambda (e) #t)
        (lambda (e) (dbglog-exception "interactive-update-time-in-zone" e))))
      (dbglog "interactive-update-time-in-zone started")
      (define num-sessions (length sessions))
      (for ([(sid n) (in-indexed sessions)])
        (with-handlers
          (((lambda (e) #t)
            (lambda (e)                   ; log the exception, than propagate it
              (dbglog "while updating session ~a: ~a" sid e)
              (raise e))))
          (update-time-in-zone-data sid database)
          (cb (format "Updating session ~a" sid) n num-sessions)))
      (dbglog "interactive-update-time-in-zone completed")
      (send frame show #f)))

  (set! frame (new
               (class dialog%
                 (super-new)
                 (define/override (on-superwindow-show show?)
                   (thread/dbglog
                    #:name "interactive-update-time-in-zone"
                    task-thread)))
               [width 400] [height 250]
               [parent parent-window]
               [stretchable-width #f] [stretchable-height #f]
               [label title]))
  (let ((pane (make-horizontal-pane frame)))
    (send pane border 20)
    (new message% [parent pane] [label (sql-export-icon)]
         [stretchable-height #f] [stretchable-width #f])
    (let ((p2 (make-vertical-pane pane)))
      (set! message-field (new message% [parent p2] [label ""] [min-width 200]))
      (set! progress-bar (new gauge% [parent p2] [label ""] [range 100]))))
  (send progress-bar set-value 0)
  (send frame show #t)      ; on-superwindow-show will start the update thread
  (void))

(define edit-sz-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Sport Zones"] [icon (edit-icon)] [min-width 600] [min-height 500])

    (define database #f)
    (define parent-window #f)
    (define sport-choice #f)
    (define metric-choice #f)
    (define szlb #f)
    (define one-sz-edit-dlg #f)

    (let ((p (send this get-client-pane)))
      (let ((p1 (new vertical-pane%
                     [stretchable-height #t]
                     [parent p] [spacing 5]
                     [alignment '(center center)])))
        (let ((p2 (new horizontal-pane%
                       [stretchable-height #f]
                       [border 15]
                       [parent p1] [spacing 5]
                       [alignment '(right center)])))
          (set! sport-choice
                (new choice% [parent p2]
                     [stretchable-width #f]
                     [label "Show zones for: "]
                     [choices '("Cycling" "Running")]
                     [callback (lambda (c e) (on-sport-selected))]))
          (set! metric-choice
                (new choice% [parent p2]
                     [stretchable-width #f]
                     [label "Zone metric : "]
                     [choices '("Heart Rate" "Pace / Speed" "Power")]
                     [callback (lambda (c e) (on-zone-metric-selected))]))
          (new message% [parent p2] [stretchable-width #t] [label ""])
          (new button% [parent p2]
               [stretchable-width #f]
               [label "Add"]
               [callback (lambda (b e) (on-add-sz))])
          (new button% [parent p2]
               [stretchable-width #f]
               [label "Edit"]
               [callback (lambda (b e) (on-edit-sz))])
          (new button% [parent p2] [label "Delete"]
               [stretchable-width #f]
               [callback (lambda (b e) (on-delete-sz))]))

        (set! szlb
              (new qresults-list%
                   [parent p1] [pref-tag 'activity-log:sz-editor]))))

    (define (selected-sport)
      (let ((index (send sport-choice get-selection)))
        (case index
          ((0) 2)                       ; cycling
          ((1) 1))))                    ; running

    (define (selected-zone-metric)
      (let ((index (send metric-choice get-selection)))
        (case (selected-sport)
          ((1) (case index              ; Running
                 ((0) 1)                ; HR
                 ((1) 2)))              ; Pace
          ((2) (case index              ; Cycling
                 ((0) 1)                ; HR
                 ((1) 3))))))           ; Power

    (define (on-sport-selected)
      (send metric-choice clear)
      (case (selected-sport)
        ((1)
         (send metric-choice append "Heart Rate")
         (send metric-choice append "Pace"))
        ((2)
         (send metric-choice append "Heart Rate")
         (send metric-choice append "Power")))
      (refresh-contents))

    (define (on-zone-metric-selected)
      (refresh-contents))

    (define (get-one-sz-edit-dialog)
      (unless one-sz-edit-dlg
        (set! one-sz-edit-dlg (new edit-one-sz-dialog%)))
      one-sz-edit-dlg)

    ;; Start a database transaction, if we haven't already started one.  This
    ;; is called the first time the contents of the SPORT_ZONE table are about
    ;; to be modified.
    (define (maybe-start-transaction)
      (unless (in-transaction? database)
        (start-transaction database #:option 'immediate)))

    (define (on-add-sz)
      (let* ((sport (selected-sport))
             (metric (selected-zone-metric)))
        (cond
          ((send (get-one-sz-edit-dialog)
                 show-dialog
                 (send this get-top-level-window)
                 sport
                 metric
                 #f #f)
           => (lambda (zones)
                (maybe-start-transaction)
                (put-sport-zone database sport metric (car zones) (cdr zones))
                (refresh-contents))))))

    (define (on-edit-sz)
      (let ((selected-row (send szlb get-selected-row-index)))
        (when selected-row
          (let* ((sport (selected-sport))
                 (metric (selected-zone-metric))
                 (data (send szlb get-data-for-row selected-row))
                 (zones (get-sport-zone-values database (sz-id data)))
                 (valid-from (sz-valid-from data)))
            (cond ((send (get-one-sz-edit-dialog)
                         show-dialog
                         (send this get-top-level-window)
                         sport
                         metric
                         valid-from
                         zones)
                   => (lambda (zones)
                        (maybe-start-transaction)
                        (delete-sport-zone database (sz-id data))
                        (put-sport-zone database sport metric (car zones) (cdr zones))
                        (refresh-contents))))))))
    
    ;; Called when the user clicks the "Delete" button.  Deletes the selected
    ;; entry from the database.  A transaction is started if needed, so if the
    ;; user cancels the dialog, all changes are rolled back.
    (define (on-delete-sz)
      (let ((selected-row (send szlb get-selected-row-index)))
        (when selected-row
          (let ((data (send szlb get-data-for-row selected-row)))
            (maybe-start-transaction)
            (delete-sport-zone database (sz-id data))
            (refresh-contents)))))

    (define cdefs (make-sz-columns))

    ;; Refresh the contents of the list box (called when the contents of the
    ;; database have changed)
    (define (refresh-contents)
      (let* ((sport (selected-sport))
             (metric (selected-zone-metric))
             (rows (get-sport-zone-data database sport metric)))
        (send szlb setup-column-defs cdefs)
        (send szlb set-data rows)))

    ;; Number of sessions that would need their TIZ updated when we show this
    ;; dialog (and therefore have not updated anything).
    (define original-modified-session-count 0)

    ;; Show a confirmation dialog if too many sessions need to be updated.
    ;; There is a bit of heuristic here: some sessions will always show up in
    ;; the GET-MODIFIED-SESSIONS, usually because they don't have any HR/POWER
    ;; data and there are HR/POWER zones that cover their date range.  We try
    ;; to capture these in ORIGINAL-MODIFIED-SESSION-COUNT and only show the
    ;; dialog if more sessions than that have been updated.  However,
    ;; ORIGINAL-MODIFIED-SESSION-COUNT might also contain sessions that, for
    ;; one reason or another, have failed to update last time we opened this
    ;; dialog, so we return #t even if we don't ask for confirmation.
    ;; Regardless of outcome, we will always update the TIZ if we have any
    ;; sessions in the list.
    (define (maybe-show-confirmation-dialog num-sessions)
      (if (> (abs (- num-sessions original-modified-session-count)) 10)
          (eq? (message-box
                "Confirmation"
                (format (string-append "~a sessions are affected by the changes and will need to be updated.  "
                                       "Continue and update these sessions?")
                        num-sessions)
                (send this get-top-level-window)
                '(caution yes-no))
               'yes)
          #t))

    (define/override (on-finish-edit result)
      ;; NOTE: we need to return #t if we want to close the dialog, #t
      ;; otherwise
      (if result
          (let* ((sessions (get-modified-sessions database))
                 (num-sessions (length sessions)))
            (cond ((= num-sessions 0) #t)
                  ((maybe-show-confirmation-dialog num-sessions)
                   (maybe-start-transaction)
                   (interactive-update-time-in-zone
                    sessions
                    database
                    (send this get-top-level-window))
                   #t)
                  (#t #f)))
          #t            ; user clicked "Cancel", return #t to close the dialog
          ))

    (define/public (show-dialog parent db)
      (set! database db)
      (set! parent-window parent)
      (on-sport-selected)
      (refresh-contents)
      (set! original-modified-session-count (length (get-modified-sessions database)))
      (if (send this do-edit parent)
          (when (in-transaction? database)
            (commit-transaction database))
          (when (in-transaction? database)
            (rollback-transaction database)))
      (set! database #f))

    ))

(define the-sz-editor #f)

(define (get-sz-editor)
  (unless the-sz-editor
    (set! the-sz-editor (new edit-sz-dialog%)))
  the-sz-editor)
