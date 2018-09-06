#lang racket/base
;; al-widgets.rkt -- specific widgets to the ActivityLog2 application
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/math
         racket/match
         racket/dict
         "fit-file/activity-util.rkt"
         "fmt-util.rkt"
         "sport-charms.rkt"
         "widgets/main.rkt"
         "intervals.rkt"
         "utilities.rkt"
         "data-frame/df.rkt")

(provide sport-selector%)
(provide label-input-field%)
(provide equipment-input-field%)
(provide interval-view%)
(provide mini-interval-view%)
(provide swim-lengths-view%)
(provide interval-choice%)
(provide get-sql-export-dialog)

;; Some generic preferences

(define al-pref-tablet-friendly?
  (let* ((tag 'activity-log:tablet-friendly?)
         (val (get-pref tag (lambda () #f))))
    (make-parameter
     val
     (lambda (new-val)
       ;; Write the value back to the store
       (put-pref tag new-val)
       (notify-user 'info "restart application for tablet friendly setting to take effect")
       new-val))))
(provide al-pref-tablet-friendly?)

(define al-dlg-item-font
  (if (al-pref-tablet-friendly?)
      (send the-font-list find-or-create-font 16 'default 'normal 'normal)
      normal-control-font))
(provide al-dlg-item-font)

(define al-dlg-item-spacing
  (if (al-pref-tablet-friendly?)
      20
      10))
(provide al-dlg-item-spacing)


;;....................................................... sport-selector ....

(define sport-selector%
  (class object%
    (init parent [label "Sport: "])
    (init-field [callback #f]
                [sports-in-use-only? #t]
                [sport-filter values])
    (super-new)

    (define sports
      (filter sport-filter
              (if sports-in-use-only?
                  (get-sport-names-in-use)
                  (get-sport-names))))

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
      (define idx (send the-selector get-selection))
      (and idx (get-sport-ids idx)))

    (define/public (enable enable?)
      (send the-selector enable enable?))

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
  (class tag-input-field% (init) (super-new)
    (inherit set-available-tags set-contents get-contents)

    (define database #f)
    (define available-tags '())
    (define session-tags '())
    (define session-id #f)

    (define/override (clear-contents)
      (set! available-tags '())
      (set! session-tags '())
      (set! session-id #f)
      (super clear-contents))

    (define/public (setup-for-session db sid)
      (clear-contents)
      (set! database db)
      (set! session-id sid)
      (set! available-tags (get-available-tags db))
      (set! session-tags (if sid (get-session-tags db sid) '()))
      (set-available-tags (for/list ([tag available-tags]) (vector-ref tag 1)))
      (set-contents (for/list ([tag session-tags]) (vector-ref tag 1))))

    ;; Refresh the available tags from the database.  These are the tags that
    ;; will appear in the pop-up menu for adding new tags to the control.
    (define/public (refresh-available-tags db)
      (set! available-tags (get-available-tags db))
      (set-available-tags (for/list ([tag available-tags]) (vector-ref tag 1))))

    (define/public (get-contents-as-tag-ids)
      ;; NOTE: available-tags contains tags that can be set for a session,
      ;; while session-tags might contain additional tags.  This is especially
      ;; true if we use this to edit equipment, we can only add non-retired
      ;; equipment, but the session might contain retiered equipment and we
      ;; want to keep that.
      (define (find-tag tag)
        (or
         (for/first ([t (in-sequences session-tags available-tags)]
                     #:when (string=? tag (vector-ref t 1)))
           (vector-ref t 0))
         (begin (dbglog "unexpected tag ~a for SID ~a" tag session-id) #f)))

      (let ((contents (get-contents)))
        (for/list ([tag contents])
          (find-tag tag))))

    ;; Set contents of this widget from the list of TAG-IDS.  The actual tag
    ;; names are looked up in AVAILABLE-TAGS.
    (define/public (set-contents-from-tag-ids tag-ids)
      (define tags
        (for/list ([t (in-list available-tags)] #:when (member (vector-ref t 0) tag-ids))
          (vector-ref t 1)))
      (set-available-tags (for/list ([tag available-tags]) (vector-ref tag 1)))
      (set-contents tags))

    (define/public (update-session-tags sid)
      (let ((ids (get-contents-as-tag-ids)))
        (set-session-tags database sid ids)))

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


;..................................................... interval-choice% ....

(struct split (name tag fn))


;; A choice box widget that can select various types of intervals to be
;; displayed in an associated interval-view% widget.
(define interval-choice%
  (class object%
    (init-field parent [tag 'lap-type-selector] [label "Show Split Types: "])
    (super-new)

    (define interval-view #f)           ; the view we are controlling
    (define session #f) ; the session object from which we fetch recorded laps
    (define data-frame #f)
    (define sport #f)
    (define sid #f)

    ;; Remember the type of intervals last used for each sport type.  We will
    ;; restore these when a session for the same sport is installed.
    (define interval-type-by-sport (make-hash))

    ;; Restore preferences
    (let ((pref (get-pref tag (lambda () #f))))
      (when (and pref (eq? (car pref) 'gen1))
        (match-define (list 'gen1 ltbs) pref)
        (set! interval-type-by-sport (hash-copy ltbs))))

    ;; Switch the interval view to display splits by 1 km
    (define (on-km-splits)
      (define km-splits (df-get-property data-frame 'intervals-km-splits))
      (unless km-splits
        (set! km-splits (make-split-intervals data-frame "dst" 1000))
        (df-put-property data-frame 'intervals-km-splits km-splits))
      (send interval-view set-intervals sport 'default km-splits sid))

    ;; Switch the interval view to display splits by 1 mile
    (define (on-mile-splits)
      (define mile-splits (df-get-property data-frame 'intervals-mile-splits))
      (unless mile-splits
        (set! mile-splits (make-split-intervals data-frame "dst" 1600))
        (df-put-property data-frame 'intervals-mile-splits mile-splits))
      (send interval-view set-intervals sport 'default mile-splits sid))

    ;; Switch the interval view to display the climbs in the session
    (define (on-climb-splits)
      (define climb-splits (df-get-property data-frame 'intervals-climb-splits))
      (unless climb-splits
        (set! climb-splits (make-climb-intervals data-frame))
        (df-put-property data-frame 'intervals-climb-splits climb-splits))
      (send interval-view set-intervals sport 'hill-climbs climb-splits sid))

    ;; Switch the interval view to display the descents in the session
    (define (on-descent-splits)
      (define descent-splits (df-get-property data-frame 'intervals-descent-splits))
      (unless descent-splits
        (set! descent-splits (make-climb-intervals data-frame #:descents #t))
        (df-put-property data-frame 'intervals-descent-splits descent-splits))
      (send interval-view set-intervals sport 'hill-descents descent-splits sid))

    ;; Switch the interval view to display the "bests" in the session.  For a
    ;; bike activity, sections with best power for various lengths of time are
    ;; displayed, for other activities, the best speed over various distances
    ;; is displayed.
    (define (on-best-splits)
      (define best-splits (df-get-property data-frame 'intervals-best-splits))
      (unless best-splits
        (set! best-splits
              (if (eq? (vector-ref sport 0) 2)
                  (make-best-power-intervals data-frame)
                  (make-best-pace-intervals data-frame)))
        (df-put-property data-frame 'intervals-best-splits best-splits))
      (send interval-view set-intervals sport 'best-splits best-splits))

    ;; Switch the interval view to display the splits as recorded by the
    ;; device
    (define (on-recorded-splits)
      (send interval-view set-intervals sport 'default (session-laps session) sid))

    ;; List the kind of splits we can display, together with the function that
    ;; can switch to them.
    (define split-kinds
      (list
       (split "As Recorded" 'as-recorded on-recorded-splits)
       (split "Km Splits" 'km-splits on-km-splits)
       (split "Mile Splits" 'mile-splits on-mile-splits)
       (split "Hill Climbs" 'hill-climbs on-climb-splits)
       (split "Hill Descents" 'hill-descents on-descent-splits)
       (split "Best Efforts" 'best-splits on-best-splits)))

    (define choice
      (new choice% [parent parent]
           [label label]
           [choices (map split-name split-kinds)]
           [callback (lambda (c e) (on-select-split-kind (send c get-selection)))]))

    (define (on-select-split-kind index)
      ;; NOTE: we check here if the widgets are valid, so the "on-*" don't
      ;; have to to any validity checking.
      (when (and data-frame session interval-view)
        (let ((split (list-ref split-kinds index)))
          (hash-set! interval-type-by-sport sport (split-tag split))
          ((split-fn split)))))

    ;; Setup the interval kind that was used when a session for the same sport
    ;; as the current one was last selected.
    (define (setup-interval-type-by-sport)
      (let ((tag (hash-ref interval-type-by-sport sport #f)))
        (if tag
            (for/first (((split index) (in-indexed split-kinds))
                        #:when (eq? (split-tag split) tag))
              (send choice set-selection index)
              ((split-fn split)))
            (begin
              (send choice set-selection 0)
              ((split-fn (car split-kinds)))))))

    (define/public (save-visual-layout)
      (put-pref tag (list 'gen1 interval-type-by-sport)))

    (define/public (set-session s df)
      (set! session s)
      (set! data-frame df)
      (set! sport (df-get-property data-frame 'sport))
      (set! sid (df-get-property data-frame 'session-id))
      (define is-lap-swim? (df-get-property data-frame 'is-lap-swim?))
      (send choice enable (not is-lap-swim?))
      (when is-lap-swim? (send choice set-selection 0))
      (setup-interval-type-by-sport))

    ;; Set the interval view we are about to control.
    (define/public (set-interval-view lv)
      (set! interval-view lv))

    ))

;; Implements laps-view% which displays the laps of an activity


;........................................................... lap fields ....

;; Define the fields that show up in the lap view for each sport.  For each
;; field, we define the column name an extracor (which retrieves the value
;; from the lap) and a formatter which converts the value to a string for
;; display.

(define (mk-qcolumn name extractor formatter)
  (qcolumn
   name
   (lambda (lap)
     (let ((value (extractor lap)))
       (if value (formatter (extractor lap)) "")))
   (lambda (lap)
     (or (extractor lap) 0))))

(define (lap-num l)
  (dict-ref l 'lap-num #f))

(define (lap-num-fmt num)
  (format "~a" num))

(define (lap-num-pretty l)
  (format "~a" (dict-ref l 'lap-num-pretty #f)))

(define *run-lap-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Time of day" lap-start-time time-of-day->string)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Moving Time" lap-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Pace" lap-avg-speed pace->string)
   (mk-qcolumn "Best Pace" lap-max-speed pace->string)
   (mk-qcolumn "Speed" lap-avg-speed speed->string)
   (mk-qcolumn "Max Speed" lap-max-speed speed->string)
   (mk-qcolumn "HR" lap-avg-hr n->string)
   (mk-qcolumn "Max HR" lap-max-hr n->string)
   (mk-qcolumn "Pa:HR" lap-aerobic-decoupling pct->string)
   (mk-qcolumn "Cadence" lap-avg-cadence n->string)
   (mk-qcolumn "Max Cadence" lap-max-cadence n->string)
   (mk-qcolumn "VOSC" lap-avg-vertical-oscillation vosc->string)
   (mk-qcolumn "VRATIO" lap-avg-vratio pct->string)
   (mk-qcolumn "GCT" lap-avg-stance-time n->string)
   (mk-qcolumn "L-R Bal" lap-left-right-balance pct->string)
   (mk-qcolumn "Stride" lap-avg-stride stride->string)
   (mk-qcolumn "Calories" lap-calories n->string)
   (mk-qcolumn "Ascent" lap-total-ascent n->string)
   (mk-qcolumn "Descent" lap-total-descent n->string)))

(define *run-mini-lap-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Pace" lap-avg-speed pace->string)))

(define *run-mini-ascend-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Ascent" lap-total-ascent n->string)))

(define *run-mini-descend-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Descent" lap-total-descent n->string)))

(define *bike-lap-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Time of day" lap-start-time time-of-day->string)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Moving Time" lap-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Speed" lap-avg-speed speed->string)
   (mk-qcolumn "Max Speed" lap-max-speed speed->string)
   (mk-qcolumn "HR" lap-avg-hr n->string)
   (mk-qcolumn "Max HR" lap-max-hr n->string)
   (mk-qcolumn "Pw:HR" lap-aerobic-decoupling pct->string)
   (mk-qcolumn "Cadence" lap-avg-cadence n->string)
   (mk-qcolumn "Max Cadence" lap-max-cadence n->string)

   (mk-qcolumn "Power" lap-avg-power n->string)
   (mk-qcolumn "Max Power" lap-max-power n->string)
   (mk-qcolumn "Weighted Power" lap-normalized-power n->string)

   (mk-qcolumn "L-R Bal" lap-left-right-balance pct->string)
   (mk-qcolumn "Left TEff" lap-avg-left-torque-effectiveness n->string)
   (mk-qcolumn "Right TEff" lap-avg-right-torque-effectiveness n->string)
   (mk-qcolumn "Left PSmth" lap-avg-left-pedal-smoothness n->string)
   (mk-qcolumn "Right PSmth" lap-avg-right-pedal-smoothness n->string)

   (mk-qcolumn "Left PCO" lap-avg-left-pco
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Right PCO" lap-avg-right-pco
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Left PP Start" lap-avg-left-pp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Left PP End" lap-avg-left-pp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Right PP Start" lap-avg-right-pp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Right PP End" lap-avg-right-pp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Left Peak PP Start" lap-avg-left-ppp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Left Peak PP End" lap-avg-left-ppp-end
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Right Peak PP Start" lap-avg-right-ppp-start
                   (lambda (val) (number->string (exact-round val))))
   (mk-qcolumn "Right Peak PP End" lap-avg-right-ppp-end
                   (lambda (val) (number->string (exact-round val))))

   (mk-qcolumn "Calories" lap-calories n->string)
   (mk-qcolumn "Ascent" lap-total-ascent n->string)
   (mk-qcolumn "Descent" lap-total-descent n->string)))

(define *bike-mini-lap-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Speed" lap-avg-speed speed->string)))

(define *bike-mini-ascend-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Ascent" lap-total-ascent n->string)))

(define *bike-mini-descend-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Descent" lap-total-descent n->string)))

(define *bike-mini-best-fields*
  (list
   (mk-qcolumn "Lap" lap-num lap-num-fmt)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance distance->string)
   (mk-qcolumn "Power" lap-avg-power n->string)))

(define *swim-lap-fields*
  (list
   ;; (mk-qcolumn "Lap #" lap-num lap-num-fmt)
   (qcolumn "Lap" lap-num-pretty lap-num)
   (mk-qcolumn "Swim Stroke" lap-swim-stroke get-swim-stroke-name)
   ;; (mk-qcolumn "Time of day" lap-start-time format-time-of-day)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Moving Time" lap-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Lengths" lap-num-lengths n->string)
   (mk-qcolumn "Distance" lap-distance n->string)
   (mk-qcolumn "Pace" lap-avg-speed swim-pace->string)
   (mk-qcolumn "Best Pace" lap-max-speed swim-pace->string)
   (mk-qcolumn "SWOLF" lap-avg-swolf n->string)
   (mk-qcolumn "Best SWOLF" lap-best-swolf n->string)
   (mk-qcolumn "Total Strokes" lap-total-cycles n->string)
   (mk-qcolumn "Avg Strokes" lap-avg-strokes n->string)
   (mk-qcolumn "Cadence" lap-avg-cadence n->string)
   (mk-qcolumn "Max Cadence" lap-max-cadence n->string)
   (mk-qcolumn "Calories" lap-calories n->string)))

(define *swim-mini-lap-fields*
  (list
   ;; (mk-qcolumn "Lap #" lap-num lap-num-fmt)
   (qcolumn "Lap" lap-num-pretty lap-num)
   ;; (mk-qcolumn "Time of day" lap-start-time format-time-of-day)
   (mk-qcolumn "Duration" lap-elapsed-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Distance" lap-distance n->string)
   (mk-qcolumn "Pace" lap-avg-speed swim-pace->string)))

(define (length-num l)
  (dict-ref l 'length-num #f))

(define (length-num-fmt num)
  (format "~a" num))

;; For swimming activitites, we display the lengths of the selected lap
(define *swim-length-fields*
  (list
   (mk-qcolumn "Length" length-num length-num-fmt)
   (mk-qcolumn "Swim Stroke" length-swim-stroke get-swim-stroke-name)
   (mk-qcolumn "Duration" length-time (lambda (v) (duration->string v #t)))
   (mk-qcolumn "Strokes" length-total-cycles n->string)
   (mk-qcolumn "SWOLF" length-swolf n->string)
   (mk-qcolumn "Pace" length-avg-speed swim-pace->string)))

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

(define *mini-lap-definitions*
  (hash
   (cons 1 'hill-climbs) *run-mini-ascend-fields*
   (cons 1 'hill-descents) *run-mini-descend-fields*
   1 *run-mini-lap-fields*
   (cons 2 'hill-climbs) *bike-mini-ascend-fields*
   (cons 2 'hill-descents) *bike-mini-descend-fields*
   (cons 2 'best-splits) *bike-mini-best-fields*
   2 *bike-mini-lap-fields*
   ;; Generic sports
   (cons 0 'hill-climbs) *bike-mini-ascend-fields*
   (cons 0 'hill-descents) *bike-mini-descend-fields*
   0 *bike-mini-lap-fields*
   ;; Alpine Skiing
   (cons 13 'hill-climbs) *bike-mini-ascend-fields*
   (cons 13 'hill-descents) *bike-mini-descend-fields*
   13 *bike-mini-lap-fields*
   ;; Hiking Skiing
   (cons 256 'hill-climbs) *bike-mini-ascend-fields*
   (cons 256 'hill-descents) *bike-mini-descend-fields*
   256 *bike-mini-lap-fields*

   5 *swim-mini-lap-fields*))

(define *default-mini-lap-fields* *bike-mini-lap-fields*)

(define (get-lap-field-definitions sport)
  (cond ((assoc (vector-ref sport 0) *sport-lap-definitions*) => cdr)
	(#t *default-lap-fields*)))

(define (get-mini-lap-field-definitions sport (topic 'default))
  (define (href key) (hash-ref *mini-lap-definitions* key #f))
  (or (href (cons sport topic))
      (href sport)
      (and (vector? sport)
           (or (href (cons (vector-ref sport 0) topic))
               (href (vector-ref sport 0))))
      *default-mini-lap-fields*))


;............................................................ interval-view% ....

;; Display the laps of a session in a list box.  Provides a call back for
;; notifications when a lap is selected.
(define interval-view%
  (class object%
    (init-field parent tag callback)
    (super-new)

    (define lb
      (new (class qresults-list% (init) (super-new)
             (define/override (on-select row-index row-data)
               (when callback
                 (callback row-index row-data))))
           [pref-tag tag]
           [parent parent]))

    (define/public (lap-field-definitions sport topic)
      (get-lap-field-definitions sport))

    ;; Add lap numbers to the laps, if a lap has 0 distance, label it as
    ;; "Rest"
    (define (number-session-laps laps)
      (let ((lap-num 0)
            (lap-num-pretty 0))
        (for/list ([lap (in-list laps)])
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

    (define/public (set-intervals sport topic intervals (database-id #f))
      (send lb set-default-export-file-name
            (format "~a-intervals.csv" (or database-id "session")))
      ;; Make sure we use the correct tag for the column definitions
      (let* ((stag (if (vector? sport)
                       (if (vector-ref sport 1)
                           (format "~a-~a" (vector-ref sport 0) (vector-ref sport 1))
                           (format "~a" (vector-ref sport 0)))
                       (format "~a" sport)))
             (vtag (format "~a-~a-~a" tag stag topic)))
        (send lb set-tag (string->symbol vtag)))
      (send lb setup-column-defs (lap-field-definitions sport topic))
      (send lb set-data (number-session-laps intervals)))

    (define/public (save-visual-layout)
      (send lb save-visual-layout))

    ))


;;....................................................... mini-interval-view% ....

(define mini-interval-view%
  (class interval-view%
    (init parent callback)
    (super-new [parent parent] [callback callback])

    (define/override (lap-field-definitions sport topic)
      (get-mini-lap-field-definitions sport topic))

    ))


;;................................................... swim-lengths-view% ....

;; Display information about swim lengths in a list box.
(define swim-lengths-view%
  (class object%
    (init parent tag)
    (super-new)

    (define parent-panel parent)
    (define deleted? #t)

    (define (paint-label canvas dc)
      (define msg "Lengths for lap")
      (define font
        (send the-font-list find-or-create-font 14 'default 'normal 'normal))
      (let-values (([cw ch] (send dc get-size))
                   ([w h x y] (send dc get-text-extent msg font #t)))
        (send dc set-font font)
        (define text-color (make-color #x2f #x4f #x4f))
        (send dc set-text-foreground text-color)
        (let ((ox (- (/ cw 2) (/ h 2)))
              (oy (+ (/ ch 2) (/ w 2))))
          (send dc draw-text msg ox oy #t 0 (/ pi 2)))))

    (define p (new horizontal-panel% [parent parent-panel] [style '(deleted)]))
    (define lc (new canvas% [parent p] [min-width 40] [stretchable-width #f]
                    [paint-callback paint-label]))
    (define lb (new qresults-list% [pref-tag tag] [parent p]))

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
               [label (sql-export-icon)]
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
