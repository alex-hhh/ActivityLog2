#lang racket/base
;; trends-chart.rkt -- common trend chart functionality
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018, 2021, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         pict
         plot-container/hover-util
         plot/no-gui
         racket/class
         racket/contract
         racket/date
         racket/format
         racket/gui/base
         racket/match
         racket/math
         racket/string
         racket/vector
         "../al-widgets.rkt"
         "../database.rkt"
         "../fmt-util-ut.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt")

(provide
 trends-chart%
 session-filter%
 chart-settings-interface<%>)

(provide/contract
 (get-true-min-start-date (-> connection? exact-integer?))
 (pmc-date-ticks (-> ticks?))
 (generate-timestamps (-> exact-integer? exact-integer? (or/c 0 1 2)
                          (listof exact-integer?)))
 (start-of-day (-> exact-integer? exact-integer?))
 (str->date (-> string? (or/c #f exact-integer?)))
 )

(provide
 pad-data
 simplify-labels
 read-session-markers
 allocate-marker-slots
 make-session-marker-renderers
 make-session-marker-renderers/histogram)

;; A "session filter" widget: displays options for selecting sessions based on
;; sport, date-range, labels and equipment.  This is just a convenient package
;; for the four widgets together with save/restore functionality.  It is used
;; as part of the settings dialogs for several trends charts.
(define session-filter%
  (class object%
    (init parent)
    (init-field database
                sport-charms
                [sport-selected-callback #f]
                [sport-filter values])
    (super-new)

    (define gb (make-group-box-panel parent))
    (define hpane (make-horizontal-pane gb))
    (define left-pane (make-vertical-pane hpane #f))
    (define sport-selector
      (new sport-selector%
           [parent left-pane]
           [sport-charms sport-charms]
           [sports-in-use-only? #t]
           [callback sport-selected-callback]
           [sport-filter sport-filter]))

    (define date-range-selector
      (new date-range-selector% [parent left-pane]))

    (define right-pane (make-vertical-pane hpane))

    (define labels-input (new label-input-field% [parent right-pane]))
    (define equipment-input (new equipment-input-field% [parent right-pane]))

    ;; Returns "save" data that can be set back to this widget via
    ;; `restore-from` so the widget will display the same selection.
    (define/public (get-restore-data)
      (hash
       'date-range (send date-range-selector get-restore-data)
       'timestamps (send date-range-selector get-selection)
       'sport (send sport-selector get-selection)
       'labels (send labels-input get-contents-as-tag-ids)
       'equipment (send equipment-input get-contents-as-tag-ids)))

    ;; Restore data produced by `get-restore-data`.  After this call, the
    ;; widget will display the same selection as when `get-restore-data` was
    ;; called.
    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database))
        (send labels-input refresh-available-tags database)
        (send equipment-input refresh-available-tags database))
      (when (hash? data)
        (let ((labels (hash-ref data 'labels '())))
          ;; NOTE: set the contents even if they are empty, as this sets the
          ;; available tags, allowing new ones to be added
          (send labels-input set-contents-from-tag-ids labels))
        (let ((equipment (hash-ref data 'equipment '())))
          ;; NOTE: set the contents even if they are empty, as this sets the
          ;; available tags, allowing new ones to be added
          (send equipment-input set-contents-from-tag-ids equipment))
        (let ((dr (hash-ref data 'date-range #f)))
          (when dr
            (send date-range-selector restore-from dr)))
        (let ((sp (hash-ref data 'sport #f)))
          (when sp
            (send sport-selector set-selected-sport (car sp) (cdr sp))
            (and sport-selected-callback (sport-selected-callback sp))))))

    ;; Return the selected date range as a (cons START END), where START and
    ;; END are unix timestamps.
    (define/public (get-date-range)
      (match-define (cons start end) (send date-range-selector get-selection))
      (cons (if (and database (= start 0))
                (get-true-min-start-date database)
                start)
            end))

    ;; Returns the selected sport as a (cons SPORT SUB-SPORT)
    (define/public (get-sport) (send sport-selector get-selection))

    ;; Returns the list of selected labels as IDS (the LABEL.id value from the
    ;; database).  Returns the empty list if no labels are selected.

    (define/public (get-labels) (send labels-input get-contents-as-tag-ids))

    ;; Returns the list of selected equipment as IDS (the EQUIPMENT.id value
    ;; from the database.  Returns the empty list if no equipment is selected.
    (define/public (get-equipment) (send equipment-input get-contents-as-tag-ids))

    ;; Update the selectors with the possibly updated seasons, available
    ;; labels and equipment.  Should be called before showing the dialog that
    ;; contains this widget to ensure the menus are up to date with the
    ;; database contents.
    (define/public (on-before-show-dialog)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database))
        (send labels-input refresh-available-tags database)
        (send equipment-input refresh-available-tags database)))

    ))

;; Interface that has to be implemented by all chart settings dialogs
(define chart-settings-interface<%>
  (interface ()
    (get-chart-settings (->m (hash/c symbol? any/c)))
    (put-chart-settings (->m (hash/c symbol? any/c) any/c))
    (show-dialog (->m (or/c (is-a?/c area<%>) #f) (or/c (hash/c symbol? any/c) #f)))))

;; Base class for all the trends charts. Contains the common mechanisms for
;; interacting with the rest of the application, so the individual charts can
;; focus on getting the data and building the charts only.
(define trends-chart%
  (class object%
    (init)
    (super-new)

    (define settings-dialog #f)

    ;; The parameters for the chart, a hash having at least 'name and 'title
    ;; keys.
    (define chart-settings #f)

    ;; Used to receive notifications about changed sessions, etc.
    (define change-notification-source (make-log-event-source))

    ;; Create a new settings dialog for this class.  This needs to be
    ;; overridden.
    (define/public (make-settings-dialog)
      #f)

    ;; Indicate that the chart params have been changed, and the data for the
    ;; chart will need to be updated.  This needs to be overriden.
    (define/public (invalidate-data)
      #f)

    ;; Return #t if this trends chart needs to be refreshed because the
    ;; underlying session data has changed.
    (define/public (need-refresh?)
      (define events (collect-events change-notification-source))
      (is-invalidated-by-events? events))

    ;; Return true if the data in this trend chart is invalidated by EVENTS as
    ;; returned by 'collect-events'.  This method should not invalidate the
    ;; data -- 'invalidate-data' is used for that.
    ;;
    ;; The default implementation will return true if anything has changed in
    ;; the database.  Individual trends charts might override this method to
    ;; return true only for a subset of the events (e.g. the body weight chart
    ;; should only be updated if athlete metrics are changed).
    (define/public (is-invalidated-by-events? events)
      (> (hash-count events) 0))

    ;; Construct a new plot snip for the chart and insert it into CANVAS.  A
    ;; new plot snip needs to be constructed every time even if the data for
    ;; the plot has not changed.  This needs to be overriden.
    (define/public (put-plot-snip canvas)
      #f)

    ;; Save the plot to a file. The image is stored in FILE-NAME, and will
    ;; have WIDTH x HEIGHT dimensions.
    (define/public (save-plot-image file-name width height)
      #f)

    ;; Export the data for the plot as a CSV file.
    (define/public (export-data-to-file file-name formatted?)
      #f)

    (define/public (get-settings-dialog)
      (unless settings-dialog (set! settings-dialog (make-settings-dialog)))
      settings-dialog)

    (define/public (get-chart-settings)
      (unless chart-settings
        (set! chart-settings (send (get-settings-dialog) get-chart-settings)))
      chart-settings)

    (define/public (put-chart-settings data)
      (send (get-settings-dialog) put-chart-settings data)
      ;; set `chart-settings` to #f so that data is read at least once, so
      ;; that the 'timestamps entry is updated!
      (set! chart-settings #f)
      (invalidate-data))

    (define/public (get-name)
      (let ((p (get-chart-settings)))
        (if p (hash-ref p 'name "XXX (no params)") "XXX (no params)")))

    (define/public (get-title)
      (let ((p (get-chart-settings)))
        (if p (hash-ref p 'title "No Title (no params)") "No Title (no params)")))

    ;; Open a settings dialog (with PARENT as the parent window) and update
    ;; any chart settings. Returns #t if settings were updated, #f otherwise.
    (define/public (interactive-setup parent)
      (define dlg (get-settings-dialog))
      (cond ((send dlg show-dialog parent)
             => (lambda (dialog-result)
                  (set! chart-settings dialog-result)
                  (invalidate-data)
                  #t))
            (#t
             ;; Dialog has been canceled, restore previous settings so they
             ;; show up correctly next time the dialog is opened.
             (when chart-settings
               (send dlg put-chart-settings chart-settings))
             #f)))

    (define/public (refresh)
      (set! chart-settings #f))

    ))


;;................................................................ other ....


;; Convert a date received from SQLite (e.g 2014-12-01) to a unix timestamp
(define (str->date s)
  (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
    (if m
        (let ((year (string->number (list-ref m 1)))
              (month (string->number (list-ref m 2)))
              (day (string->number (list-ref m 3))))
          (date->seconds (date 0 0 0 day month year 0 0 #f 0)))
        #f)))

(define (start-of-day timestamp)
  (let ((d (seconds->date timestamp #t)))
    (find-seconds
     0 0 0
     (date-day d) (date-month d) (date-year d) #t)))

(define (week-start timestamp)
  (let* ((date (seconds->date timestamp))
         (week-day (date-week-day date))
         (delta (+ (* 24 3600 week-day)
                   (* 3600 (date-hour date))
                   (* 60 (date-minute date))
                   (date-second date))))
    ;; NOTE: calculation makes the week start on a Sunday, adjust for that.
    (+ (* 24 3600) (- timestamp delta))))

(define (next-week-start timestamp)
  (week-start (+ timestamp (* 7 24 3600) (* 12 60))))

(define (month-start timestamp)
  (let ((d (seconds->date timestamp)))
    (date->seconds
     (date 0 0 0
           1 (date-month d) (date-year d)
           0 0
           (date-dst? d)
           (date-time-zone-offset d)))))

(define (next-month-start timestamp)
  (let* ((d (seconds->date timestamp))
         (month (date-month d))
         (year (date-year d))
         (december? (eqv? month 12)))
    (date->seconds
     (date 0 0 0
           1
           (if december? 1 (+ 1 month))
           (if december? (+ 1 year) year)
           0 0
           (date-dst? d)
           (date-time-zone-offset d)))))

(define (year-start timestamp)
  (let ((d (seconds->date timestamp)))
    (date->seconds
     (date 0 0 0
           1 1 (date-year d)
           0 0
           (date-dst? d)
           (date-time-zone-offset d)))))

(define (next-year-start timestamp)
  (let ((d (seconds->date timestamp)))
    (date->seconds
     (date 0 0 0
           1 1 (+ 1 (date-year d))
           0 0
           (date-dst? d)
           (date-time-zone-offset d)))))

(define months '("XXX" "Jan" "Feb" "Mar"
                 "Apr" "May" "Jun" "Jul" "Aug"
                 "Sep" "Oct" "Nov" "Dec"))

;; Generate a list of UNIX timestamps between START and END based on GROUP-BY
;; (0 -- every week start, 1 -- every month start, 2 -- every year start
(define (generate-timestamps start end group-by)
  (let ((start (cond ((eq? group-by 0) (week-start start))
                     ((eq? group-by 1) (month-start start))
                     ((eq? group-by 2) (year-start start))
                     (#t #f)))
        (skip-fn (cond ((eq? group-by 0) next-week-start)
                       ((eq? group-by 1) next-month-start)
                       ((eq? group-by 2) next-year-start)
                       (#t #f))))
    (let loop ((crt start) (timestamps '()))
      (if (> crt end)
          (reverse timestamps)
          (loop (skip-fn crt) (cons crt timestamps))))))

;; Add entries to a data set for missing time periods.  TIMESTAMPS is a list
;; of timestamps for which we want to generate a graph.  DATA is a list of
;; points (vector date value) with some missing items (e.g. when no training
;; happened in a particular week, there will be no entry in DATA for that
;; week).  This function will return an updated data set with an item for each
;; timestamp in TIMESTAMPS, where there is a corresponding entry in DATA, it
;; will be used, otherwise a value of 0 will be inserted.
;;
;; RESULT is used internally should be left empty,not a good design :-)
(define (pad-data timestamps data [result '()])
  (cond ((null? timestamps) result)
        ((null? data) (pad-data (cdr timestamps) data (cons (vector #f) result)))
        (#t
         (let ((data-ts (str->date (vector-ref (car data) 0))))
           (cond ((< data-ts (car timestamps))
                  (pad-data timestamps (cdr data) (cons (car data) result)))
                 ((> data-ts (car timestamps))
                  (pad-data (cdr timestamps) data (cons (vector #f) result)))
                 (#t
                  (pad-data (cdr timestamps) (cdr data) (cons (car data) result))))))))

;; Convert LABEL (a string DD-MM-YYYY) into a simpler version, depeding on
;; GROUP-BY.  If group-by is 0 (weekly), it is unchanged, if group-by is 1,
;; the label is condidered montly and only "month year" is returned, if
;; group-by is 2, it is a yearly label and only the year is returned.
(define (convert-label label group-by)
  (cond ((= group-by 0) label)          ; week
        ((= group-by 1)
         (let ((date (seconds->date (str->date label))))
           (if (member (date-month date) '(1 4 7 10)) ; put only some of the months
               (format "~a ~a" (list-ref months (date-month date))
                       (date-year date))
               "")))
        ((= group-by 2)
         (let ((date (seconds->date (str->date label))))
           (format "~a" (date-year date))))))

;; Simplify the labels in DATA according to GROUP-BY (0 - weekly, 1 - monthy,
;; 2 yearly).  Labels are reformated to a more appropiate value and some
;; labels might be omited to declutter the data.
(define (simplify-labels data group-by)
  (for/list ([d data])
    (let ((label (vector-ref d 0))
          (nd (vector-copy d)))
      (when label
        (vector-set! nd 0 (convert-label label group-by)))
      nd)))

;; Return the earliest practical start date.  The date range selector returns
;; 0..TODAY for the "All Days" selection, but that creates useless charts.
;; Instead we look for the first activity in the database.
(define (get-true-min-start-date db)
  (let ((val (query-maybe-value db "select min(start_time) from A_SESSION")))
    ;; Can happen if the database is empty
    (if (sql-null? val) 0 val)))


;;; Date ticks

;; Define more reasonable date ticks for our graphs, as the default ones are
;; not useful.

;; Create ticks between START and END with major ticks every MAJOR month start
;; and minor ticks every month start.
(define (pmc-date-ticks-layout-year start end major)
  (for*/list ([ts (in-range (start-of-day start) end (* 24 3600))]
              [d (in-value (seconds->date ts #t))]
              #:when (eq? (date-day d) 1))
    (pre-tick ts (eq? (remainder (date-month d) major) 0))))


;; Create ticks between START and END, with major ticks at the start of the
;; month and minor ticks at the start of the week.  The minor ticks will not
;; be aligned with the major ones, but it is OK, as the ticks are more usefull
;; that way.
(define (pmc-date-ticks-layout-month start end)
  (for*/list ([ts (in-range (start-of-day start) end (* 24 3600))]
              [d (in-value (seconds->date ts #t))]
              #:when (or (eq? (date-week-day d) 1)
                         (eq? (date-day d) 1)))
    (pre-tick ts (eq? (date-day d) 1))))

;; Create ticks between START and END, with major ticks at the start of the
;; week and minor ticks at the start of each day.
(define (pmc-date-ticks-layout-week start end)
  (for/list ((ts (in-range (start-of-day start) end (* 24 3600))))
    (let ((d (seconds->date ts #t)))
      (pre-tick ts (eq? (date-week-day d) 1)))))

;; Create ticks between START and END, different ticks are created depending
;; on the distance between start and end.
(define (pmc-date-ticks-layout start end)
  (let ((s (exact-truncate start))
        (e (exact-truncate end)))
    (let ((ndays (/ (- e s) (* 24 3600))))
      (cond ((> ndays 1000)
             (pmc-date-ticks-layout-year s e 6))
            ((> ndays 400)
             (pmc-date-ticks-layout-year s e 3))
            ((> ndays 60)
             (pmc-date-ticks-layout-month s e))
            (#t
             (pmc-date-ticks-layout-week s e))))))

;; Create tick labels for each tick in PRE-TICKS.  START and END are ignored,
;; they are there to match the expected plot interface.
(define (pmc-date-ticks-format start end pre-ticks)
  (for/list ((tick (in-list pre-ticks)))
    (let ((s (seconds->date (pre-tick-value tick))))
      (format "~a ~a ~a" (date-day s) (list-ref months (date-month s)) (date-year s)))))

;; Create the X-Axis tiks to be used in PMC plots.
(define (pmc-date-ticks)
  (ticks pmc-date-ticks-layout pmc-date-ticks-format))


;;...................................................... session-markers ....

;; Query template to read a list of sessions and their labels (unfortunately
;; we need to use `format` for this query.  This is used in
;; `read-session-markers` below.  Note that our query returns at most one
;; label of each type for a day, this means, for example we won't show
;; multiple "race" labels in a triathlon, just because the swim, bike and run
;; legs were on the same day and all were labeled "race".
;;
;; Note also that we can handle null values for start and end times by
;; selecting a date a long time in the past or a long time in the future...

(define session-marker-query-template
  "
select date(S.start_time, 'unixepoch', 'localtime', 'start of day') as period,
       min(S.start_time),
       L.name
  from A_SESSION S, SESSION_LABEL SL, LABEL L, ACTIVITY A
 where SL.session_id = S.id
   and SL.label_id = L.id
   and S.activity_id = A.id
   and S.start_time between ifnull(?, strftime('%s', '1900-01-01'))
                        and ifnull(?, strftime('%s', '2500-01-01'))
   and SL.label_id in (~a)
group by period
order by S.start_time")

;; Read a set of session markers from the database DB based on data from
;; PARAMS.  PARAMS is a hash table and we look for two keys: 'timestamps, a
;; cons of START and END times and 'marker-labels a list of label IDs.  We
;; fetch all activities between the specified dates and having any of the
;; specified labels.
;;
;; Also note that we fetch activities, not sessions -- this allows multisport
;; activities to show up only once, since these multisport activities are
;; shown as multiple sessions.
;;
;; Returns a list of items, each item being a (vector DAY-STRING START-TIME
;; LABEL).  Returns an empty list if no sessions are found.
(define (read-session-markers db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (define labels (hash-ref params 'marker-labels '()))
  (if (null? labels)
      '()                 ; if no labels are selected, don't show any sessions
      (let ([q (format session-marker-query-template
                       (string-join (map ~a labels) ","))])
        (query-rows db q (or start sql-null) (or end sql-null)))))

(define (make-marker-badge label timestamp)
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'bold))
  (define text-color (make-object color% #x2f #x4f #x4f))
  (define text-face (cons text-color font))
  (define background (make-object color% #xcc #xdd #xaa 0.95))
  (define outline-color (make-object color% #x22 #x55 #x22))

  (define p1 (vc-append 3 (text label text-face)
                        (text (calendar-date->string timestamp) text-face)))
  (cc-superimpose
   (filled-rounded-rectangle (+ (pict-width p1) 15)
                             (+ (pict-height p1) 15) -0.1
                             #:draw-border? #t
                             #:border-color outline-color
                             #:border-width 1.1
                             #:color background)
   p1))

;; Return a list of renderers corresponding to SESSION-MARKERS (as produced by
;; `read-session-markers`).  The markers will be lines (whose color is
;; controlled by #:color) and have labels with the label name and date of the
;; session.  Labels are placed at the #:y position, the top anchor of the
;; label.
;;
;; The markers are drawn in the order they are given, but if
;; HIGHLIGHT-TIMESTAMP is provided, the marker closest to this timestamp will
;; be displayed last, and will render on top of the others.
;;
;; Limitations: there is no consideration of the plot size, so, if there are
;; too many labels close to each other in time, they might overlap.
(define (make-session-marker-renderers
         session-markers
         #:y y
         #:color [color "coral"]
         #:highlight-timestamp [highlight-timestamp 0])
  (for/fold ([toplevel #f]
             [others '()]
             #:result (reverse (if toplevel (cons toplevel others) others)))
            ([m (in-list session-markers)])
    (match-define (vector id timestamp label) m)
    (define renderer
      (list
       (vrule timestamp #:width 2 #:style 'short-dash #:color color)
       (point-pict (vector timestamp y)
                   (make-marker-badge label timestamp)
                   #:point-sym 'none
                   #:anchor 'top)))
    (cond [toplevel
           (values toplevel (cons renderer others))]
          [(>= highlight-timestamp timestamp)
           (values renderer others)]
          [#t
           (values toplevel (cons renderer others))])))

;; Allocate the markers (as read by `read-session-markers`) to slots
;; corresponding to the TIMESTAMPS list.  This is used to determine the
;; position of the markers on a histogram plot.  Fist timestamp in TIMESTAMPS
;; is at slot 0, second at slot 1, etc.  This function determines where in the
;; list of TIMESTAMPS each marker belongs and adds its position (slot) to the
;; marker.  Returns a list of (vector DAY-STRING START-TIME LABEL SLOT).
(define (allocate-marker-slots timestamps markers)
  (let loop ([timestamps timestamps]
             [index 0]
             [markers markers]
             [result '()])
    (cond ((null? markers)
           (reverse result))
          ((null? timestamps)
           (let loop ([markers markers]
                      [result result])
             (if (null? markers)
                 (reverse result)
                 (match-let ([(vector aid start-time label) (car markers)])
                   (loop (cdr markers) (cons (vector aid start-time label (sub1 index)) result))))))
          (#t
           (match-let ([(vector aid start-time label) (car markers)])
             (if (<= start-time (car timestamps))
                 (loop timestamps index (cdr markers) (cons (vector aid start-time label (sub1 index)) result))
                 (loop (cdr timestamps) (add1 index) markers result)))))))

;; Return a list of renderers corresponding to SESSION-MARKERS (as produced by
;; `allocate-marker-slots`).  The markers will be lines (whose color is
;; controlled by #:color) and have labels with the label name and date of the
;; session.  Labels are placed at the #:y position, the top anchor of the
;; label, the X position is determined by the marker slot (see
;; `allocate-marker-slots`) and, if multiple labels are present at a slot,
;; they are stacked on top of each other (most recent at the top)
;;
;; Limitations: there is no consideration of the plot size, so, if there are
;; too many labels, they might render under the histogram bars, and if they
;; are too close to each other, they will also overlap.
(define (make-session-marker-renderers/histogram
         session-markers
         #:y y
         #:color [color "coral"])
  ;; NOTE that several labels can be in the same slot, especially on plots
  ;; which are grouped by months or years.  We need to group these together
  ;; into a single pict...
  (define badges
    (for/fold ([result (hash)])
              ([m (in-list session-markers)])
      (match-define (vector id timestamp label slot) m)
      (define b (make-marker-badge label timestamp))
      (hash-update result slot
                   (lambda (prev) (cons b prev))
                   (lambda () '()))))

  (for/list ([key (in-list (sort (hash-keys badges) <))])
    (define b (hash-ref badges key))
    (define x (+ key 0.5))              ; they key is the histogram slot
    (list
     (vrule x #:width 2 #:style 'short-dash #:color color)
     (point-pict (vector x y)
                 (if (= (length b) 1) (car b) (apply vc-append 3 b))
                 #:point-sym 'none #:anchor 'top))))
