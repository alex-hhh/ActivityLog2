#lang racket/gui
;; view-trends.rkt -- trends graphs
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
         plot
         racket/date
         racket/stream
         "al-prefs.rkt"
         "al-widgets.rkt"
         "database.rkt"
         "icon-resources.rkt"
         "plot-builder.rkt"
         "snip-canvas.rkt"
         "sport-charms.rkt"
         "widgets.rkt"
         "plot-hack.rkt")

(provide view-trends%)


;;..................................................... helper functions ....

;; Convert a date received from SQLite (e.g 2014-12-01) to a unix timestamp
(define (str->date s)
  (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
    (if m
        (begin
          (let ((year (string->number (list-ref m 1)))
                (month (string->number (list-ref m 2)))
                  (day (string->number (list-ref m 3))))
            (date->seconds (date 0 0 0 day month year 0 0 #f 0))
            ))
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
           0 0 0 (date-time-zone-offset d)))))

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
           0 0 0 (date-time-zone-offset d)))))

(define (year-start timestamp)
  (let ((d (seconds->date timestamp)))
    (date->seconds
     (date 0 0 0
           1 1 (date-year d)
           0 0 0 (date-time-zone-offset d)))))

(define (next-year-start timestamp)
  (let ((d (seconds->date timestamp)))
    (date->seconds
     (date 0 0 0
           1 1 (+ 1 (date-year d))
           0 0 0 (date-time-zone-offset d)))))

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

;; Return the earliest practical start date.  The date range selector returns
;; 0..TODAY for the "All Days" selection, but that creates useless charts.
;; Instead we look for the first activity in the database.
(define (get-true-min-start-date db)
  (query-maybe-value db "select min(start_time) from A_SESSION"))


;;; Date ticks

;; Define more reasonable date ticks for our graphs, as the default ones are
;; not useful.

;; Create ticks between START and END with major ticks every MAJOR month start
;; and minor ticks every month start.
(define (pmc-date-ticks-layout-year start end major)
  (filter identity
          (for/list ((ts (in-range (start-of-day start) end (* 24 3600))))
            (let ((d (seconds->date ts #t)))
              (if (eq? (date-day d) 1)
                  (pre-tick ts (eq? (remainder (date-month d) major) 0))
                  #f)))))

;; Create ticks between START and END, with major ticks at the start of the
;; month and minor ticks at the start of the week.  The minor ticks will not
;; be aligned with the major ones, but it is OK, as the ticks are more usefull
;; that way.
(define (pmc-date-ticks-layout-month start end)
  (filter identity
          (for/list ((ts (in-range (start-of-day start) end (* 24 3600))))
            (let ((d (seconds->date ts #t)))
              (if (or (eq? (date-week-day d) 1)
                      (eq? (date-day d) 1))
                  (pre-tick ts (eq? (date-day d) 1))
                  #f)))))

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

(define months '("XXX" "Jan" "Feb" "Mar" 
                 "Apr" "May" "Jun" "Jul" "Aug"
                 "Sep" "Oct" "Nov" "Dec"))

;; Create tick labels for each tick in PRE-TICKS.  START and END are ignored,
;; they are there to match the expected plot interface.
(define (pmc-date-ticks-format start end pre-ticks)
  (for/list ((tick (in-list pre-ticks)))
    (let ((s (seconds->date (pre-tick-value tick))))
      (format "~a ~a ~a" (date-day s) (list-ref months (date-month s)) (date-year s)))))

;; Create the X-Axis tiks to be used in PMC plots.
(define (pmc-date-ticks)
  (ticks pmc-date-ticks-layout pmc-date-ticks-format))


;;........................................................ trends-chart% ....

;; Basic parameters for a chart.  This is shared with all the charts.
;; Individual charts will define another params structure derived from this
;; one.
(struct basic-params (name title))

;; Base class for all the trends charts. Contains the common mechanisms for
;; interacting with the rest of the application, so the individual charts can
;; focus on getting the data and building the charts only.
(define trends-chart%
  (class object%
    (init)
    (super-new)

    (define settings-dialog #f)

    ;; Keep a copy of the last good retore data from the settings dialog.  The
    ;; restore data from the dialog might be wrong if the user opens the
    ;; dialog, makes modifications than hits cancel.
    (define restore-data #f)

    ;; the parameters for the chart, a struct derived from basic-params
    (define params #f)

    ;; Create a new settings dialog for this class.  This needs to be
    ;; overriden.
    (define/public (make-settings-dialog)
      #f)

    ;; Indicate that the chart params have been changed, and the data for the
    ;; chart will need to be updated.  This needs to be overriden.
    (define/public (invalidate-data)
      #f)

    ;; Construct a new plot snip for the chart and insert it into CANVAS.  A
    ;; new plot snip needs to be constructed every time even if the data for
    ;; the plot has not changed.  This needs to be overriden.
    (define/public (put-plot-snip canvas)
      #f)
    
    (define/public (get-settings-dialog)
      (unless settings-dialog (set! settings-dialog (make-settings-dialog)))
      settings-dialog)

    (define/public (get-params)
      (unless params
        (set! params (send (get-settings-dialog) get-settings)))
      params)

    (define/public (get-name)
      (let ((p (get-params)))
        (if p (basic-params-name p) "XXX (no params)")))
    
    (define/public (get-title)
      (let ((p (get-params)))
        (if p (basic-params-title p) "No Title (no params)")))

    ;; Open a settings dialog (with PARENT as the parent window) and update
    ;; any chart settings. Returns #t if settings were updated, #f otherwise.
    (define/public (interactive-setup parent)
      (cond ((send (get-settings-dialog) show-dialog parent)
             => (lambda (dialog-result)
                  (set! params dialog-result)
                  (set! restore-data (send (get-settings-dialog) get-restore-data))
                  (invalidate-data)
                  #t))
            (#t
             ;; Dialog has been canceled, restore previous settings so they
             ;; show up correctly next time the dialog is opened.
             (when restore-data
               (send (get-settings-dialog) restore-from restore-data))
             #f)))

    (define/public (get-restore-data)
      restore-data)

    (define/public (restore-from data)
      (set! params #f)
      (set! restore-data data)
      (send (get-settings-dialog) restore-from data)
      (invalidate-data))

    (define/public (refresh)
      (set! params #f))

    ))


;;................................................ basic-chart-settings% ....

(struct basic-params1 basic-params (start-date end-date group-by))

(define basic-chart-settings%
  (class al-edit-dialog%

    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    
    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10]
               [tablet-friendly? #t])

    (define name-field #f)
    (define title-field #f)
    (define date-range-selector #f)
    (define group-by-choice #f)

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! name-field (new text-field% [parent p0] [label "Name "]))
        (send name-field set-value default-name))
      
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! title-field (new text-field% [parent p0] [label "Title "]))
        (send title-field set-value default-title))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-range-selector (new date-range-selector%
                                       [parent p0]
                                       )))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! group-by-choice (new choice% [parent p0]
                                   [label "Group By "]
                                   [choices '("Week" "Month" "Year")]))))

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send group-by-choice get-selection)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send group-by-choice set-selection d3))
       
    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))
    
    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (basic-params1
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send group-by-choice get-selection)))
            #f)))
    
    ))


;;.................................................. vol-chart-settings% ....

;; Metric: 0 - time, 1 - distance, 2 - session count, 3 - tss
(struct vol-params basic-params (start-date end-date group-by sport sub-sport metric))

(define vol-chart-settings%
  (class al-edit-dialog%

    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    
    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10]
               [tablet-friendly? #t])

    (define name-field #f)
    (define title-field #f)
    (define date-range-selector #f)
    (define sport-selector #f)
    (define group-by-choice #f)
    (define metric-choice #f)

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! name-field (new text-field% [parent p0] [label "Name "]))
        (send name-field set-value default-name))
      
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! title-field (new text-field% [parent p0] [label "Title "]))
        (send title-field set-value default-title))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-range-selector (new date-range-selector%
                                       [parent p0]
                                       )))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! sport-selector
              (new sport-selector% [parent p0]
                   [sports-in-use-only? #t])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! group-by-choice (new choice% [parent p0]
                                   [label "Group By "]
                                   [choices '("Week" "Month" "Year")])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! metric-choice (new choice% [parent p0]
                                 [label "Metric "]
                                 [choices '("Time" "Distance" "Number of sessions" "Training Stress")])))

      )

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send group-by-choice get-selection)
       (send metric-choice get-selection)
       (send sport-selector get-selection)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3 d4 d5) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send group-by-choice set-selection d3)
      (send metric-choice set-selection d4)
      (send sport-selector set-selected-sport (car d5) (cdr d5)))
       
    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))
    
    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection))
            (sport (send sport-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (vol-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send group-by-choice get-selection)
               (car sport)
               (cdr sport)
               (send metric-choice get-selection)))
            #f)))
    
    ))


;;................................................ trivol-chart-settings% ....

;; Group-by: 0 - week, 1 - month, 2 - year
;; Metric: 0 - time, 1 - distance, 2 - session count
(struct trivol-params basic-params (start-date end-date group-by metric))

(define trivol-chart-settings%
  (class al-edit-dialog%

    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    
    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10]
               [tablet-friendly? #t])

    (define name-field #f)
    (define title-field #f)
    (define date-range-selector #f)
    (define group-by-choice #f)
    (define metric-choice #f)

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! name-field (new text-field% [parent p0] [label "Name "]))
        (send name-field set-value default-name))
      
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! title-field (new text-field% [parent p0] [label "Title "]))
        (send title-field set-value default-title))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-range-selector (new date-range-selector%
                                       [parent p0]
                                       )))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! group-by-choice (new choice% [parent p0]
                                   [label "Group By "]
                                   [choices '("Week" "Month" "Year")])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! metric-choice (new choice% [parent p0]
                                 [label "Metric "]
                                 [choices '("Time" "Distance" "Number of sessions")])))

      )

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send group-by-choice get-selection)
       (send metric-choice get-selection)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3 d4) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send group-by-choice set-selection d3)
      (send metric-choice set-selection d4))
       
    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))
    
    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (trivol-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send group-by-choice get-selection)
               (send metric-choice get-selection)))
            #f)))
    
    ))


;;..................................................... vol-trend-chart% ....

(define (vol-trends-make-sql-query start-date end-date group-by sport sub-sport)
  (format "select ~a as period, 
           total(VAL.duration) / 3600.0 as duration,
           total(VAL.distance) / 1000.0 as distance,
           count(VAL.session_id) as session_count,
           total(VAL.tss) as training_stress
           from V_ACTIVITY_LIST VAL
           where VAL.start_time between ~a and ~a 
             and ~a
           group by period order by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(VAL.start_time, 'unixepoch', 'localtime', '+1 days', 'weekday 1', '-7 days')")
                ((eqv? group-by 1)       ; month
                 "date(VAL.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(VAL.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))
          start-date
          end-date
          (if sport
              (cond (sub-sport
                     (format "VAL.sub_sport = ~a" sub-sport))
                    (sport
                     (format "VAL.sport = ~a" sport))
                    (#t
                     "1 = 1"))
              "1 = 1")))

(define (vol-trends-get-data db sql-query metric)
  (for/list (([period duration distance count tss] (in-query db sql-query)))
    (vector period
            (case metric
              ((0) duration)
              ((1) distance)
              ((2) count)
              ((3) tss)))))

(define (vol-trends-plot canvas data y-label)
  (define max-y 0)
  (define pdata
    (for/list ([row data]
               [n (in-range (length data))])
      (if (> (vector-length row) 1)
          (let ((dummy #f))
            (match-define (vector timestamp val) row)
            (set! max-y (max max-y val))
            (vector timestamp val))
          (vector "" 0))))
  (set! max-y (* 1.2 max-y)) ;; make it larger to fit the legend
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-x-label #f]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 30]
                 [plot-y-label y-label])
    (plot-snip/hack
     canvas
     (list (y-tick-lines)
           (discrete-histogram
            pdata
            #:y-max max-y
            #:color *sea-green*
            #:line-width 0
            #:gap 0.5)))))

(define vol-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define sql-query #f)
    (define sql-query-result #f)
    (define chart-data #f)

    (define/override (make-settings-dialog)
      (new vol-chart-settings%
           [default-name "Vol"]
           [default-title "Training Volume"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (and data-valid?
           (let* ((metric (vol-params-metric (send this get-params)))
                  (y-label (case metric
                             ((0) "Time") ((1) "Distance")
                             ((2) "Session Count") ((3) "Trainning Stress"))))
             (vol-trends-plot canvas chart-data y-label))))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let* ((start (vol-params-start-date params))
                   (end (vol-params-end-date params))
                   (group-by (vol-params-group-by params))
                   (metric (vol-params-metric params))
                   (sport (vol-params-sport params))
                   (sub-sport (vol-params-sub-sport params))
                   (timestamps (generate-timestamps start end group-by)))
              (set! sql-query
                    (vol-trends-make-sql-query start end group-by sport sub-sport))
              (set! sql-query-result (vol-trends-get-data database sql-query metric))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! data-valid? #t)))))))
    
    ))


;;................................................. trivol-trends-chart% ....

(define (trivol-trends-make-sql-query start-date end-date group-by)
  (format "select ~a as period, 
           total(T.strength_time) / 3600.0 as strength_time,
           total(T.swim_time) / 3600.0 as swim_time,
           total(T.bike_time) / 3600.0 as bike_time,
           total(T.run_time) / 3600.0 as run_time
           from V_TRIATHLON_SESSIONS T
           where T.start_time between ~a and ~a group by period order by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(T.start_time, 'unixepoch', 'localtime', '+1 days', 'weekday 1', '-7 days')")
                ((eqv? group-by 1)       ; month
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))
          start-date
          end-date))

(define (tricnt-trends-make-sql-query start-date end-date group-by)
  (format "select ~a as period, 
           total(T.strength_count),
           total(T.swim_count),
           total(T.bike_count),
           total(T.run_count)
           from V_TRIATHLON_SESSIONS T
           where T.start_time between ~a and ~a group by period order by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(T.start_time, 'unixepoch', 'localtime', '+1 days', 'weekday 1', '-7 days')")
                ((eqv? group-by 1)       ; month
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))
          start-date
          end-date))

(define (tridist-trends-make-sql-query start-date end-date group-by)
  (format "select ~a as period, 
           0 as strength_distance,
           total(T.swim_distance) / 1000.0,
           total(T.bike_distance) / 1000.0,
           total(T.run_distance) / 1000.0
           from V_TRIATHLON_SESSIONS T
           where T.start_time between ~a and ~a group by period order by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(T.start_time, 'unixepoch', 'localtime', '+1 days', 'weekday 1', '-7 days')")
                ((eqv? group-by 1)       ; month
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(T.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))
          start-date
          end-date))

(define (trivol-trends-get-data db sql-query)
  (query-rows db sql-query))

(define (trivol-trends-plot canvas data y-label)
  (define max-y 0)
  (define pdata
    (for/list ([row data]
               [n (in-range (length data))])
      (if (> (vector-length row) 1)
          (let ((dummy #f))
            (match-define (vector timestamp wtime stime btime rtime) row)
            (set! max-y (max max-y (+ wtime stime btime rtime)))
            (list timestamp ;(if (eqv? (remainder n 12) 0) timestamp "")
                  (vector wtime stime btime rtime)))
          (list "" (vector)))))
  (set! max-y (* 1.2 max-y)) ;; make it larger to fit the legend
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-x-label #f]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 30]
                 [plot-y-label y-label])
    (plot-snip/hack
     canvas
     (list (y-tick-lines)
           (stacked-histogram 
            pdata
            #:y-max max-y
            #:colors 
            (list (get-sport-color 4 20)
                  (get-sport-color 5 #f)
                  (get-sport-color 2 #f)
                  (get-sport-color 1 #f))
            #:labels '("Weights" "Swim" "Bike" "Run")
            #:line-widths '(0 0 0 0)
            #:gap 0.5)))))

(define trivol-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define sql-query #f)
    (define sql-query-result #f)
    (define chart-data #f)

    (define/override (make-settings-dialog)
      (new trivol-chart-settings%
           [default-name "TriVol"]
           [default-title "Multisport Training Volume"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (and data-valid?
           (let* ((metric (trivol-params-metric (send this get-params)))
                  (y-label (case metric
                             ((0) "Time") ((1) "Distance") ((2) "Session Count"))))
             (trivol-trends-plot canvas chart-data y-label))))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let* ((start (trivol-params-start-date params))
                   (end (trivol-params-end-date params))
                   (group-by (trivol-params-group-by params))
                   (metric (trivol-params-metric params))
                   (timestamps (generate-timestamps start end group-by)))
              (set! sql-query
                    (case metric
                      ((0) (trivol-trends-make-sql-query start end group-by))
                      ((1) (tridist-trends-make-sql-query start end group-by))
                      ((2) (tricnt-trends-make-sql-query start end group-by))))
              (set! sql-query-result (trivol-trends-get-data database sql-query))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! data-valid? #t)))))))
    
    ))


;;............................................. bodyweight-trends-chart% ....

(define (bw-trends-make-sql-query)
  "select timestamp, body_weight as bw from ATHLETE_METRICS where timestamp between ? and ?")

(define (bw-trends-get-data db sql-query start-date end-date group-by)
  (let* ((filter-width (* 24 60 60 (case group-by ((0) 7) ((1) 30) ((2) 365))))
         (filter (make-low-pass-filter filter-width #f)))
    (for/list (([timestamp bw] (in-query db sql-query start-date end-date)))
      (filter (vector timestamp bw)))))

(define *sea-green* '(#x2e #x8b #x57))

(define (bw-trends-plot canvas data)
  (parameterize ([plot-x-ticks (pmc-date-ticks)]
                 [plot-x-label #f]
                 [plot-y-label "Bodyweight"])
    (plot-snip/hack canvas (list (tick-grid) (lines data #:color *sea-green* #:width 3.0)))))

(define bodyweight-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define sql-query #f)
    (define sql-query-result #f)
    
    (define/override (make-settings-dialog)
      (new basic-chart-settings%
           [default-name "BodyWeight"]
           [default-title "Body Weight"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (and data-valid? (bw-trends-plot canvas sql-query-result)))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let* ((start (basic-params1-start-date params))
                   (end (basic-params1-end-date params))
                   (group-by (basic-params1-group-by params)))
              (set! sql-query (bw-trends-make-sql-query))
              (set! sql-query-result (bw-trends-get-data database sql-query start end group-by))
              (set! data-valid? (> (length sql-query-result) 0)))))))

    ))


;;.................................................... tiz-trends-chart% ....

(struct tiz-params basic-params (start-date end-date group-by sport zone-metric))

(define tiz-chart-settings%
  (class al-edit-dialog%

    (init-field database [default-name "TIZ"] [default-title "Time in Zone"])
    (super-new [title "Chart Settings"]
               [icon edit-icon] [min-height 10] [tablet-friendly? #t])

    (define name-field #f)
    (define title-field #f)
    (define date-range-selector #f)
    (define group-by-choice #f)
    (define sport-choice #f)
    (define zone-metric-choice #f)

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! name-field (new text-field% [parent p0] [label "Name "]))
        (send name-field set-value default-name))
      
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! title-field (new text-field% [parent p0] [label "Title "]))
        (send title-field set-value default-title))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! sport-choice (new choice% [parent p0]
                                [label "Sport "]
                                [choices '("Running" "Cycling")])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! zone-metric-choice (new choice% [parent p0]
                                      [label "Zone "]
                                      [choices '("Heart Rate" "Power")])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-range-selector (new date-range-selector% [parent p0])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! group-by-choice (new choice% [parent p0]
                                   [label "Group By "]
                                   [choices '("Week" "Month" "Year")]))))

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send group-by-choice get-selection)
       (send sport-choice get-selection)
       (send zone-metric-choice get-selection)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3 d4 d5) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send group-by-choice set-selection d3)
      (send sport-choice set-selection d4)
      (send zone-metric-choice set-selection d5))
       
    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))
    
    (define/public (set-seasons seasons)
      (send date-range-selector set-seasons seasons))

    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (tiz-params
               (send name-field get-value)
               (send title-field get-value)
               start-date end-date
               (send group-by-choice get-selection)
               (case (send sport-choice get-selection) ; TODO: not nice, use data from database
                 ((0) 1)                               ; Running
                 ((1) 2))                              ; Cycling
               (case (send zone-metric-choice get-selection)
                 ((0) 1)                  ; Heart rate
                 ((1) 3))                 ; Power
               ))
            #f)))
    
    ))

(define (tiz-trends-make-sql-query group-by)
  (format "select ~a as period,
       total(case TIZ.zone_id when 0 then TIZ.duration else 0 end) / 3600.0 as z0_duration,
       total(case TIZ.zone_id when 1 then TIZ.duration else 0 end) / 3600.0 as z1_duration,
       total(case TIZ.zone_id when 2 then TIZ.duration else 0 end) / 3600.0 as z2_duration,
       total(case TIZ.zone_id when 3 then TIZ.duration else 0 end) / 3600.0 as z3_duration,
       total(case TIZ.zone_id when 4 then TIZ.duration else 0 end) / 3600.0 as z4_duration,
       total(case TIZ.zone_id when 5 then TIZ.duration else 0 end) / 3600.0 as z5_duration,
       total(case TIZ.zone_id when 6 then TIZ.duration else 0 end) / 3600.0 as z6_duration,
       total(case TIZ.zone_id when 7 then TIZ.duration else 0 end) / 3600.0 as z7_duration,
       total(case TIZ.zone_id when 8 then TIZ.duration else 0 end) / 3600.0 as z8_duration,
       total(case TIZ.zone_id when 9 then TIZ.duration else 0 end) / 3600.0 as z9_duration,
       total(case TIZ.zone_id when 10 then TIZ.duration else 0 end) / 3600.0 as z10_duration
  from A_SESSION S, TIME_IN_ZONE TIZ, SPORT_ZONE SZ
 where TIZ.session_id = S.id
   and TIZ.sport_zone_id = SZ.id
   and S.sport_id = ?
   and SZ.zone_metric_id = ?
   and S.start_time between ? and ?
 group by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(S.start_time, 'unixepoch', 'localtime', '+1 days', 'weekday 1', '-7 days')")
                ((eqv? group-by 1)       ; month
                 "date(S.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(S.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))))

(define (tiz-trends-get-data db sql-query sport zone-metric start end)
  (query-rows db sql-query sport zone-metric start end))

;; http://www.spycolor.com/w3c-colors
(define tiz-colors
  (list (make-object color% #xad #xd8 #xe6) ; z0, light blue
        (make-object color% #x00 #xbf #xff) ; z1, deep sky blue
        (make-object color% #x22 #x8b #x22) ; z2, forrest green
        (make-object color% #xff #x7f #x50) ; z3, coral
        (make-object color% #xcd #x5c #x5c) ; z4, indian red
        (make-object color% #xdc #x14 #x3c) ; z5, crimson
        (make-object color% #x8b #x00 #x00) ; z6, dark red
        (make-object color% #x99 #x32 #xcc) ; z7, dark orchid
        (make-object color% #x00 #x00 #x8b) ; z8, dark blue
        (make-object color% #xff #x8c #x00) ; z9, dark orange
        (make-object color% #xda #xa5 #x20) ; z10, golden rod
        ))

(define tiz-labels
  (list "z0" "z1" "z2" "z3" "z4" "z5" "z6" "z7" "z8" "z9" "z10"))
   
(define (tiz-trends-plot canvas data)

  (define (min-zone . zones)
    (let loop ((zones zones)
               (index 0))
      (cond ((null? zones) index)
            ((< (car zones) 0.001)
             (loop (cdr zones) (+ 1 index)))
            (#t index))))

  (define (max-zone . zones)
    (define zindex (apply min-zone (reverse zones)))
    (- (length zones) zindex))

  (define zmin 10)
  (define zmax 0)

  ;; Determine min and max zones
  (for ([row data])
    (when (> (vector-length row) 1)
      (let ((dummy #f))
        (match-define (vector timestamp z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10) row)
        (set! zmin (min zmin (min-zone z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10)))
        (set! zmax (max zmax (max-zone z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))))))

  (define plot-colors
    (drop (take tiz-colors zmax) zmin))

  (define plot-labels
    (drop (take tiz-labels zmax) zmin))

  (define (select-zones . zones)
    (drop (take zones zmax) zmin))
  
  (define max-y 0)
  (define pdata
    (for/list ([row data]
               [n (in-range (length data))])
      (if (> (vector-length row) 1)
          (let ((dummy #f))
            (match-define (vector timestamp z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10) row)
            (define zones (select-zones z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))
            (set! max-y (max max-y (foldl + 0 zones)))
            (list timestamp zones))
          (list "" (list)))))
  (set! max-y (* 1.2 max-y)) ;; make it larger to fit the legend
  
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-x-label #f]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 30]
                 [plot-y-label "Time in zone"])
    (plot-snip/hack
     canvas
     #:x-min 0
     #:x-max (length pdata)
     #:y-min 0
     #:y-max max-y
     (list (y-tick-lines)
           (stacked-histogram 
            pdata
            #:colors tiz-colors
            #:labels '("z0" "z1" "z2" "z3" "z4" "z5" "z6" "z7" "z8" "z9" "z10")
            #:line-widths '(0 0 0 0 0 0 0 0 0 0 )
            #:gap 0.5)))))

(define tiz-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define sql-query #f)
    (define sql-query-result #f)
    (define chart-data #f)
    
    (define/override (make-settings-dialog)
      (new tiz-chart-settings%
           [default-name "TIZ"]
           [default-title "Time in Zone"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (and data-valid? (tiz-trends-plot canvas chart-data)))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let* ((start (tiz-params-start-date params))
                   (end (tiz-params-end-date params))
                   (group-by (tiz-params-group-by params))
                   (sport (tiz-params-sport params))
                   (zone (tiz-params-zone-metric params))
                   (timestamps (generate-timestamps start end group-by)))
              (set! sql-query (tiz-trends-make-sql-query group-by))
              (set! sql-query-result (tiz-trends-get-data database sql-query sport zone start end))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! data-valid? #t)))))))
    
    ))


;;.................................................... pmc-trends-chart% ....

;; Number of day over which we average the daily TSS to obtain the "Chronic
;; Training Load" (fitness).  Default is 42 (6 weeks)
(define ctl-range 42)

;; Number of days over which we average the daily TSS to obtain the "Acute
;; Training Load" (fatigue).  Default is 7 days.
(define atl-range 7)

(struct pmc-params basic-params
  (start-date end-date show-form? show-fitness? show-fatigue? show-daily-tss?))

(define pmc-chart-settings%
  (class al-edit-dialog%

    (init-field database [default-name "Trends"] [default-title "Trends Chart"])
    (super-new [title "Chart Settings"]
               [icon edit-icon] [min-height 10] [tablet-friendly? #t])

    (define name-field #f)
    (define title-field #f)
    (define date-range-selector #f)
    (define show-form-check-box #t)
    (define show-fitness-check-box #t)
    (define show-fatigue-check-box #t)
    (define show-daily-tss-check-box #t)

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! name-field (new text-field% [parent p0] [label "Name "]))
        (send name-field set-value default-name))
      
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! title-field (new text-field% [parent p0] [label "Title "]))
        (send title-field set-value default-title))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-range-selector
              (new date-range-selector% [parent p0])))

      (let ((p (new vertical-pane% [parent p]
                    [stretchable-width #f] [border 20]
                    [alignment '(center center)])))
        (let ((q (new horizontal-pane% [parent p]
                      [alignment '(center center)])))
          (set! show-form-check-box
                (new check-box% [parent q] [label "Form"] [value #t]))
          (set! show-daily-tss-check-box
                (new check-box% [parent q] [label "Daily TSS"] [value #t])))
        (let ((q (new horizontal-pane% [parent p]
                      [alignment '(center center)])))
          (set! show-fitness-check-box
                (new check-box% [parent q] [label "Fitness"] [value #t]))
          (set! show-fatigue-check-box
                (new check-box% [parent q] [label "Fatigue"] [value #t])))))

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send show-form-check-box get-value)
       (send show-fitness-check-box get-value)
       (send show-fatigue-check-box get-value)
       (send show-daily-tss-check-box get-value)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list d0 d1 d2 d3 d4 d5 d6) data)
      (send name-field set-value d0)
      (send title-field set-value d1)
      (send date-range-selector restore-from d2)
      (send show-form-check-box set-value d3)
      (send show-fitness-check-box set-value d4)
      (send show-fatigue-check-box set-value d5)
      (send show-daily-tss-check-box set-value d6))
       
    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (if (send this do-edit parent)
          (get-settings)
          #f))
    
    (define/public (set-seasons seasons)
      (send date-range-selector set-seasons seasons))

    (define/public (get-settings)
      (let ((dr (send date-range-selector get-selection)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (when (eqv? start-date 0)
                (set! start-date (get-true-min-start-date database)))
              (pmc-params
               (send name-field get-value)
               (send title-field get-value)
               start-date end-date
               (send show-form-check-box get-value)
               (send show-fitness-check-box get-value)
               (send show-fatigue-check-box get-value)
               (send show-daily-tss-check-box get-value)))
            #f)))
    
    ))

;; Return a list of TSS values between START and END and the timestamp when
;; they were "earned".  A list of (vector TIMESTAMP TSS) is returned.
(define (get-tss start end db)
  ;; NOTE: the TSS is earned at the end of an activtiy.
  (query-rows
   db
   "select round(VAL.start_time + VAL.duration) as timestamp, VAL.tss as tss
     from V_ACTIVITY_LIST VAL
     where VAL.start_time between ? and ?
       and VAL.tss > 0
   order by timestamp"
   start end))

(define day-in-seconds (* 24 3600))     ; number of seconds in a day

;; Generate a stream of TSS values between START and END timestamps.  TSS
;; values are generated every SKIP seconds.  TSS values are taken from SAMPLES
;; (as returned by `get-tss`, but extra 0 TSS entries are generated in-between
;; tss samples.  The resulting stream has a TSS value (possibly 0)
;; approximately every SKIP seconds.
(define (generate-tss-stream start end skip samples)
  
  (define (g crt samples)
    (cond ((> crt end) empty-stream)
          ((null? samples)
           (stream-cons (vector crt 0) (g (+ crt skip) samples)))
          (#t (let ((sample (vector-ref (car samples) 0)))
                (if (< sample crt)
                    (stream-cons (car samples)
                                 (g
                                  (if (< (- crt sample) day-in-seconds)
                                      (+ crt skip)
                                      crt)
                                  (cdr samples)))
                    (stream-cons (vector crt 0) (g (+ crt skip) samples)))))))

  (g start samples))

;; Produce PMC data, a list of (vector TIMESTAMP CTL ATL TSS), between START
;; and END dates, using SAMPLES as the TSS values.  SAMPLES is as returned by
;; `get-tss`.
;;
;; This method assumes that two TSS "earnings" in a day are not additive and
;; treats each one of them separately.  In general, it will produce lower ATL
;; and CTL values.
(define (produce-pmc-data-1 start end samples)
  
  (define search-start (start-of-day start))
  (define search-end (start-of-day (+ end day-in-seconds)))

  (define atl-filter (make-low-pass-filter (* atl-range day-in-seconds) #f))
  (define ctl-filter (make-low-pass-filter (* ctl-range day-in-seconds) #f))

  (for/list ([tss-point (generate-tss-stream search-start search-end day-in-seconds samples)])
    ;; NOTE: the filter functions return a (vector TIMESTAMP VALUE)
    (let ((ctl (ctl-filter tss-point))
          (atl (atl-filter tss-point)))
      (vector
       (vector-ref tss-point 0)         ; timestamp
       (vector-ref ctl 1)
       (vector-ref atl 1)
       (vector-ref tss-point 1)         ; TSS 
       ))))

(define (produce-pmc-data/method-1 start end db)
  (let ((samples (get-tss start end db)))
    (produce-pmc-data-1 start end samples)))

;; Return a hash table containing the daily TSS for each day between START and
;; END.
(define (get-daily-tss start end db)
  (define result (make-hash))
  (for (([date tss] 
         (in-query
          db
          "select date(S.start_time, 'unixepoch', 'localtime'),
                  sum(S.training_stress_score)
             from A_SESSION S
            where S.training_stress_score is not null 
              and S.start_time between ? and ?
         group by date(S.start_time, 'unixepoch', 'localtime')"
          start end)))
    
    (hash-set! result (str->date date) tss))
  result)

;; produce the performance data between START and END dates based on daily
;; TSS-DATA (as returned by `get-daily-tss'.  The PMC is a list of (vector DAY
;; CTL ATL DAY-TSS).
;;
;; This method assumes that TSS is additive in a day and will produce bigger
;; ATL / CTL values.
(define (produce-pmc-data-2 start end tss-data)
  
  (define search-start (start-of-day start))
  (define search-end (start-of-day (+ end (* 24 3600))))

  (define atl-filter (make-low-pass-filter (* atl-range day-in-seconds) #f))
  (define ctl-filter (make-low-pass-filter (* ctl-range day-in-seconds) #f))
  
  (let ((result '()))
    (let loop ((day search-start))
      (when (< day search-end)
        (let* ((tss (hash-ref tss-data day 0))
               (v (vector day tss))
               (ctl (vector-ref (ctl-filter v) 1))
               (atl (vector-ref (atl-filter v) 1)))
          (set! result (cons (vector day ctl atl tss) result)))
        (loop (+ day (* 24 3600)))))
    (reverse result)))

(define (produce-pmc-data/method-2 start end db)
  (let ((samples (get-daily-tss start end db)))
    (produce-pmc-data-2 start end samples)))

(define (get-fitness-data-series pmc-data)
  (for/list ((e (in-list pmc-data)))
    (vector (vector-ref e 0) (vector-ref e 1))))

(define (get-fatigue-data-series pmc-data)
  (for/list ((e (in-list pmc-data)))
    (vector (vector-ref e 0) (vector-ref e 2))))

(define (get-form-data-series pmc-data)
  (for/list ((e (in-list pmc-data)))
    (vector (vector-ref e 0) (- (vector-ref e 1) (vector-ref e 2)))))

(define (get-tss-data-series pmc-data)
  ;; NOTE: TSS data series does not contain zeroes
  (for/list ((e (in-list pmc-data)))
             ;; #:when (> (vector-ref e 3) 0))
    (vector (vector-ref e 0) (vector-ref e 3))))

(define (make-form-renderer data)
  (let ((fdata (get-form-data-series data))
        (zeroes (for/list ((e (in-list data)))
                  (vector (vector-ref e 0) 0))))
    (lines-interval 
     fdata zeroes
     #:color "blue" 
     #:line1-width 2.0
     #:line2-width 0
     #:alpha 0.2
     #:label "Form")))

(define (make-tss-renderer data)
  (let ((tdata (get-tss-data-series data)))
    (points tdata 
            #:color "black" 
            #:fill-color "purple"
            #:size 7
            #:line-width 1.5
            #:label "Training Stress")))

(define (make-fitness-renderer data)
  (let ((fdata (get-fitness-data-series data)))
    (lines fdata #:color *sea-green* #:width 3.0 #:label "Fitness")))

(define *dark-red* '(#x8b #x00 #x00))

(define (make-fatigue-renderer data)
  (let ((fdata (get-fatigue-data-series data)))
    (lines fdata #:color *dark-red* #:width 1.5 #:label "Fatigue")))

(define pmc-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define pmc-data #f)
    
    (define/override (make-settings-dialog)
      (new pmc-chart-settings%
           [default-name "PMC"]
           [default-title "Performance"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-build-pmc-data)
      (let ((params (send this get-params)))
        (if params
            (let ((rt (list (tick-grid))))
              (when (pmc-params-show-form? params)
                (let ((form-renderer (make-form-renderer pmc-data)))
                  (set! rt (cons form-renderer rt))))
          
              (when (pmc-params-show-fitness? params)
                (let ((fitness-renderer (make-fitness-renderer pmc-data)))
                  (set! rt (cons fitness-renderer rt))))
          
              (when (pmc-params-show-fatigue? params)
                (let ((fatigue-renderer (make-fatigue-renderer pmc-data)))
                  (set! rt (cons fatigue-renderer rt))))
          
              (when (pmc-params-show-daily-tss? params)
                (let ((daily-tss-renderer (make-tss-renderer pmc-data)))
                  (set! rt (cons daily-tss-renderer rt))))
          
              (parameterize ([plot-x-ticks (pmc-date-ticks)])
                (plot-snip/hack
                 canvas
                 #:x-min (pmc-params-start-date params)
                 #:x-label #f #:y-label #f rt)))
            #f)))

    (define (maybe-build-pmc-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            ;; NOTE: we extend the range so ATL CTL at the start of the range
            ;; is correctly computed (w/ exponential averaging, all past TSS
            ;; values have a contribution to the present)
            (let ((start (- (pmc-params-start-date params) (* 4 ctl-range 24 3600)))
                  (end (pmc-params-end-date params)))
              (set! pmc-data (produce-pmc-data/method-2 start end database)))
            (set! data-valid? #t)))))

    ))


;;......................................................... view-trends% ....

(struct chart-info (name tag class))

(define chart-types
  (list
   (chart-info "Body Weight" 'bw bodyweight-trends-chart%)
   (chart-info "Traning Volume (multisport)" 'trivol trivol-trends-chart%)
   (chart-info "Traning Volume" 'vol vol-trends-chart%)
   (chart-info "Time in Zone" 'tiz tiz-trends-chart%)
   (chart-info "Performance" 'pmc pmc-trends-chart%)))

(define new-trend-chart-dialog%
  (class al-edit-dialog%
    (super-new [title "New Chart"]
               [icon reports-icon]
               [save-button-name "Select"]
               [min-height 10] [tablet-friendly? #t])

    (define chart-choice #f)

    (let ((p (send this get-client-pane)))
      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! chart-choice (new choice%
                                [parent p0] [label "Chart Type "]
                                [choices (map chart-info-name chart-types)]))))

    (define/public (show-dialog parent)
      (if (send this do-edit parent)
          (list-ref chart-types (send chart-choice get-selection))
          #f))))

(define trend-chart-pane%
  (class panel%
    (init-field parent info-tag trend-chart)
    (super-new [parent parent] [style '(deleted)])

    (define first-activation? #t)
    (define graph-pb (new snip-canvas% [parent this]))

    (define/public (get-name)
      (send trend-chart get-name))

    (define/public (get-title)
      (send trend-chart get-title))

    (define/public (activate)
      (when first-activation?
        (refresh-chart)
        (set! first-activation? #f)))
    
    (define/public (refresh-chart)
      (send trend-chart invalidate-data)
      (send trend-chart put-plot-snip graph-pb))

    (define/public (interactive-setup parent)
      (when (send trend-chart interactive-setup parent)
        (refresh-chart)))

    (define/public (get-restore-data)
      (list info-tag (send trend-chart get-restore-data)))

    (define/public (export-image-to-file file)
      (send graph-pb export-image-to-file file))

    ))

(define view-trends%
  (class object%
    (init-field parent database)
    (super-new)

    (define tag 'activity-log:view-trends)

    (define pane 
      (new (class vertical-panel%
             (init)
             (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image))
             (define/public (interactive-export-data formatted?)
               (on-interactive-export-data formatted?)))
           [parent parent]
           [alignment '(left center)]))

    (define title-field #f)

    (let ((sel-pane (new horizontal-pane% [parent pane] 
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      
      (new message% [parent sel-pane] [label reports-icon])

      (let ([font (send the-font-list find-or-create-font 14 'default 'normal 'normal)])
        (set! title-field (new message% [parent sel-pane] [font font]
                               [label ""] [stretchable-width #t])))

      (new button% [parent sel-pane] [label "New..."]
           [callback (lambda (b e) (on-new-chart))])
      (new button% [parent sel-pane] [label "Delete..."]
           [callback (lambda (b e) (on-delete-chart))])
      (new button% [parent sel-pane] [label "Setup..."]
           [callback (lambda (b e) (on-setup-chart))])

      ;; This is a spacer
      (new message% [parent sel-pane] [label ""] [min-width 10] [stretchable-width #f])
      
      )

    (define trend-charts '())

    (define trend-charts-panel
      (new tab-panel%
           [stretchable-height #t]
           [choices '()]
           [callback (lambda (p c)
                       (switch-tabs (send p get-selection)))]
           [parent pane]))

    (define (restore-previous-charts)
      (let ((data (al-get-pref tag (lambda () #f))))

        (define (find-chart-info tag)
          (let loop ((chart-types chart-types))
            (if (null? chart-types)
                #f
                (if (eq? tag (chart-info-tag (car chart-types)))
                    (car chart-types)
                    (loop (cdr chart-types))))))
      
        (when data
          (for ([datum data])
            (match-define (list chart-tag restore-data) datum)
            (define ci (find-chart-info chart-tag))
            (when ci
              (let ((pane (let ((tc (new (chart-info-class ci) [database database])))
                            (send tc restore-from restore-data)
                            (new trend-chart-pane%
                                 [parent trend-charts-panel]
                                 [info-tag (chart-info-tag ci)]
                                 [trend-chart tc]))))
                (set! trend-charts (append trend-charts (list pane)))
                (send trend-charts-panel append (send pane get-name)))))
          (when (> (length data) 0)
            (switch-tabs 0)))))
          
    (define (on-new-chart)
      (let ((ct (send (new new-trend-chart-dialog%) show-dialog parent)))
        (when ct
          (let ((pane (let ((tc (new (chart-info-class ct) [database database])))
                        (new trend-chart-pane%
                             [parent trend-charts-panel]
                             [info-tag (chart-info-tag ct)]
                             [trend-chart tc]))))
            (when (send pane interactive-setup parent)
              (set! trend-charts (append trend-charts (list pane)))
              (send trend-charts-panel append (send pane get-name))
              (switch-tabs (- (length trend-charts) 1)))))))
    
    (define (on-delete-chart)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (let ((mresult (message-box/custom
                            "Confirm delete"
                            (format "Really delete chart \"~a\"?" (send c get-name))
                            #f "Delete" "Cancel"
                            (send parent get-top-level-window)
                            '(caution default=3))))
              (when (equal? mresult 2)
                (send trend-charts-panel delete n)
                (if (eqv? n 0)
                    (set! trend-charts (cdr trend-charts)) ; deletin first item
                    (let-values (([head tail] (split-at trend-charts n)))
                      (set! trend-charts (append head (cdr tail)))))
                (cond ((> (length trend-charts) n)
                       (switch-tabs n))
                      ((> (length trend-charts) 0)
                       (switch-tabs (- (length trend-charts) 1)))
                      (#t
                       (send trend-charts-panel change-children (lambda (old) (list)))))))))))
    
    (define (on-setup-chart)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (send c interactive-setup parent)
            (let ((nname (send c get-name))
                  (ntitle (send c get-title)))
              (send trend-charts-panel set-item-label n nname)
              (send title-field set-label ntitle))))))
    
    (define (switch-tabs n)
      (send trend-charts-panel set-selection n)
      (let ((chart (list-ref trend-charts n)))
        (send trend-charts-panel change-children
              (lambda (old) (list chart)))
        (send chart activate)
        (send title-field set-label (send chart get-title))))
    
    (define (on-interactive-export-image)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                                '(("PNG Files" "*.png") ("Any" "*.*")))))
            (when file
              (send (list-ref trend-charts n) export-image-to-file file))))))
    
    (define (on-interactive-export-data formatted?)
      #f)

    (define first-activation #t)

    (define/public (activated)
      (when first-activation
        (restore-previous-charts)
        (set! first-activation #f)))

    (define/public (refresh)
      (let ((n (send trend-charts-panel get-selection)))
        (when n
          (let ((c (list-ref trend-charts n)))
            (send c refresh-chart)))))

    (define/public (save-visual-layout)
      ;; Charts for this view are loaded on first activation (see `activate'),
      ;; if the view was not activated, don't save anything, otherwise we will
      ;; erase all our charts.
      (unless first-activation
        (let ((data (for/list ([tc trend-charts]) (send tc get-restore-data))))
          (al-put-pref tag data))))
    
    ))
