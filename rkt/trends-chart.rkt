#lang racket/base
;; trends-chart.rkt -- common trend chart functionality
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require
 db
 plot
 racket/class
 racket/date
 racket/math
 racket/vector
 racket/contract)

(provide
 (struct-out tc-params)
 trends-chart%)

(provide/contract
 (get-true-min-start-date (-> connection? (or/c #f exact-nonnegative-integer?)))
 (make-low-pass-filter (-> positive? boolean? (-> vector? vector?)))
 (pmc-date-ticks (-> ticks?))
 (generate-timestamps (-> exact-nonnegative-integer? exact-nonnegative-integer? (or/c 0 1 2)
                          (listof exact-nonnegative-integer?)))
 (start-of-day (-> exact-nonnegative-integer? exact-nonnegative-integer?))
 (str->date (-> string? (or/c #f exact-nonnegative-integer?)))
 )

(provide
 pad-data
 simplify-labels)

;; Basic parameters for a chart, all chart parameters should derive from this
;; structure.  Individual charts will define additional parameters.
(struct tc-params (name title))

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

    ;; The parameters for the chart, a struct derived from tc-params
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
        (if p (tc-params-name p) "XXX (no params)")))
    
    (define/public (get-title)
      (let ((p (get-params)))
        (if p (tc-params-title p) "No Title (no params)")))

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
  (query-maybe-value db "select min(start_time) from A_SESSION"))


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

;; Create a low-pass filter of WIDTH.  The returned function will accept a
;; data point (a vector of [X Y]) and will return a new filtered datapoint [X
;; Y-filtered]).  The filter will keep state.
;;
;; When STOP-DETECTION? is #t, the function will attempt to detect stop points
;; (where dX is 0) and reset the filter.  This will produce nicer looking
;; graphs.  This works best if the data to be filtered was extracted with stop
;; detection (see `make-low-pass-filter')
;;
(define (make-low-pass-filter width stop-detection?)
  ;; NOTE: this function can be called from multiple threads and the value of
  ;; STATE is shared between subsequent calls to this function, so we make
  ;; sure it has a per-thread value.
  (let ((state (make-thread-cell #f)))
    (lambda (v)
      (if (eq? (thread-cell-ref state) #f)
          (begin (thread-cell-set! state v) v) ; first value

          ;; Start filtering
          (let* ((dt (- (vector-ref v 0) (vector-ref (thread-cell-ref state) 0)))
                 (alpha (/ dt (+ dt width)))
                 (new-v (vector (vector-ref v 0)
                                (+ (* alpha (vector-ref v 1))
                                   (* (- 1 alpha)
                                      (vector-ref (thread-cell-ref state) 1))))))
            ;; NOTE: stop detection introduces duplicate items (same X axis
            ;; value) to make graphs look nicer.  Don't break it, reset the
            ;; filter instead.
            (if (and stop-detection? (< dt 0.001))
                (let ((v (vector-copy v)))
                  (vector-set! v 1 0)
                  (thread-cell-set! state v)
                  v)
                (begin (thread-cell-set! state new-v) new-v)))))))

