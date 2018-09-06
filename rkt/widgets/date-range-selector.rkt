#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/gui/base racket/class racket/date racket/list racket/unit
         "date-input-field.rkt")
(provide date-range-selector%)


;;................................................... date-time-selector ....

;; Return the UNIX timestamp of the start of the current year (midnight 1 Jan)
(define (this-year-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 1 (date-year now)
           0 0 0 (date-time-zone-offset now)))))

;; Return the UNIX timestamp of the start of the previous year (midnight 1
;; Jan)
(define (last-year-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 1 (- (date-year now) 1)
           0 0 0 (date-time-zone-offset now)))))

;; Return the UNIX timestamp of the start of the current month
(define (this-month-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           1 (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

;; Return the UNIX timestamp of the start of the previous month
(define (last-month-start)
  (let* ((now (current-date))
         (month (date-month now))
         (year (date-year now)))
    (date->seconds
     (date 0 0 0
           1 (if (= month 1) 12 (- month 1)) (if (= month 1) (- year 1) year)
           0 0 0
           (date-time-zone-offset now)))))

;; Return the UNIX timestamp of the start of the current week (week starts on
;; Monday)
(define (this-week-start)
  (let* ((seconds (current-seconds))
         (now (seconds->date seconds))
         (week-day (date-week-day now))
         (delta (+ (* 24 3600 week-day)
                   (* 3600 (date-hour now))
                   (* 60 (date-minute now))
                   (date-second now))))
    ;; NOTE: calculation makes the week start on a Sunday, adjust for that.
    (+ (* 24 3600) (- seconds delta))))

;; Return the UNIX timestamp of the start of the previous week (week starts on
;; Monday)
(define (last-week-start)
  (- (this-week-start) (* 7 24 3600)))

;; Return the UNIX timestamp of the start of the current day
(define (this-day-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           (date-day now) (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

;; Return the UNIX timestamp for the start of a day, 7 days ago
(define (last-7-days-start)
  (- (this-day-start) (* 6 24 3600)))

;; Return the UNIX timestamp for the start of a day, 30 days ago
(define (last-30-days-start)
  (- (this-day-start) (* 29 24 3600)))

;; Return the UNIX timestamp for the start of a day, 6 weeks ago (42 days)
(define (last-6-weeks-start)
  (- (this-day-start) (* 41 24 3600)))

;; Return the UNIX timestamp for the start of a day, 6 months ago -- we
;; simplify this as 6 * 30 days
(define (last-6-months-start)
  (- (this-day-start) (* (* 30 6) 24 3600)))

;; Return the UNIX timestamp of the start of the day, 365 days ago.
(define (last-365-days-start)
  (- (this-day-start) (* 364 24 3600)))

;; Return the UNIX timestamp of the end of today
(define (this-day-end)
  (+ (this-day-start) (* 24 3600)))

;; Defines a time period, with a tag, name and functions to generate the start
;; and end UNIX timestamps for the periods
(struct tperiod (tag name start-ts end-ts))

;; List of time periods available in the date-range-selector%.  Note that we
;; have some special ones: 'current-dates and 'seasons -- these receive
;; special treatment in the date-range-selector%.
(define the-time-periods
  (list
   (tperiod 'all-days "all days" (lambda () 0) this-day-end)
   (tperiod 'custom-dates "custom dates" (lambda () 'custom-dates) this-day-end)
   (tperiod 'seasons "seasons" (lambda () 'seasons) this-day-end)
   (tperiod 'last-7-days "last 7 days" last-7-days-start this-day-end)
   (tperiod 'this-week "this week" this-week-start this-day-end)
   (tperiod 'last-week "last week" last-week-start this-week-start)
   (tperiod 'last-30-days "last 30 days" last-30-days-start this-day-end)
   (tperiod 'this-month "this month" this-month-start this-day-end)
   (tperiod 'last-month "last month" last-month-start this-month-start)
   (tperiod 'last-6-weeks "last 6 weeks" last-6-weeks-start this-day-end)
   (tperiod 'last-6-months "last 6 months" last-6-months-start this-day-end)
   (tperiod 'last-365-days "last 12 months" last-365-days-start this-day-end)
   (tperiod 'this-year "this year" this-year-start this-day-end)
   (tperiod 'last-year "last year" last-year-start this-year-start)))

;; Return the position in THE-TIME-PERIODS of the time period identified by
;; TAG.
(define (get-time-period-index tag)
  (for/first (((tp index) (in-indexed (in-list the-time-periods)))
              #:when (eq? (tperiod-tag tp) tag))
    index))

;; Return a date range, a (CONS START-TIMESTAMP END-TIMESTAMP) corresponding
;; to the time period at TIME-PERIOD-INDEX.  Note that START-TIMESTAMP might
;; not necessarily be a UNIX timestamp (see the 'custom-dates and 'seasons
;; time period definitions)
(define (get-date-range time-period-index)
      (let ((range (list-ref the-time-periods time-period-index)))
        (cons ((tperiod-start-ts range)) ((tperiod-end-ts range)))))

;; GUI widget to select a date range, which is a start-end UNIX timestamp.
;; The widget presents a drop-down box with predefined ranges (see
;; THE-TIME-PERIODS), allows entering custom start/end dates, or allows
;; selecting from a set of previously user defined date range, called seasons.
;;
;; The user can receive date range notifications by supplying a CALLBACK or
;; can retrieve the current date rage by calling GET-SELECTION.
(define date-range-selector%
  (class object%
    (init parent [initial-selection #f])
    (init-field [callback (lambda (x) #f)])
    (super-new)

    ;; Keep the last selected date range here, a (CONS start end).  When the
    ;; user switches to custom dates for the first time, we initialize the
    ;; custom date boxes with this date range.
    (define last-selected-date-range #f)

    ;; start/end timestamps selected by the custom date range fields
    (define start-date #f)
    (define end-date #f)

    ;; A list of user defined date ranges, each item is a (VECTOR name start
    ;; end) where START and END are UNIX timestamps.  See also 'set-seasons'
    ;; on how to set a list of seasons.
    (define seasons '())

    ;; Invoke the user defined callback with the newly selected DATE-RANGE,
    ;; and also save the value for later..
    (define (time-selection-callback date-range)
      (set! last-selected-date-range date-range)
      (when callback
        (callback date-range)))

    ;; Callback for the time-period-choice, it is invoked when a new time
    ;; period is selected.  Sets up the custom date widgets as necessary and
    ;; calls the user callback with the newly selected date range.
    (define (on-time-period-selected control event)
      (let* ((time-period-index (send control get-selection))
             (date-range (get-date-range time-period-index)))
        (cond ((eq? (car date-range) 'custom-dates)
               (select-custom-date-panel 'custom-dates last-selected-date-range)
               (when (and (number? start-date)
                          (number? end-date)
                          (> end-date start-date))
                 (time-selection-callback (cons start-date end-date))))
              ((eq? (car date-range) 'seasons)
               (select-custom-date-panel 'seasons)
               (on-season-selected (send season-choice get-selection)))
              (#t
               (select-custom-date-panel #f date-range)
               (time-selection-callback date-range)))))

    ;; Callback for the custom-range-start widget, invoked with a new, valid
    ;; start range.  Note that this can be 'empty as well.
    (define (on-valid-custom-start-date v)
      ;; If start date is 'empty, we assume the start of the UNIX epoch...
      (set! start-date (if (number? v) v 0))
      (let ((end (or end-date (+ (* 24 60 60) (current-seconds)))))
        (when (> end start-date)
          (time-selection-callback (cons start-date end)))))

    ;; Callback for the custom-range-end widget, invoked with a new, valid end
    ;; range.  Note that this can be 'empty as well.
    (define (on-valid-custom-end-date v)
      ;; Add 24 hours to end-date to make it inclusive.  If the end date is
      ;; 'empty, we assume "right now".
      (set! end-date (+ (* 24 60 60)
                        (if (number? v) v (current-seconds))))
      (let ((start (or start-date 0)))
        (when (> end-date start)
          (time-selection-callback (cons start end-date)))))

    ;; This is the pane% that holds the entire control
    (define pane (new vertical-pane%
                       [parent parent]
                       [border 0]
                       [spacing 5]
                       [vert-margin 0]
                       [horiz-margin 0]
                       [stretchable-height #f]
                       [stretchable-width #f]
                       [alignment '(left center)]))

    (define time-period-choice
      (new choice%
           [parent pane]
           [label "Time period "]
           [callback on-time-period-selected]
           [choices (map tperiod-name the-time-periods)]))

    ;; If an initial time period selection was specified, select it now.
    (when initial-selection
      (let ((index (get-time-period-index initial-selection)))
        (when index
          (send time-period-choice set-selection index))))

    ;; Panel that holds the custom start/end date input widgets
    (define custom-date-panel
      (new horizontal-panel% [parent pane] [stretchable-height #f] [spacing 0]))

    ;; By default, the custom-date-panel is not shown...
    (send custom-date-panel show #f)

    ;; Date input field for the start custom date (when 'custom-date is
    ;; selected in the time-period-choice)
    (define custom-range-start
      (new date-input-field%
           [valid-value-cb on-valid-custom-start-date]
           [parent custom-date-panel] [label ""]
           [style '(single)]
           [stretchable-width #f]
           [min-width 1]))

    ;; Date input field for the end custom date (when 'custom-date is selected
    ;; in the time-period-choice)
    (define custom-range-end
      (new date-input-field%
           [parent custom-date-panel]
           [label "--"]
           [valid-value-cb on-valid-custom-end-date]
           [style '(single)]
           [stretchable-width #f]
           [min-width 1]))

    ;; Choice box for the list of seasons (when 'seasons is selected in the
    ;; time-period-choice)
    (define season-choice
      (new choice%
           [parent custom-date-panel]
           [label ""]
           [stretchable-width #t]
           [choices '()]
           [callback (lambda (c event) (on-season-selected (send c get-selection)))]))

    ;; Select a custom date selection panel based on MODE.  If MODE is
    ;; 'custom-dates, the start/end date input fields are shown, if it is
    ;; 'seasons, the seasons selector is shown, otherwise the custom date
    ;; selection panel is hidden.
    ;;
    ;; CURRENT-DATE-RANGE is used to initialize the start/end date input
    ;; fields, if this is the first time they are shown.
    (define (select-custom-date-panel mode (current-date-range #f))
      (cond ((eq? mode 'custom-dates)
             (when current-date-range
               (unless (and start-date end-date)
                 ;; if the start/end custom dates are not set, set them now
                 ;; from whatever the previous selection was.
                 (set! start-date (car current-date-range))
                 (set! end-date (car current-date-range))
                 (send custom-range-start set-date-value start-date)
                 (send custom-range-end set-date-value end-date)))
             (send custom-date-panel change-children
                   (lambda (old) (list custom-range-start custom-range-end)))
             (send custom-date-panel show #t))
            ((eq? mode 'seasons)
             (send custom-date-panel change-children
                   (lambda (old) (list season-choice)))
             (send custom-date-panel show #t))
            (#t
             (send custom-date-panel show #f))))

    ;; Callback when a new season is selected (INDEX is the position in the
    ;; SEASONS list).
    (define (on-season-selected index)
      (unless (null? seasons)
        (let ((season (list-ref seasons index)))
          (time-selection-callback
           (cons (vector-ref season 1) (vector-ref season 2))))))

    ;; Return the current date range as a (CONS start-timestamp
    ;; end-timestamp), or #f if no valid date range is selected.
    (define/public (get-selection)
      (let ((dr (get-date-range (send time-period-choice get-selection))))
        (cond ((eq? (car dr) 'custom-dates)
               (if (and (send custom-range-start has-valid-value?)
                        (send custom-range-end has-valid-value?))
                   (let ((start (if (number? start-date) start-date 0))
                         (end (if (number? end-date) end-date (+ (* 24 60 60) (current-seconds)))))
                     (if (> end start)
                         (cons start end)
                         #f))
                   #f))
              ((eq? (car dr) 'seasons)
               (if (pair? seasons)
                   (let ((season (list-ref seasons (send season-choice get-selection))))
                     (cons (vector-ref season 1) (vector-ref season 2)))
                   #f))
              (#t
               dr))))

    ;; Set a new list of seasons and attempt to select the same season again.
    ;; This is needed since set-seasons will can be called repeatedly on
    ;; refresh calls on the GUI.
    ;;
    ;; We use two methods to restore the previous season: if we find a season
    ;; with the same name as the previously selected one, we use it, otherwise
    ;; we try to use the previous index position in the list (if at least that
    ;; many seasons are available.
    (define/public (set-seasons s)
      (let* ((index (send season-choice get-selection)) ; might be #f
             (name (if index (vector-ref (list-ref seasons index) 0) #f)))
        (set! seasons s)
        (send season-choice clear)
        (for ([(season n) (in-indexed (in-list seasons))])
          (let ((sname (vector-ref season 0)))
            (send season-choice append sname)
            (when (equal? name sname) (set! index n))))
        (when (and index (< index (length seasons)))
          (send season-choice set-selection index))))

    ;; Return the state of the date-range-selector% object.  This state can be
    ;; saved somewhere and can later be used to set this, or another
    ;; date-range-selector% to the same values by calling RESTORE-FROM.
    (define/public (get-restore-data)
      (let ((tp (list-ref the-time-periods (send time-period-choice get-selection)))
            (sn (and (pair? seasons) (send season-choice get-selection))))
        (list
         (tperiod-tag tp)
         (and sn (vector-ref (list-ref seasons sn) 0))
         (send custom-range-start get-converted-value)
         (send custom-range-end get-converted-value))))

    ;; Set the state of this date-range-selector% object from DATA.  DATA
    ;; should have been obtained from a GET-RESTORE-DATA call on this or
    ;; another date-range-selector% object.
    (define/public (restore-from data)
      (let ((tag (first data))
            (season (second data))
            (start (third data))
            (end (fourth data)))
        (when tag
          (let ((index (get-time-period-index tag)))
            (when index
              (send time-period-choice set-selection index)))
          (select-custom-date-panel tag))
        (when season
          (let loop ((s seasons) (index 0))
            (unless (null? s)
              (if (equal? season (vector-ref (car s) 0))
                  (send season-choice set-selection index)
                  (loop (cdr s) (+ 1 index))))))
        (when (number? start)
          (set! start-date start))
        (when (number? end)
          (set! end-date end))
        (send custom-range-start set-date-value start)
        (send custom-range-end set-date-value end)))

    ))

