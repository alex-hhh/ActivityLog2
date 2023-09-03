#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; trend-pmc.rkt -- "Performance Management Chart"
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018, 2019, 2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         plot-container/hover-util
         plot/no-gui
         racket/class
         racket/format
         racket/gui/base
         racket/match
         racket/runtime-path
         "../al-widgets.rkt"
         "../database.rkt"
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(provide pmc-trends-chart%)

;; Calculate and return the "dots-per-pixel" values for the plot SNIP on the X
;; and Y axis -- this is the conversion factor between pixels and plot units
;; along the two plot axes.
;;
;; WARNING: this assumes that the plot uses linear scale without any plot
;; transforms!
;;
(define (compute-plot-dpp plot-snip)
  (match-define (vector (vector min-x max-x) (vector min-y max-y))
    (send plot-snip get-plot-bounds))
  ;; NOTE: plot Y axis grows from bottom to top, but DC grows (increses) from
  ;; top to bottom, so from MIN-Y we obtain DC-MAX-Y and vice-versa.
  (match-define (vector dc-min-x dc-max-y)
    (send plot-snip plot->dc (vector min-x min-y)))
  (match-define (vector dc-max-x dc-min-y)
    (send plot-snip plot->dc (vector max-x max-y)))
  (values
   (/ (- max-x min-x) (- dc-max-x dc-min-x))
   (/ (- max-y min-y) (- dc-max-y dc-min-y))))

(define pmc-chart-settings%
  (class edit-dialog-base%
    (init-field database [default-name "Trends"] [default-title "Trends Chart"])
    (super-new [title "Chart Settings"] [icon (edit-icon)] [min-height 10])

    (define-values
      (name-field
       title-field)
      (let ([gb (make-group-box-panel (send this get-client-pane))])
        (values
         (new text-field%
              [parent gb]
              [label "Name "]
              [init-value default-name])
         (new text-field%
              [parent gb]
              [label "Title "]
              [init-value default-title]))))

    (define date-range-selector
      (let ([gb (make-group-box-panel (send this get-client-pane))])
        (new date-range-selector% [parent gb])))

    (define labels-input
      (let ([gb (make-group-box-panel (send this get-client-pane))])
        (new message%
             [parent gb]
             [label "Mark Sessions With These Labels"])
        (new label-input-field% [parent gb])))

    (define-values
      (show-form-check-box
       show-daily-tss-check-box
       show-fitness-check-box
       show-fatigue-check-box)
      (let* ([gb (make-group-box-panel (send this get-client-pane))]
             [p (new grid-pane%
                     [parent gb]
                     [columns 2]
                     [alignment '(left center)]
                     [border 20])])
        (values
         (new check-box% [parent p] [label "Form"] [value #t])
         (new check-box% [parent p] [label "Daily TSS"] [value #t])
         (new check-box% [parent p] [label "Fitness"] [value #t])
         (new check-box% [parent p] [label "Fatigue"] [value #t]))))

    (define/public (get-chart-settings)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'timestamps
       (match-let ([(cons start end) (send date-range-selector get-selection)])
         (when (and (eqv? start 0) database)
           (set! start (get-true-min-start-date database)))
         (cons start end))
       'show-form? (send show-form-check-box get-value)
       'show-fitness? (send show-fitness-check-box get-value)
       'show-fatigue? (send show-fatigue-check-box get-value)
       'show-tss? (send show-daily-tss-check-box get-value)
       'marker-labels (send labels-input get-contents-as-tag-ids)))

    (define/public (put-chart-settings data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database))
        (send labels-input refresh-available-tags database))
      (send name-field set-value (hash-ref data 'name ""))
      (send title-field set-value (hash-ref data 'title ""))
      (let ((dr (hash-ref data 'date-range #f)))
        (when dr
          (send date-range-selector restore-from dr)))
      (send show-form-check-box set-value (hash-ref data 'show-form? #t))
      (send show-fitness-check-box set-value (hash-ref data 'show-fitness? #t))
      (send show-fatigue-check-box set-value (hash-ref data 'show-fatigue? #t))
      (send show-daily-tss-check-box set-value (hash-ref data 'show-tss? #f))
      (let ((labels (hash-ref data 'marker-labels '())))
        ;; NOTE: set the contents even if they are empty, as this sets the
        ;; available tags, allowing new ones to be added
        (send labels-input set-contents-from-tag-ids labels)))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database))
        (send labels-input refresh-available-tags database))
      (and (send this do-edit parent) (get-chart-settings)))

    ))


(define-runtime-path sql-pmc-data-path "../../sql/queries/pmc-data.sql")
(define sql-pmc-data (define-sql-statement sql-pmc-data-path))

(define-runtime-path sql-pmc-sessions-path "../../sql/queries/pmc-sessions.sql")
(define sql-pmc-sessions (define-sql-statement sql-pmc-sessions-path))

(define (read-pmc-data db start-timestamp end-timestamp)
  (define (->string timestamp)
    (let ([d (seconds->date timestamp)])
      (string-append
       (~a (date-year d) #:width 4 #:left-pad-string "0" #:align 'right)
       "-"
       (~a (date-month d) #:width 2 #:left-pad-string "0" #:align 'right)
       "-"
       (~a (date-day d) #:width 2 #:left-pad-string "0" #:align 'right))))
  (define start (->string start-timestamp))
  (define end (->string end-timestamp))
  (df-read/sql db (sql-pmc-data) start end))

(define (read-pmc-sessions db start-timestamp end-timestamp)
  (define df (df-read/sql db (sql-pmc-sessions) start-timestamp end-timestamp))
  (when (> (df-row-count df) 0)
    (df-set-sorted! df "day" string<?))
  df)

;; Number of day over which we average the daily TSS to obtain the "Chronic
;; Training Load" (fitness).  Default is 42 (6 weeks)
(define default-ctl-range 42)

;; Number of days over which we average the daily TSS to obtain the "Acute
;; Training Load" (fatigue).  Default is 7 days.
(define default-atl-range 7)

(define (prepare-pmc db start-timestamp end-timestamp
                     #:ctl-range [ctl-range default-ctl-range]
                     #:initial-ctl [initial-ctl 0]
                     #:atl-range [atl-range default-atl-range]
                     #:initial-atl [initial-atl 0])
  (define df (read-pmc-data db start-timestamp end-timestamp))

  ;; "timestamp" series records the entry at the start of the day (midnight)
  (df-set-sorted! df "timestamp" <)

  ;; "tsmidday" series is the midday timestamp (12 hours after "timestamp"),
  ;; we use this one to display entries on the plot -- since it looks much
  ;; nicer.  The PMC chart has one entry per day, and it looks much nicer if
  ;; the entry shows up in the middle of the day, even if the TSS was earned
  ;; earlier/later that day.
  (df-set-sorted! df "tsmidday" <)

  (let ([ctl initial-ctl])
    (df-add-derived!
     df
     "ctl"
     '("timestamp" "tss")
     (lambda (prev current)
       (if (and prev current)
           (match-let ([(list pt _ptss) prev]
                       [(list ct ctss) current])
             (define dt (/ (- ct pt) (* 24 3600)))
             (define alpha (/ dt (+ dt ctl-range)))
             (define new-ctl (+ (* alpha ctss) (* (- 1 alpha) ctl)))
             (set! ctl new-ctl)
             new-ctl)
           ctl))))

  (let ([atl initial-atl])
    (df-add-derived!
     df
     "atl"
     '("timestamp" "tss")
     (lambda (prev current)
       (if (and prev current)
           (match-let ([(list pt _ptss) prev]
                       [(list ct ctss) current])
             (define dt (/ (- ct pt) (* 24 3600)))
             (define alpha (/ dt (+ dt atl-range)))
             (define new-atl (+ (* alpha ctss) (* (- 1 alpha) atl)))
             (set! atl new-atl)
             new-atl)
           atl))))

  ;; Fitness for each day is calculated as the difference between ctl and atl
  ;; of the PREVIOUS DAY!
  (df-add-derived!
   df
   "form"
   '("ctl" "atl")
   (lambda (prev current)
     (if prev
         (match-let ([(list ctl atl) prev])
           (- ctl atl))
         0)))

  df)

(define (get-pmc-data-for-timestamp pmc-data timestamp)
  ;; We cannot use df-lookup here, as it would return the first entry with a
  ;; time stamp greater than TIMESTAMP, and we want the earlier one, since
  ;; timestamps record the start of the day.
  (define index (df-index-of pmc-data "timestamp" timestamp))
  (if (> index 0)                       ; no entry before the first one
      (df-ref* pmc-data (sub1 index) "timestamp" "day" "ctl" "atl" "form" "tss")
      #f))

(define (make-form-renderer data)
  (let* ((fdata (df-select* data "tsmidday" "form"))
         (zeroes (for/list ((e (in-vector fdata)))
                   (vector (vector-ref e 0) 0))))
    (lines-interval
     fdata zeroes
     #:color '(0 119 187)               ; blue
     #:line1-width 2.0
     #:line2-width 0
     #:alpha 0.2
     #:label "Form")))

;; Return true if the entry V have a TSS value.  We fill our data set with
;; zeroes for days with no activities.  We use this function to filter out
;; days with no TSS in `make-tss-renderer`.
(define (have-tss? v)
  (let ([tss (vector-ref v 1)])
    (and (real? tss) (> tss 0))))

(define (make-tss-renderer data)
  (points (df-select* data "tsmidday" "tss" #:filter have-tss?)
          #:color "black"
          #:fill-color '(51 187 238)  ; cyan
          #:size 7
          #:line-width 0.5
          #:sym 'fullcircle4
          #:label "Training Stress"))

(define (make-fitness-renderer data)
  (lines (df-select* data "tsmidday" "ctl")
         #:color '(0 153 136)         ; teal
         #:width 3.0
         #:label "Fitness"))

(define (make-fatigue-renderer data)
  (lines (df-select* data "tsmidday" "atl")
         #:color
         '(204 51 17)                 ; red
         #:width 1.5
         #:label "Fatigue"))

(define (make-renderer-tree params pmc-data session-markers)

  (define show-form? (hash-ref params 'show-form? #t))
  (define show-fitness? (hash-ref params 'show-fitness? #t))
  (define show-fatigue? (hash-ref params 'show-fatigue? #t))
  (define show-tss? (hash-ref params 'show-tss? #f))

  (define max-y
    (for/fold ([y 0])
              ([(ctl atl tss) (in-data-frame pmc-data "ctl" "atl" "tss")])
      (max y (if show-fitness? ctl 0) (if show-fatigue? atl 0) (if show-tss? tss 0))))

  (define renderer-tree
    (list (tick-grid)
          (make-session-marker-renderers
           session-markers
           #:y (+ max-y 20)
           #:color "dark orange")
          (and show-form? (make-form-renderer pmc-data))
          (and show-fitness? (make-fitness-renderer pmc-data))
          (and show-fatigue? (make-fatigue-renderer pmc-data))
          (and show-tss? (make-tss-renderer pmc-data))
          ;; Add a "today" vertical line to the plot
          (vrule (current-seconds))))

  ;; Remove #f values from the renderer tree -- for plots that we don't show.
  (filter values renderer-tree))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas params renderer-tree)
  (match-define (cons start-date end-date) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-to-canvas
      renderer-tree canvas
      ;; x-min is here because PMC data is retrieved further in the past to
      ;; start the "attenuation process", but we want the plot to start at the
      ;; date that the user specified.
      #:x-min start-date #:x-label #f #:y-label #f #:legend-anchor 'bottom-left))
   renderer-tree))

(define (save-plot-to-file file-name width height params renderer-tree)
  (match-define (cons start-date end-date) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-file
      renderer-tree
      file-name #:width width #:height height
      ;; x-min is here because PMC data is retrieved further in the past to
      ;; start the "attenuation process", but we want the plot to start at the
      ;; date that the user specified.
      #:x-min start-date #:x-label #f #:y-label #f
      #:legend-anchor 'bottom-left))
   renderer-tree))

(define pmc-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define *sea-green-hl* (make-object color% #x2e #x8b #x57 0.2))

    (define data-valid? #f)

    (define pmc-data #f)
    (define pmc-sessions #f)
    (define session-markers '())        ; see read-session-markers

    (define cached-day #f)
    (define cached-badge #f)
    (define cached-session-badges (make-hash))

    (define plot-scale-x 1)
    (define plot-scale-y 1)

    (define/override (make-settings-dialog)
      (new pmc-chart-settings%
           [default-name "PMC"]
           [default-title "Performance"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f)
      (set! cached-day #f)
      (set! cached-badge #f)
      (set! plot-scale-x 1)
      (set! plot-scale-y 1)
      (set! cached-session-badges (make-hash)))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)))

    (define/override (export-data-to-file file formatted?)
      (when pmc-data
        (call-with-output-file file
          (lambda (out) (df-write/csv pmc-data out))
          #:mode 'text
          #:exists 'truncate)))

    (define/private (get-session-badge ts day)
      (define b (hash-ref cached-session-badges day #f))
      (unless b
        (when pmc-sessions
          (let-values ([(start stop) (df-equal-range pmc-sessions "day" day)])
            (define sessions
              (for/fold ([result (list (list "" (calendar-date->string ts)))])
                        ([(h t sport sub-sport)
                          (in-data-frame pmc-sessions "headline" "tss" "sport" "sub_sport"
                                         #:start start #:stop stop)])
                (cons (list #f (get-sport-name sport sub-sport))
                      (cons (list (~r t #:precision 0) h)
                            result))))
            (set! b (make-hover-badge sessions))
            (hash-set! cached-session-badges day b))))
      b)

    (define/private (get-day-badge ts day ctl atl tsb tss params)
      (unless (eq? cached-day day)
        (define info
          (filter
           values                      ; remove any #f entries generated below
           (list (and (hash-ref params 'show-tss? #f)
                      (list "Stress" (~r tss #:precision 1)))
                 (and (hash-ref params 'show-fatigue? #t)
                      (list "Fatigue" (~r atl #:precision 1)))
                 (and (hash-ref params 'show-fitness? #t)
                      (list "Fitness" (~r ctl #:precision 1)))
                 (and (hash-ref params 'show-form? #t)
                      (list "Form" (~r tsb #:precision 1)))
                 (list "Date" (calendar-date->string ts)))))
        (set! cached-day day)
        (set! cached-badge (make-hover-badge info)))
      cached-badge)

    (define (plot-hover-callback snip event x y)
      (define renderers
        (if (good-hover? snip x y event)
            (let ([entry (get-pmc-data-for-timestamp pmc-data x)])
              (if entry
                  (match-let ([(vector ts day ctl atl tsb tss) entry])
                    (define params (send this get-chart-settings))
                    (if (and (> tss 0)  ; only when there's some activity
                             (hash-ref params 'show-tss? #f)
                             (< (/ (abs (- ts x)) plot-scale-x) 10)
                             (< (/ (abs (- tss y)) plot-scale-y) 10))
                        ;; Mouse cursor is close to one of the TSS points, and
                        ;; they are shown on the plot.  Display the session(s)
                        ;; associated with that TSS point
                        (list
                         (points
                          (list (vector ts tss))
                          #:color "black"
                          #:fill-color '(238 51 119)  ; magenta
                          #:size 14
                          #:line-width 0.5
                          #:sym 'fullcircle4)
                         (let ([b (get-session-badge ts day)])
                           (if b (hover-label x y b) null)))

                        ;; Otherwise, highlight the entire day -- while the
                        ;; plot is linear, values are only computed for an
                        ;; entire day.
                        (list
                          (hover-vrange ts (+ ts (* 24 3600)) *sea-green-hl*)
                          (let ([b (get-day-badge ts day ctl atl tsb tss params)])
                            (if b (hover-label x y b) null)))))
                  ;; No entry
                  null))
            ;; Not a good hover
            null))
        (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-build-pmc-data)
      (let ((params (send this get-chart-settings)))
        (if params
            (let* ([rt (make-renderer-tree params pmc-data session-markers)]
                   [snip (insert-plot-snip canvas params rt)])
              (set-mouse-event-callback snip plot-hover-callback)
              (define-values (sx sy) (compute-plot-dpp snip))
              (set! plot-scale-x sx)
              (set! plot-scale-y sy))
            #f)))

    (define/override (save-plot-image file-name width height)
      (when data-valid?
        (let ((params (send this get-chart-settings)))
          (when params
            (let ((rt (make-renderer-tree params pmc-data session-markers)))
              (save-plot-to-file file-name width height params rt))))))

    (define/private (maybe-build-pmc-data)
      (unless data-valid?
        (let ((params (send this get-chart-settings)))
          (when params
            (match-define
              (cons start-date end-date)
              (hash-ref params 'timestamps))
            ;; NOTE: we extend the range so ATL CTL at the start of the range
            ;; is correctly computed (w/ exponential averaging, all past TSS
            ;; values have a contribution to the present)
            (let ((start (max 0 (- start-date (* 4 default-ctl-range 24 3600))))
                  (end end-date))
              (set! pmc-data (prepare-pmc database start end))
              (set! pmc-sessions (read-pmc-sessions database start end)))
            (set! session-markers (read-session-markers database params))
            (set! data-valid? #t)))))

    ))
