#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; trend-pmc.rkt -- "Performance Management Chart"
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018-2019, 2021, 2023-2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         pict
         pict/snip
         plot-container/hover-util
         plot/no-gui
         racket/class
         racket/format
         racket/gui/base
         racket/match
         racket/math
         racket/runtime-path
         "../al-widgets.rkt"
         "../database.rkt"
         "../dbutil.rkt"
         "../fmt-util-ut.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(provide pmc-trends-chart%)

;; Number of day over which we average the daily TSS to obtain the "Chronic
;; Training Load" (fitness).  Default is 42 (6 weeks)
(define default-ctl-range 42)

;; Number of days over which we average the daily TSS to obtain the "Acute
;; Training Load" (fatigue).  Default is 7 days.
(define default-atl-range 7)

;; Number of weeks of past training we consider when calculating the ramp rate
;; for a specific day
(define default-ramp-rate-weeks 4)

;; Colors and line width of the plot elements
(define form-renderer-color '(0 119 187)) ; blue
(define form-renderer-line-width 1.0)
(define tss-renderer-color '(51 187 238)) ; cyan
(define tss-point-size 9)
(define fitness-renderer-color '(0 153 136)) ; teal
(define fitness-renderer-line-width 3.0)
(define fatigue-renderer-color '(204 51 17)) ; red
(define fatigue-renderer-line-width 1.5)

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
        (new date-range-selector%
             [parent gb]
             [this-day-end-as-seconds #f])))

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

(define (fetch-pmc-data db start-timestamp end-timestamp)
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

(define (fetch-pmc-sessions db start-timestamp end-timestamp)
  (define df (df-read/sql db (sql-pmc-sessions) start-timestamp end-timestamp))
  (when (> (df-row-count df) 0)
    (df-set-sorted! df "day" string<?))
  df)

(define (prepare-pmc db start-timestamp end-timestamp
                     #:ctl-range [ctl-range default-ctl-range]
                     #:initial-ctl [initial-ctl 0]
                     #:atl-range [atl-range default-atl-range]
                     #:initial-atl [initial-atl 0])
  (define df (fetch-pmc-data db start-timestamp end-timestamp))

  ;; "timestamp" series records the entry at the start of the day (midnight)
  (df-set-sorted! df "timestamp" <)

  ;; "tsmidday" series is the midday timestamp (12 hours after "timestamp"),
  ;; we use this one to display entries on the plot -- since it looks much
  ;; nicer.  The PMC chart has one entry per day, and it looks much nicer if
  ;; the entry shows up in the middle of the day, even if the TSS was earned
  ;; earlier/later that day.
  (df-set-sorted! df "tsmidday" <)

  ;; Simpler exponential decay (low pass filtering) when we know that the
  ;; samples in the data frame are equally spaced, every day in our case.
  ;; Otherwise, we would have to calculate alpha as 'dt / (dt + range)' for
  ;; each sample.
  (define ctl-alpha (- 1.0 (exp (/ -1.0 ctl-range))))
  (define atl-alpha (- 1.0 (exp (/ -1.0 atl-range))))

  (let ([ctl initial-ctl])
    (df-add-derived!
     df
     "ctl"
     '("timestamp" "tss")
     (lambda (current)
       (match-let ([(list _ct ctss) current])
         (define new-ctl (+ (* ctl-alpha ctss) (* (- 1.0 ctl-alpha) ctl)))
         (set! ctl new-ctl)
         new-ctl)
       ctl)))

  (let ([atl initial-atl])
    (df-add-derived!
     df
     "atl"
     '("timestamp" "tss")
     (lambda (current)
       (match-let ([(list _ct ctss) current])
         (define new-atl (+ (* atl-alpha ctss) (* (- 1.0 atl-alpha) atl)))
         (set! atl new-atl)
         new-atl)
       atl)))

  ;; Form for each day is calculated as the difference between ctl and atl of
  ;; the PREVIOUS DAY as the difference between CTL and ATL.
  (df-add-derived!
   df
   "form"
   '("ctl" "atl")
   (lambda (prev _current)
     (if prev
         (match-let ([(list ctl atl) prev])
           (- ctl atl))
         0)))

  df)

(define (get-pmc-data-for-timestamp pmc-data timestamp)
  ;; We cannot use `df-lookup` here, as it would return the first entry with a
  ;; time stamp greater than TIMESTAMP, and we want the earlier one, since
  ;; timestamps record the start of the day.
  (define index (sub1 (df-index-of pmc-data "timestamp" timestamp)))
  (if (>= index 0)                      ; no entry before the first one
      (let ([result (df-ref* pmc-data index "timestamp" "day" "ctl" "atl" "form" "tss")])
        (if (< (- timestamp (vector-ref result 0)) (* 24 3600))
            (values result index)
            ;; This entry is the last one in the data-frame, but timestamp is
            ;; more than a day in advance, so it is not a match...
            (values #f #f)))
      (values #f #f)))

;; Return the "ramping rate" for the date specified as TIMESTAMP. Ramping rate
;; is measured in CTL points/week and it is an estimate of how much change in
;; CTL happened in recent time.  We calculate "ramping rate" as the slope of a
;; linear fit over the "ctl"/"timestamp" series, between "TIMESTAMP" and
;; "DAYS-IN-PAST" number of days in the past.
;;
;; While TIMESTAMP is a unix timestamp, we consider only "day-level"
;; granularity since this is how the pmc-data is stored: one sample, or row,
;; for each day.
;;
(define (get-ramping-rate pmc-data timestamp [days-in-past 28])

  (define stop
    (let ([tindex (df-index-of pmc-data "timestamp" timestamp)])
      (cond ((= tindex 0)
             ;; timestamp is before the first row
             #f)
            ((and (= tindex (df-row-count pmc-data))
                  ;; timestamp is inside the 24-hour period of the last row
                  (< (- timestamp (df-ref pmc-data "timestamp" (sub1 tindex))) (* 24 3600)))
             tindex)
            (else
             ;; include "today" in the ramping rate calculations
             (add1 tindex)))))

  (and stop
       (let* ([start-timestamp (- timestamp (* days-in-past 24 3600))]
              [start (df-index-of pmc-data "timestamp" start-timestamp)]
              [lsf (and
                    ;; must have at least two entries to calculate slope
                    (> (- stop start) 2)
                    (df-least-squares-fit
                     pmc-data "timestamp" "ctl" #:mode 'linear
                     #:start start #:stop stop))]
              [slope (list-ref (least-squares-fit-coefficients lsf) 1)])
         ;; Convert the slope from CTL points/second to CTL points/week
         (* slope 7 24 3600))))

;; Prepare a pict representing the plot legend based on `pmc-data` -- this is
;; a more complex legend that what would be shown by the plot package: we show
;; not only the keys for the plot lines, but also the form, fitness and
;; fatigue, as well as the ramping rate for "today", if today's date is in
;; `pmc-data`
(define (pmc-plot-legend pmc-data)
  ;; TODO: copied from native-series.rkt, really need to normalize these
  ;; overlay colors, fonts and faces
  (define pd-background (make-object color% #xff #xf8 #xdc 0.95))
  (define pd-item-color (make-object color% #x2f #x4f #x4f))
  (define pd-label-color (make-object color% #x77 #x88 #x99))
  (define pd-title-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
  (define pd-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
  (define pd-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
  (define pd-title-face (cons pd-item-color pd-title-font))
  (define pd-item-face (cons pd-item-color pd-item-font))
  (define pd-label-face (cons pd-label-color pd-label-font))

  (define plot-legend/pict
    (let ([w 40]
          [h 10])
      (define entries
        (list (text "Form" pd-item-face) (colorize (filled-rectangle w h) form-renderer-color)
              (text "Fitness" pd-item-face) (colorize (linewidth fitness-renderer-line-width (hline w h)) fitness-renderer-color)
              (text "Fatigue" pd-item-face) (colorize (linewidth fatigue-renderer-line-width (hline w h)) fatigue-renderer-color)
              (text "Stress" pd-item-face)
              (cc-superimpose
               (ghost (rectangle w h))
               (colorize
                (disk tss-point-size
                      #:draw-border? #t
                      #:border-color "black")
                tss-renderer-color))))
      (table 2 entries lc-superimpose cc-superimpose 30 3)))

  (define today (current-seconds))

  (define title/pict
    (text (string-append "Today, " (calendar-date->string today)) pd-title-face))

  (define today-report/pict
    (let-values ([(entry _index) (get-pmc-data-for-timestamp pmc-data today)])
      (if entry
          (match-let ([(vector _ts _day ctl atl tsb _tss) entry])
            (let* ([rr-days (* default-ramp-rate-weeks 7)]
                   [rr (get-ramping-rate pmc-data today rr-days)]
                   [fmt (lambda (v) (if (real? v) (~r v #:precision '(= 1)) "N/A"))]
                   [entries
                    (list (text "Fitness" pd-label-face)
                          (text (fmt ctl) pd-item-face)
                          (text "CTL" pd-label-face)
                          (text "Fatigue" pd-label-face)
                          (text (fmt atl) pd-item-face)
                          (text "ATL" pd-label-face)
                          (text "Form" pd-label-face)
                          (text (fmt tsb) pd-item-face)
                          (text "TSB" pd-label-face)
                          (text "" pd-label-face)
                          (text (tsb-factor-label (tsb->label tsb)) pd-label-face)
                          (text "" pd-label-face)
                          (text "Ramping Rate" pd-label-face)
                          (text (fmt rr) pd-item-face)
                          (text "CTL points/week" pd-label-face)
                          (text "" pd-label-face)
                          (text "" pd-label-face)
                          (text (format "past ~a weeks" default-ramp-rate-weeks) pd-label-face))])
              (table 3 entries
                     (list lc-superimpose rc-superimpose lc-superimpose)
                     cc-superimpose 15 3)))
          (text "today's date not in the plot" pd-label-face))))

  (define p
    (vc-append
     10
     title/pict
     (colorize (hline (* (pict-width plot-legend/pict) 1.5) 1) pd-label-color)
     today-report/pict
     (colorize (hline (* (pict-width plot-legend/pict) 1.5) 1) pd-label-color)
     plot-legend/pict))
  (cc-superimpose
    (filled-rounded-rectangle
     (+ (pict-width p) 20) (+ (pict-height p) 20) -0.05
     #:draw-border? #t
     #:color pd-background)
    p))


;;........................................................ tsb-factoring ....

;; We colorize the FORM (Training Stress Balance) graph using guidelines from
;; https://www.trainingpeaks.com/coach-blog/a-coachs-guide-to-atl-ctl-tsb/

;; Key positions in the TSB balance when things "change" (see labels bellow)
(define tsb-key-points '(20 5 -10 -30))

;; Convert a TSB value into a label (a small positive number).  This is done
;; according to the tsb-key-points above (e.g. a TSB greater than the first
;; value in that list has a label, or index of 0
(define (tsb->label tsb)
  (or
   (for/first ([kp (in-list tsb-key-points)]
               [index (in-naturals)]
               #:when (> tsb kp))
     index)
   (length tsb-key-points)))

;; Give names to the TSB factor labels
(define tsb-factor-labels
  (vector "Transition"
          "Fresh"
          "Grey Zone"
          "Optimal"
          "High Risk"))

;; Return the label corresponding to the TSB label F
(define (tsb-factor-label f)
  (vector-ref tsb-factor-labels f))

;; Associate a color with each TSB factor label
#;(define tsb-factor-colors
  ;; Bright qualitative
  (vector '(204 187 68)                 ; Transition
          '(68 119 170)                 ; Fresh
          '(187 187 187)                ; Grey Zone
          '(34 136 51)                  ; Optimal
          '(238 102 119)))              ; High Risk

(define tsb-factor-colors
  ;; Vibrant Qualitative
  (vector '(238 119 51)                 ; Transition
          '(0 119 187)                  ; Fresh
          '(187 187 187)                ; Grey Zone
          '(0 153 136)                  ; Optimal
          '(204 51 17)))                ; High Risk

;; Return the color corresponding to the TSB label F
(define (tsb-factor-color f)
  (vector-ref tsb-factor-colors f))

;; Return the TSB value that represents the transition point between LABEL and
;; DIRECTION (another label).  E.g. the transition point between "Fresh" (1)
;; and "Transition" (0) is 20, the transition point between "Fresh" (1) and
;; Gray Zone (2) is 5
(define (tsb-transition-point label direction)
  (cond
    ((= label direction)
     (error "cannot transition to same label"))
    ((< label direction)
     (for/first ([kp (in-list tsb-key-points)]
                 [index (in-naturals)]
                 #:when (= index label))
       kp))
    (else ; (> label direction)
     (define slabel (sub1 label))
     (for/first ([kp (in-list tsb-key-points)]
                 [index (in-naturals)]
                 #:when (= index slabel))
       kp))))

;; Make a renderer for the FORM (Training Stress Balance) -- this is a
;; complicated one, since we split the "form" data series into segments
;; according to the TSB label and colorize them differently.
(define (make-form-renderer data start-index today-index)
  (for/list ([start (list start-index (sub1 today-index))]
             [stop (list today-index (df-row-count data))]
             [line-style '(solid short-dash)]
             [alpha '(1.0 0.5)])
    (for/fold ([current-data-set '()]
               [current-zeroes '()]
               [current-label #f]
               [renderers '()]
               #:result
               (if (null? current-data-set)
                   (reverse renderers)
                   (let ([renderer (lines-interval
                                    (reverse current-data-set) (reverse current-zeroes)
                                    #:color (tsb-factor-color current-label)
                                    #:line1-color (tsb-factor-color current-label)
                                    #:line1-width form-renderer-line-width
                                    #:line2-width 0
                                    #:line1-style line-style
                                    #:alpha alpha)])
                     (reverse (cons renderer renderers)))))
              ([(tsmidday form) (in-data-frame data "tsmidday" "form" #:start start #:stop stop)])
      (define label (tsb->label form))
      (cond
        ((equal? label current-label)
         (values (cons (vector tsmidday form) current-data-set)
                 (cons (vector tsmidday 0) current-zeroes)
                 current-label
                 renderers))
        ((null? current-data-set)
         (values
          (cons (vector tsmidday form) current-data-set)
          (cons (vector tsmidday 0) current-zeroes)
          label
          renderers))
        (else
         ;; LABEL and CURRENT-LABEL are different, but we need to find the
         ;; split point (possible multiple split points), so the graph is
         ;; colored correctly -- if we don't do this, segments where the label
         ;; changes will use the wrong color.  To see what the problem is, you
         ;; can replace the for/fold below with a simple (values
         ;; current-data-set current-zeroes)
         (for/fold ([cds current-data-set]
                    [cz current-zeroes]
                    [renderers renderers]
                    #:result (values cds cz label (reverse renderers)))
                   ([l (in-inclusive-range current-label label (if (> current-label label) -1 1))])
           (if (= l label)
               ;; last entry
               (let ([cds^ (cons (vector tsmidday form) cds)]
                     [cz^ (cons (vector tsmidday 0) cz)])
                 (values (list (car cds^))
                         (list (car cz^))
                         (cons (lines-interval
                                      (reverse cds^) (reverse cz^)
                                      #:color (tsb-factor-color l)
                                      #:line1-color (tsb-factor-color l)
                                      #:line1-width form-renderer-line-width
                                      #:line2-width 0
                                      #:line1-style line-style
                                      #:alpha alpha)
                                     renderers)))
               (let* ([tp (tsb-transition-point l label)]
                      [a (let ([prev-form (vector-ref (car cds) 1)])
                           (/ (- tp prev-form) (- form prev-form)))]
                      [tt
                       (let ([prev-ts (vector-ref (car cds) 0)])
                         (+ prev-ts (* a (- tsmidday prev-ts))))]
                      [cds^ (cons (vector tt tp) current-data-set)]
                      [cz^ (cons (vector tt 0) current-zeroes)])
                 (values (list (car cds^))
                         (list (car cz^))
                         (cons (lines-interval
                                (reverse cds^) (reverse cz^)
                                #:color (tsb-factor-color l)
                                #:line1-color (tsb-factor-color l)
                                #:line1-width form-renderer-line-width
                                #:line2-width 0
                                #:line1-style line-style
                                #:alpha alpha)
                               renderers))))))))))


;; Return true if the entry V have a TSS value.  We fill our data set with
;; zeroes for days with no activities.  We use this function to filter out
;; days with no TSS in `make-tss-renderer`.
(define (have-tss? v)
  (let ([tss (vector-ref v 1)])
    (and (real? tss) (> tss 0))))

(define (make-tss-renderer data start-index _today-index)
  ;; TODO: when we have planned workouts, we'll need to show them differently
  ;; (by splitting the data on the NOW index)
  (points (df-select* data "tsmidday" "tss" #:start start-index #:filter have-tss?)
          #:color "black"
          #:fill-color tss-renderer-color
          #:size tss-point-size
          #:line-width 0.5
          #:sym 'fullcircle4))

(define (make-fitness-renderer data start-index today-index)
  (list
   (lines (df-select* data "tsmidday" "ctl" #:start start-index #:stop today-index)
          #:color fitness-renderer-color
          #:width fitness-renderer-line-width)
   ;; This part represents future fitness, show it differently
   (lines (df-select* data "tsmidday" "ctl" #:start (sub1 today-index))
          #:color fitness-renderer-color
          #:width (* fitness-renderer-line-width 2/3)
          #:style 'short-dash)))

(define (make-fatigue-renderer data start-index today-index)
  (list
   (lines (df-select* data "tsmidday" "atl" #:start start-index #:stop today-index)
          #:color fatigue-renderer-color
          #:width fatigue-renderer-line-width)
   ;; This part represents future fatigue, show it differently
   (lines (df-select* data "tsmidday" "atl" #:start (sub1 today-index))
          #:color fatigue-renderer-color
          #:width (* fatigue-renderer-line-width 2/3)
          #:style 'short-dash)))

(define (make-renderer-tree params pmc-data session-markers)

  (define show-form? (hash-ref params 'show-form? #t))
  (define show-fitness? (hash-ref params 'show-fitness? #t))
  (define show-fatigue? (hash-ref params 'show-fatigue? #t))
  (define show-tss? (hash-ref params 'show-tss? #f))

  (match-define (cons start _end) (hash-ref params 'timestamps '(0 . 0)))

  (define start-index (sub1 (df-index-of pmc-data "timestamp" start)))
  (define today-index (df-index-of pmc-data "timestamp" (current-seconds)))

  (define max-y
    (for/fold ([y 0])
              ([(ctl atl tss) (in-data-frame pmc-data "ctl" "atl" "tss" #:start start-index)])
      (max y (if show-fitness? ctl 0) (if show-fatigue? atl 0) (if show-tss? tss 0))))

  (define renderer-tree
    (list (tick-grid)
          (make-session-marker-renderers
           session-markers
           #:y (+ max-y 25)
           #:color "dark orange")

          ;; Hack to make sure there is enough room at the top of the plot,
          ;; since the plot package calculates tight bounds by default, so the
          ;; highest entry is right on the border.  We place an invisible
          ;; point higher than the highest value, to make some room.
          (points (list (vector 0 (+ max-y 25))) #:sym 'none)

          (and show-form? (make-form-renderer pmc-data start-index today-index))
          (and show-fitness? (make-fitness-renderer pmc-data start-index today-index))
          (and show-fatigue? (make-fatigue-renderer pmc-data start-index today-index))
          (and show-tss? (make-tss-renderer pmc-data start-index today-index))))

  ;; Remove #f values from the renderer tree -- for plots that we don't show.
  (filter values renderer-tree))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas params renderer-tree)
  (match-define (cons start _end) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-to-canvas
      renderer-tree canvas
      ;; x-min is here because PMC data is retrieved further in the past to
      ;; start the "attenuation process", but we want the plot to start at the
      ;; date that the user specified.
      #:x-min start #:x-label #f #:y-label #f #:legend-anchor 'bottom-left))
   renderer-tree))

(define (save-plot-to-file file-name width height params renderer-tree)
  (match-define (cons start _end) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-file
      renderer-tree
      file-name #:width width #:height height
      ;; x-min is here because PMC data is retrieved further in the past to
      ;; start the "attenuation process", but we want the plot to start at the
      ;; date that the user specified.
      #:x-min start #:x-label #f #:y-label #f #:legend-anchor 'bottom-left))
   renderer-tree))

(define pmc-trends-chart%
  (class trends-chart%
    (init-field database sport-charms sport-zones)
    (super-new)

    (define *sea-green-hl* (make-object color% #x2e #x8b #x57 0.2))

    (define data-valid? #f)

    (define pmc-data #f)
    (define pmc-sessions #f)
    (define session-markers '())        ; see read-session-markers
    (define params (hash))              ; chart parameters for current plot
    (define pmc-today-snip #f)
    (define saved-pmc-today-snip-location #f)

    (define cached-day #f)
    (define cached-badge #f)
    (define cached-session-badges (make-hash))

    (define plot-scale-x 1)
    (define plot-scale-y 1)
    (define tss-search-half-range 1)

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
      (set! tss-search-half-range 1)
      (set! pmc-today-snip #f)
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
                (cons (list #f (send sport-charms get-sport-name sport sub-sport))
                      (cons (list (~r t #:precision 0) (or h "Unnamed session"))
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
                      (list "" (tsb-factor-label (tsb->label tsb))))
                 (and (hash-ref params 'show-form? #t)
                      (list "Form" (~r tsb #:precision 1)))
                 (list "Date" (calendar-date->string ts)))))
        (set! cached-day day)
        (set! cached-badge (make-hover-badge info)))
      cached-badge)

    ;; Check if mouse cursor at X, Y is close to one of the TSS points, and
    ;; return an overlay highlighting that TSS point and displaying the
    ;; session(s) associated with it.
    ;;
    ;; ENTRY is the tss data right under the cursor X position, as obtained by
    ;; `get-pmc-data-for-timestamp', and INDEX is the position in the PMC-DATA
    ;; data frame for that entry -- we'll use INDEX to "look around" for
    ;; nearby data points along the X series, in case the PMC plot covers a
    ;; long period and multiple days are close in X space...
    (define/private (hover-over-tss-point x y entry index pmc-data)
      (let/ec return

        ;; First check if we target the current ENTRY -- this would save
        ;; another data-frame lookup.
        (match-let ([(vector ts day _ctl _atl _tsb tss) entry])
          ;; Points are placed in the middle of the day (12pm) rather than at
          ;; the beginning of the day (12am)
          (define ts-midday (+ ts (* 12 3600)))
          (when (and (> tss 0)  ; only when there's some activity
                     ;; the check below is redundant
                     ;; (< (/ (abs (- ts-midday x)) plot-scale-x) 10)
                     (< (/ (abs (- tss y)) plot-scale-y) 10))
            (return (list
                     (points
                      (list (vector ts-midday tss))
                      #:color "black"
                      #:fill-color '(238 51 119)  ; magenta
                      #:size 14
                      #:line-width 0.5
                      #:sym 'fullcircle4)
                     (let ([b (get-session-badge ts day)])
                       (if b (hover-label x y b) null))))))

        (for ([idx (in-inclusive-range
                    (max (- index tss-search-half-range) 0)
                    (min (+ index tss-search-half-range) (sub1 (df-row-count pmc-data))))]
              ;; skip index itself, as it was already searched above
              #:unless (equal? idx index))
          (match-define (vector ts-midday day tss)
            (df-ref* pmc-data idx "tsmidday" "day" "tss"))
          (when (and (> tss 0)  ; only when there's some activity
                     ;; the check below is redundant
                     ;; (< (/ (abs (- ts-midday x)) plot-scale-x) 10)
                     (< (/ (abs (- tss y)) plot-scale-y) 10))
            (return (list
                     (points
                      (list (vector ts-midday tss))
                      #:color "black"
                      #:fill-color '(238 51 119)  ; magenta
                      #:size 14
                      #:line-width 0.5
                      #:sym 'fullcircle4)
                     (let ([b (get-session-badge ts-midday day)])
                       (if b (hover-label x y b) null))))))

        #f                              ; nothing found
        ))

    (define (plot-hover-callback snip event x y)
      (define renderers
        (if (good-hover? snip x y event)
            (let-values ([(entry index) (get-pmc-data-for-timestamp pmc-data x)])
              (if (and entry params)
                  (match-let ([(vector ts day ctl atl tsb tss) entry])
                    (cond
                      ((and (hash-ref params 'show-tss? #f)
                            (hover-over-tss-point x y entry index pmc-data))
                       => values)
                      (else
                       ;; Otherwise, highlight the entire day -- while the
                       ;; plot is linear, values are only computed for an
                       ;; entire day.
                        (list
                         (hover-vrange ts (+ ts (* 24 3600)) *sea-green-hl*)
                         (let ([b (get-day-badge ts day ctl atl tsb tss params)])
                           (if b (hover-label x y b) null))))))
                  ;; No entry
                  null))
            ;; Not a good hover
            null))
        (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-build-pmc-data)
      (if params
          (let* ([rt (make-renderer-tree params pmc-data session-markers)]
                 [snip (insert-plot-snip canvas params rt)])
            (set-mouse-event-callback snip plot-hover-callback)
            (define-values (sx sy) (compute-plot-dpp snip))
            (set! plot-scale-x sx)
            (set! plot-scale-y sy)
            (set! tss-search-half-range (exact-ceiling (/ (* sx 8) (* 24 3600))))
            (let ([saved-location (get-snip-location pmc-today-snip)]
                  [legend (pmc-plot-legend pmc-data)])
              (if legend
                  (begin
                    (set! pmc-today-snip (new pict-snip% [pict legend]))
                    (send canvas set-floating-snip pmc-today-snip 0 0)
                    (move-snip-to pmc-today-snip (or saved-location saved-pmc-today-snip-location)))
                  (set! pmc-today-snip #f))))
          #f))

    (define/override (save-plot-image file-name width height)
      (when data-valid?
        (let ((rt (make-renderer-tree params pmc-data session-markers)))
          (save-plot-to-file file-name width height params rt))))

    (define/private (maybe-build-pmc-data)
      (unless data-valid?
        (set! params (send this get-chart-settings))
        (when params
          (match-define
            (cons start-date end-date)
            (hash-ref params 'timestamps))
          ;; NOTE: we extend the range so ATL CTL at the start of the range is
          ;; correctly computed (w/ exponential averaging, all past TSS values
          ;; have a contribution to the present)
          (let ((start (max 0 (- start-date (* 4 default-ctl-range 24 3600))))
                (end (if (number? end-date)
                         end-date
                         ;; Add a future period if the date range extends to
                         ;; today -- this makes the plot look nicer.
                         (+ (current-seconds) (* 6 7 24 3600)))))
            (set! pmc-data (prepare-pmc database start end))
            (set! pmc-sessions (fetch-pmc-sessions database start end)))
          (set! session-markers (read-session-markers database params))
          (set! data-valid? #t))))

    (define/override (get-chart-settings)
      (define sdata (super get-chart-settings))
      (let ([location (or (get-snip-location pmc-today-snip)
                          saved-pmc-today-snip-location)])
        (if location
            (hash-set sdata 'pmc-today-location location)
            sdata)))

    (define/override (put-chart-settings data)
      (set! saved-pmc-today-snip-location
            (hash-ref data 'pmc-today-location #f))
      (super put-chart-settings data))

    ))
