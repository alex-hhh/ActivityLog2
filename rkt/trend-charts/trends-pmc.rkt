#lang racket/base

;; trend-pmc.rkt -- "Performance Management Chart"
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
 racket/class
 racket/match
 racket/gui/base
 racket/stream
 racket/format
 db/base
 plot/no-gui
 "../database.rkt"
 "../widgets/main.rkt"
 "trends-chart.rkt"
 "../fmt-util.rkt"
 "../plot-util.rkt")

(provide pmc-trends-chart%)

;; Number of day over which we average the daily TSS to obtain the "Chronic
;; Training Load" (fitness).  Default is 42 (6 weeks)
(define ctl-range 42)

;; Number of days over which we average the daily TSS to obtain the "Acute
;; Training Load" (fatigue).  Default is 7 days.
(define atl-range 7)

(define pmc-chart-settings%
  (class edit-dialog-base%
    (init-field database [default-name "Trends"] [default-title "Trends Chart"])
    (super-new [title "Chart Settings"] [icon (edit-icon)] [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define show-form-check-box #f)
    (define show-daily-tss-check-box #f)
    (define show-fitness-check-box #f)
    (define show-fatigue-check-box #f)

    (define curves-gb (make-group-box-panel (send this get-client-pane)))

    (let ((p (new vertical-pane% [parent curves-gb]
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
              (new check-box% [parent q] [label "Fatigue"] [value #t]))))

    (define/public (get-chart-settings)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'timestamps (send date-range-selector get-selection)
       'show-form? (send show-form-check-box get-value)
       'show-fitness? (send show-fitness-check-box get-value)
       'show-fatigue? (send show-fatigue-check-box get-value)
       'show-tss? (send show-daily-tss-check-box get-value)))

    (define/public (put-chart-settings data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (send name-field set-value (hash-ref data 'name ""))
      (send title-field set-value (hash-ref data 'title ""))
      (let ((dr (hash-ref data 'date-range #f)))
        (when dr
          (send date-range-selector restore-from dr)))
      (send show-form-check-box set-value (hash-ref data 'show-form? #t))
      (send show-fitness-check-box set-value (hash-ref data 'show-fitness? #t))
      (send show-fatigue-check-box set-value (hash-ref data 'show-fatigue? #t))
      (send show-daily-tss-check-box set-value (hash-ref data 'show-tss? #f)))

    (define/public (show-dialog parent)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (and (send this do-edit parent) (get-chart-settings)))

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

;; NOTE: the form computed "today" is applied the next day.  That is, form is
;; plotted at the start of the day, fitness and fatigue at the end of the day.
;; See also issue #2
(define (get-form-data-series pmc-data)
  (for/list ((e (in-list pmc-data))
             (next (in-list (if (pair? pmc-data) (cdr pmc-data) '()))))
    (vector (vector-ref next 0) (- (vector-ref e 1) (vector-ref e 2)))))

(define (get-tss-data-series pmc-data)
  ;; NOTE: TSS data series does not contain zeroes
  (for/list ((e (in-list pmc-data)))
             ;; #:when (> (vector-ref e 3) 0))
    (vector (vector-ref e 0) (vector-ref e 3))))

;; Find the performance data corresponding to TIMESTAMP inside PMC-DATA.
;; Returns a list of 5 values:
;;
;; timestamp -- the start of day timestamp,
;;
;; CTL -- form, or chronic training load for the day,
;;
;; ATL -- fatigue, or acute training load for the day,
;;
;; TSB -- fitness, or training stress balance, calculated as CTL - ATL on the
;; *previous* day (see issue #2)
;;
;; TSS -- total training stress for the day
;;
;; NOTE that TSB or form is computed on yesterdays values.  I.e. form is shown
;; at the beginning of the day, while ATL, CTL are shown at the end of the
;; day.  See also issue #2
(define (get-pmc-data-for-timestamp pmc-data timestamp)
  (for/or ((yesterday pmc-data)
           (today (cdr pmc-data)))
    (match-define (vector ts ctl atl tss) today)
    (and (> timestamp ts)
         (< (- timestamp ts) (* 24 3600))
         (match-let (((vector yts yctl yatl ytss) yesterday))
           (list ts ctl atl (- yctl yatl) tss)))))

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

(define *sea-green* '(#x2e #x8b #x57))
(define *sea-green-hl* (make-object color% #x2e #x8b #x57 0.2))

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

(define (make-renderer-tree params pmc-data)
  (let ((rt (list (tick-grid))))
    (when (hash-ref params 'show-form? #t)
      (let ((form-renderer (make-form-renderer pmc-data)))
        (set! rt (cons form-renderer rt))))

    (when (hash-ref params 'show-fitness? #t)
      (let ((fitness-renderer (make-fitness-renderer pmc-data)))
        (set! rt (cons fitness-renderer rt))))

    (when (hash-ref params 'show-fatigue? #t)
      (let ((fatigue-renderer (make-fatigue-renderer pmc-data)))
        (set! rt (cons fatigue-renderer rt))))

    (when (hash-ref params 'show-tss? #f)
      (let ((daily-tss-renderer (make-tss-renderer pmc-data)))
        (set! rt (cons daily-tss-renderer rt))))

    ;; Add a "today" vertical line to the plot
    (set! rt
          (cons (vrule (current-seconds)) rt))

    rt))

(define (generate-plot output-fn renderer-tree)
  (parameterize ([plot-x-ticks (pmc-date-ticks)])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas params renderer-tree)
  (match-define (cons start-date end-date) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-to-canvas
      renderer-tree canvas
      #:x-min start-date #:x-label #f #:y-label #f))
   renderer-tree))

(define (save-plot-to-file file-name width height params renderer-tree)
  (match-define (cons start-date end-date) (hash-ref params 'timestamps (cons 0 0)))
  (generate-plot
   (lambda (renderer-tree)
     (plot-file
      renderer-tree
      file-name #:width width #:height height
      #:x-min start-date #:x-label #f #:y-label #f))
   renderer-tree))

(define pmc-trends-chart%
  (class trends-chart% (init-field database) (super-new)

    (define data-valid? #f)
    (define pmc-data #f)
    (define cached-day #f)
    (define cached-badge #f)

    (define/override (make-settings-dialog)
      (new pmc-chart-settings%
           [default-name "PMC"]
           [default-title "Performance"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f)
      (set! cached-day #f)
      (set! cached-badge #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)))

    (define/override (export-data-to-file file formatted?)
      (when pmc-data
        (call-with-output-file file
          (lambda (out) (export-data-as-csv out formatted?))
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out formatted?)
      (define (fmt val) (~r val #:precision 2 #:notation 'positional))
      (write-string "Day, ATL, CTL, TSB, Stress" out)
      (newline out)
      ;; PMC is computed well in advance to compensate for the long ramp up
      ;; time of ATL CTL values.  Only print out the actual range, values
      ;; before start-date are not accurate.
      (let ((params (send this get-chart-settings)))
        (match-define (cons start-date end-date)
          (hash-ref params 'timestamps (cons 0 0)))
        (for ((datum pmc-data) #:when (>= (vector-ref datum 0) start-date))
          (match-define (vector day ctl atl tss) datum)
          (write-string
           (format "~a, ~a, ~a, ~a, ~a~%"
                   (calendar-date->string day)
                   (fmt atl) (fmt ctl) (fmt (- ctl atl)) (fmt tss))
           out))))

    (define (plot-hover-callback snip event x y)
      (define info '())
      (define (add-info tag val) (set! info (cons (list tag val) info)))
      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))
      (when (good-hover? x y event)
        (let ((entry (get-pmc-data-for-timestamp pmc-data x))
              (params (send this get-chart-settings)))
          (when entry
            (match-define (list ts ctl atl tsb tss) entry)
            ;; Highlight the entire day -- while the plot is linear, values
            ;; are only computed for an entire day.
            (add-renderer (pu-vrange ts (+ ts (* 24 3600)) *sea-green-hl*))
            (unless (eq? cached-day ts)
              (add-info "Date" (calendar-date->string ts))
              (when (hash-ref params 'show-form? #t)
                (add-info "Form" (~r tsb #:precision 1)))
              (when (hash-ref params 'show-fitness? #t)
                (add-info "Fitness" (~r ctl #:precision 1)))
              (when (hash-ref params 'show-fatigue? #t)
                (add-info "Fatigue" (~r atl #:precision 1)))
              (when (hash-ref params 'show-tss? #f)
                (add-info "Stress" (~r tss #:precision 1)))
              (unless (null? info)
                (set! cached-badge (make-hover-badge info))))
            (when cached-badge (add-renderer (pu-label x y cached-badge))))))
      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-build-pmc-data)
      (let ((params (send this get-chart-settings)))
        (if params
            (let ((rt (make-renderer-tree params pmc-data)))
              (let ((snip (insert-plot-snip canvas params rt)))
                (set-mouse-event-callback snip plot-hover-callback)))
            #f)))

    (define/override (save-plot-image file-name width height)
      (when data-valid?
        (let ((params (send this get-chart-settings)))
          (when params
            (let ((rt (make-renderer-tree params pmc-data)))
              (save-plot-to-file file-name width height params rt))))))

    (define (maybe-build-pmc-data)
      (unless data-valid?
        (let ((params (send this get-chart-settings)))
          (when params
            (match-define
              (cons start-date end-date)
              (hash-ref params 'timestamps (cons 0 0)))
            ;; NOTE: we extend the range so ATL CTL at the start of the range
            ;; is correctly computed (w/ exponential averaging, all past TSS
            ;; values have a contribution to the present)
            (let ((start (max 0 (- start-date (* 4 ctl-range 24 3600))))
                  (end end-date))
              (set! pmc-data (produce-pmc-data/method-2 start end database)))
            (set! data-valid? #t)))))

    ))

