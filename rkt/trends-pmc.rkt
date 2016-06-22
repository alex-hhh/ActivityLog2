#lang racket/base

;; trend-pmc.rkt -- "Performance Management Chart"
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
 racket/class
 racket/match
 racket/gui/base
 racket/stream
 db
 plot
 "plot-hack.rkt"
 "icon-resources.rkt"
 "database.rkt"
 "widgets.rkt"
 "al-widgets.rkt"
 "trends-chart.rkt")

(provide pmc-trends-chart%)

;; Number of day over which we average the daily TSS to obtain the "Chronic
;; Training Load" (fitness).  Default is 42 (6 weeks)
(define ctl-range 42)

;; Number of days over which we average the daily TSS to obtain the "Acute
;; Training Load" (fatigue).  Default is 7 days.
(define atl-range 7)

(struct pmc-params tc-params
  (start-date end-date show-form? show-fitness? show-fatigue? show-daily-tss?))

(define pmc-chart-settings%
  (class al-edit-dialog%
    (init-field database [default-name "Trends"] [default-title "Trends Chart"])
    (super-new [title "Chart Settings"]
               [icon edit-icon] [min-height 10] [tablet-friendly? #t])

    (define name-field
      (let ((p (make-horizontal-pane (send this get-client-pane) #f)))
        (send p spacing al-dlg-item-spacing)
        (new text-field% [parent p] [label "Name "])))
    (send name-field set-value default-name)

    (define title-field
      (let ((p (make-horizontal-pane (send this get-client-pane) #f)))
        (send p spacing al-dlg-item-spacing)
        (new text-field% [parent p] [label "Title "])))
    (send title-field set-value default-title)

    (define date-range-selector
      (let ((p (make-horizontal-pane (send this get-client-pane) #f)))
        (send p spacing al-dlg-item-spacing)
        (new date-range-selector% [parent p])))

    (define show-form-check-box #f)
    (define show-daily-tss-check-box #f)
    (define show-fitness-check-box #f)
    (define show-fatigue-check-box #f)

    (let ((p (new vertical-pane% [parent (send this get-client-pane)]
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

(define *sea-green* '(#x2e #x8b #x57))

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
  (class trends-chart% (init-field database) (super-new)

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
