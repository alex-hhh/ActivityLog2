#lang racket/base

;; trends-trivol.rkt -- triathlon activity volume chart
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
 db
 plot
 "plot-hack.rkt"
 "icon-resources.rkt"
 "database.rkt"
 "widgets.rkt"
 "al-widgets.rkt"
 "trends-chart.rkt"
 "sport-charms.rkt")

(provide trivol-trends-chart%)

;; Group-by: 0 - week, 1 - month, 2 - year
;; Metric: 0 - time, 1 - distance, 2 - session count
(struct trivol-params tc-params (start-date end-date group-by metric))

(define trivol-chart-settings%
  (class al-edit-dialog%
    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    (super-new [title "Chart Settings"] [icon edit-icon]
               [min-height 10] [tablet-friendly? #t])

    (define name-field
      (let ((p (make-horizontal-pane (send this get-client-pane)  #f)))
        (send p spacing al-dlg-item-spacing)
        (new text-field% [parent p] [label "Name "])))
    (send name-field set-value default-name)

    (define title-field
      (let ((p (make-horizontal-pane (send this get-client-pane)  #f)))
        (send p spacing al-dlg-item-spacing)
        (new text-field% [parent p] [label "Title "])))
    (send title-field set-value default-title)

    (define date-range-selector
      (let ((p (make-horizontal-pane (send this get-client-pane)  #f)))
        (send p spacing al-dlg-item-spacing)
        (new date-range-selector% [parent p])))

    (define group-by-choice
      (let ((p (make-horizontal-pane (send this get-client-pane)  #f)))
        (send p spacing al-dlg-item-spacing)
        (new choice% [parent p] [label "Group By "]
             [choices '("Week" "Month" "Year")])))

    (define metric-choice
      (let ((p (make-horizontal-pane (send this get-client-pane)  #f)))
        (send p spacing al-dlg-item-spacing)
        (new choice% [parent p] [label "Metric "]
             [choices '("Time" "Distance" "Session Count")])))

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


(define (make-sql-query/time start-date end-date group-by)
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

(define (make-sql-query/count start-date end-date group-by)
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

(define (make-sql-query/distance start-date end-date group-by)
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

(define (get-data db sql-query)
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
  (class trends-chart% (init-field database) (super-new)

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
                      ((0) (make-sql-query/time start end group-by))
                      ((1) (make-sql-query/distance start end group-by))
                      ((2) (make-sql-query/count start end group-by))))
              (set! sql-query-result (get-data database sql-query))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! data-valid? #t)))))))
    
    ))
