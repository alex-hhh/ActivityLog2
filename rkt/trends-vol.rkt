#lang racket/base
;; trends-vol.rkt -- Activity volume chart
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
 "trends-chart.rkt")

(provide vol-trends-chart%)

;; Metric: 0 - time, 1 - distance, 2 - session count, 3 - tss
(struct vol-params tc-params (start-date end-date group-by sport sub-sport metric))

(define vol-chart-settings%
  (class al-edit-dialog%
    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    (super-new [title "Chart Settings"] [icon edit-icon] [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define sport-selector (new sport-selector% [parent time-gb] [sports-in-use-only? #t]))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define grouping-gb (make-group-box-panel (send this get-client-pane)))
    (define group-by-choice
      (new choice% [parent grouping-gb] [label "Group By "]
             [choices '("Week" "Month" "Year")]))
    (define metric-choice
      (new choice% [parent grouping-gb] [label "Metric "]
           [choices '("Time" "Distance" "Session Count" "Effort")]))

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


(define (make-sql-query start-date end-date group-by sport sub-sport)
  (format "select ~a as period,
           round(total(VAL.duration) / 3600.0, 2) as duration,
           round(total(VAL.distance) / 1000.0, 2) as distance,
           count(VAL.session_id) as session_count,
           round(total(VAL.tss)) as training_stress
           from V_ACTIVITY_LIST VAL
           where VAL.start_time between ~a and ~a
             and ~a
           group by period order by period"
          (cond ((eqv? group-by 0)       ; week
                 "date(VAL.start_time, 'unixepoch', 'localtime', '-6 days', 'weekday 1')")
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

(define (get-data db sql-query metric)
  (for/list (([period duration distance count tss] (in-query db sql-query)))
    (vector period
            (case metric
              ((0) duration)
              ((1) distance)
              ((2) count)
              ((3) tss)))))

(define *sea-green* '(#x2e #x8b #x57))

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
    (define y-label #f)

    (define/override (make-settings-dialog)
      (new vol-chart-settings%
           [default-name "Vol"]
           [default-title "Training Volume"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (export-data-to-file file formatted?)
      (when chart-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      (write-string (format "Timestamp, ~a~%" y-label) out)
      (for ((datum chart-data) #:when (> (vector-length datum) 1))
        (match-define (vector timestamp val) datum)
        (write-string (format "~a, ~a~%" timestamp val) out)))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (and data-valid?
           (vol-trends-plot canvas chart-data y-label)))

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
              (set! y-label (case metric
                              ((0) "Time") ((1) "Distance")
                              ((2) "Session Count") ((3) "Trainning Stress")))
              (set! sql-query (make-sql-query start end group-by sport sub-sport))
              (set! sql-query-result (get-data database sql-query metric))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! chart-data (simplify-labels chart-data group-by))
                (set! data-valid? #t)))))))

    ))
