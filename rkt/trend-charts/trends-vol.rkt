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
 racket/math
 db/base
 plot/no-gui
 "../plot-hack.rkt"
 "../database.rkt"
 "../widgets/main.rkt"
 "../al-widgets.rkt"
 "trends-chart.rkt"
 "../plot-util.rkt"
 "../fmt-util.rkt")

(provide vol-trends-chart%)

(define histogram-gap 0.5)

;; Metric: 0 - time, 1 - distance, 2 - session count, 3 - tss
(struct vol-params tc-params (start-date end-date group-by sport sub-sport metric))

(define vol-chart-settings%
  (class edit-dialog-base%
    (init-field database
                [default-name "Trends"]
                [default-title "Trends Chart"])
    (super-new [title "Chart Settings"] [icon (edit-icon)] [min-height 10])

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
           (total(VAL.duration) / 3600.0) as duration,
           (total(VAL.distance) / 1000.0) as distance,
           count(VAL.session_id) as session_count,
           total(VAL.tss) as training_stress
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

(define (generate-plot output-fn data y-label)
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
    (output-fn
     (list (y-tick-lines)
           (discrete-histogram
            pdata
            #:y-max max-y
            #:color *sea-green*
            #:line-width 0
            #:gap histogram-gap))
     0 (length pdata) 0 max-y)))

(define (insert-plot-snip canvas data y-label)
  (generate-plot
   (lambda (renderer-tree min-x max-x min-y max-y)
    (plot-snip/hack
     canvas
     #:x-min min-x
     #:x-max max-x
     #:y-min min-y
     #:y-max max-y
     renderer-tree))
   data y-label))

(define (save-plot-to-file file-name width height data y-label)
  (generate-plot
   (lambda (renderer-tree min-x max-x min-y max-y)
     (plot-file renderer-tree file-name #:width width #:height height
                #:x-min min-x
                #:x-max max-x
                #:y-min min-y
                #:y-max max-y))
   data y-label))

(define vol-trends-chart%
  (class trends-chart%
    (init-field database)
    (super-new)

    (define data-valid? #f)
    (define sql-query #f)
    (define sql-query-result #f)
    (define chart-data #f)
    (define cached-hslot #f)
    (define cached-badge #f)

    (define/override (make-settings-dialog)
      (new vol-chart-settings%
           [default-name "Vol"]
           [default-title "Training Volume"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f)
      (set! cached-hslot #f)
      (set! cached-badge #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated #f)))

    (define/override (export-data-to-file file formatted?)
      (when chart-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      (write-string (format "Timestamp, ~a~%" (get-y-label)) out)
      (for ((datum chart-data) #:when (> (vector-length datum) 1))
        (match-define (vector timestamp val) datum)
        (write-string (format "~a, ~a~%" timestamp val) out)))

    (define (get-y-label)
      (let ((metric (vol-params-metric (send this get-params))))
        (case metric
          ((0) "Time (hours)")
          ((1) "Distance (km)")
          ((2) "Session Count")
          ((3) "Trainning Stress"))))

    (define (plot-hover-callback snip event x y)
      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))
      
      (define skip (discrete-histogram-skip))
      (define gap histogram-gap)
      
      (when (good-hover? x y event)
        (define params (send this get-params))
        (define metric (vol-params-metric params))
        (define format-value
          (case metric
            ((0) (lambda (v) (duration->string (* v 3600))))
            ((1) (lambda (v) (distance->string (* v 1000) #t)))
            ((2) (lambda (v) (format "~a activities" (exact-round v))))
            ((3) (lambda (v) (format "~a stress" (exact-round v))))))

        (define label
          (case metric
            ((0) "Time")
            ((1) "Distance")
            ((2) "Activities")
            ((3) "Stress")))
        
        (define-values (series slot) (xposition->histogram-slot x skip gap))
        (when (and chart-data series slot (= series 0) (< slot (length chart-data)))
          (let ((row (list-ref chart-data slot)))
            (when (> (vector-length row) 1)
              (match-define (vector timestamp val) row)
              (when (<= y val)
                ;; (add-mark-overlay snip x y (format "~a / ~a" series slot))
                (unless (eq? cached-hslot slot)
                  (set! cached-hslot slot)
                  (set! cached-badge
                        (make-hover-badge
                         (list (list label (format-value val))))))
                (when cached-badge
                  (add-renderer (pu-label x y cached-badge))))))))
      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (if data-valid?
          (let ((snip (insert-plot-snip canvas chart-data (get-y-label))))
            (set-mouse-event-callback snip plot-hover-callback))
          (begin
            (send canvas set-snip #f)
            (send canvas set-background-message "No data to plot"))))
    
    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (when data-valid?
        (save-plot-to-file file-name width height chart-data (get-y-label))))

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
              (set! sql-query (make-sql-query start end group-by sport sub-sport))
              (set! sql-query-result (get-data database sql-query metric))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! chart-data (simplify-labels chart-data group-by))
                (set! data-valid? #t)))))))

    ))
