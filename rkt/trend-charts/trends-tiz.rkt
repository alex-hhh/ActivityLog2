#lang racket/base

;; trends-tiz.rkt -- "Time in Zone" chart
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
 racket/list
 racket/format
 db/base
 plot/no-gui
 "../plot-hack.rkt"
 "../database.rkt"
 "../widgets/main.rkt"
 "trends-chart.rkt"
 "../fmt-util.rkt"
 "../color-theme.rkt"
 "../plot-util.rkt")

(provide tiz-trends-chart%)

(define histogram-gap 0.5)

(struct tiz-params tc-params (start-date end-date group-by sport zone-metric))

(define tiz-chart-settings%
  (class edit-dialog-base%
    (init-field database [default-name "TIZ"] [default-title "Time in Zone"])
    (super-new [title "Chart Settings"] [icon (edit-icon)] [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define sport-choice
      (new choice% [parent time-gb] [label "Sport "]
           [choices '("Running" "Cycling")]))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define grouping-gb (make-group-box-panel (send this get-client-pane)))
    (define group-by-choice
      (new choice% [parent grouping-gb] [label "Group By "]
             [choices '("Week" "Month" "Year")]))
    (define zone-metric-choice
      (new choice% [parent grouping-gb] [label "Zone "]
           [choices '("Heart Rate" "Power")]))

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

(define (make-sql-query group-by)
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
                 "date(S.start_time, 'unixepoch', 'localtime', '-6 days', 'weekday 1')")
                ((eqv? group-by 1)       ; month
                 "date(S.start_time, 'unixepoch', 'localtime', 'start of month')")
                ((eqv? group-by 2)       ; year
                 "date(S.start_time, 'unixepoch', 'localtime', 'start of year')")
                (#t
                 #f))))

(define (get-data db sql-query sport zone-metric start end)
  (query-rows db sql-query sport zone-metric start end))

(define (generate-plot output-fn data)

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

  (define zcolors (drop (take (zone-colors) zmax) zmin))
  
  (define plot-colors (map cdr zcolors))
  (define plot-labels (map symbol->string (map car zcolors)))

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
                 [plot-y-label "Time in Zone (hours)"])
    (output-fn
     (list (y-tick-lines)
                     (stacked-histogram
                      pdata
                      #:colors plot-colors
                      #:labels plot-labels
                      #:line-widths '(0 0 0 0 0 0 0 0 0 0 )
                      #:gap histogram-gap))
     0 (length pdata) 0 max-y)))

(define (insert-plot-snip canvas data)
  (generate-plot
   (lambda (renderer-tree min-x max-x min-y max-y)
     (plot-snip/hack
      canvas
      #:x-min min-x
      #:x-max max-x
      #:y-min min-y
      #:y-max max-y
      renderer-tree))
   data))

(define (save-plot-to-file file-name width height data)
  (generate-plot
   (lambda (renderer-tree min-x max-x min-y max-y)
     (plot-file renderer-tree file-name #:width width #:height height
                #:x-min min-x
                #:x-max max-x
                #:y-min min-y
                #:y-max max-y))
   data))

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
    
    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated-data #f)))

    (define/override (export-data-to-file file formatted?)
      (when chart-data
        (call-with-output-file file
          (lambda (out) (export-data-as-csv out formatted?))
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out formatted?)
      (write-string
       "Timestamp, Z0, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10" out)
      (newline out)      
      (for ((datum chart-data) #:when (> (vector-length datum) 1))
        (match-define (vector timestamp z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10) datum)
        (write-string
         (if formatted?
             (format "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a~%"
                     timestamp
                     (duration->string (* z0 3600.0))
                     (duration->string (* z1 3600.0))
                     (duration->string (* z2 3600.0))
                     (duration->string (* z3 3600.0))
                     (duration->string (* z4 3600.0))
                     (duration->string (* z5 3600.0))
                     (duration->string (* z6 3600.0))
                     (duration->string (* z7 3600.0))
                     (duration->string (* z8 3600.0))
                     (duration->string (* z9 3600.0))
                     (duration->string (* z10 3600.0)))
             (format "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a~%"
                     timestamp z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))
         out)))

    (define cached-hslot #f)
    (define cached-badge #f)

    (define (plot-hover-callback snip event x y)

      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))
      (define skip (discrete-histogram-skip))
      (define gap histogram-gap)
      
      (when (good-hover? x y event)
        (define-values (series slot) (xposition->histogram-slot x skip gap))
        (when (and chart-data series slot (= series 0) (< slot (length chart-data)))
          (let ((row (list-ref chart-data slot)))
            (when (> (vector-length row) 1)
              (match-define (vector timestamp z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10) row)
              (define total (+ z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10))
              (when (<= y total)
                (unless (eq? cached-hslot slot)
                  (set! cached-hslot slot)
                  (set! cached-badge
                        (make-hover-badge
                         (append 
                          (for/list (((duration index) (in-indexed (in-vector row 1))) #:when (> duration 0))
                            (list (format "Zone ~a" index)
                                  (duration->string (* duration 3600))
                                  (string-append (~r (* 100 (/ duration total)) #:precision 1) " %")))
                          (list (list "Total" (duration->string (* total 3600))))))))
                (when cached-badge (add-renderer (pu-label x y cached-badge))))))))
      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (if data-valid?
          (let ((snip (insert-plot-snip canvas chart-data)))
            (set-mouse-event-callback snip plot-hover-callback))
          (begin
            (send canvas set-snip #f)
            (send canvas set-background-message "No data to plot"))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (when data-valid?
        (save-plot-to-file file-name width height chart-data)))

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
              (set! sql-query (make-sql-query group-by))
              (set! sql-query-result (get-data database sql-query sport zone start end))
              (when (> (length sql-query-result) 0)
                (set! chart-data (reverse (pad-data timestamps sql-query-result)))
                (set! chart-data (simplify-labels chart-data group-by))
                (set! data-valid? #t)))))))

    ))
