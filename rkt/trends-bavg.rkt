#lang racket/base

;; trends-tt.rkt -- "Training Time chart, a punch card style chart
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
 racket/match
 racket/gui/base
 racket/math
 racket/format
 "database.rkt"
 "trends-chart.rkt"
 "icon-resources.rkt"
 "widgets.rkt"
 "plot-hack.rkt"
 "sport-charms.rkt"
 "data-frame.rkt"
 "al-widgets.rkt"
 "series-meta.rkt"
 "metrics.rkt"
 "spline-interpolation.rkt"
 "workers.rkt")


(struct bavg-params tc-params (start-date end-date sport series zero-base? heat-map? heat-map-pct))

(define default-axis-choices
  (list
   axis-speed
   axis-pace
   axis-speed-zone
   axis-grade
   axis-grade-inverted
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-stride
   axis-vratio
   axis-power
   axis-power-zone
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-power-phase-angle
   axis-left-peak-power-phase-angle
   axis-right-power-phase-angle
   axis-right-peak-power-phase-angle
   ))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis axis-list series-name)

  (define (match? axis)
    (let ((sn (if (list? axis)
                  (car axis)
                  (send axis series-name))))
      (equal? series-name sn)))

  (for/first ([(axis index) (in-indexed axis-list)]
              #:when (match? axis))
    index))

(provide bavg-chart-settings%)
(define bavg-chart-settings%
  (class al-edit-dialog%
    (init-field database
                [default-name "Bavg"]
                [default-title "Best Avg Chart"])

    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define time-gb (make-group-box-panel (send this get-client-pane)))
    (define sport-selector
      (new sport-selector% [parent time-gb] [sports-in-use-only? #t]))
    (define date-range-selector (new date-range-selector% [parent time-gb]))

    (define series-gb (make-group-box-panel (send this get-client-pane)))
    (define series-selector
      (let ((p (make-horizontal-pane series-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new choice% [parent p]
             [label "Data Series: "] [choices '("***************************")])))
    (define show-heat-checkbox #f)
    (define heat-percent-input #f)
    (let ((p (make-horizontal-pane series-gb #f)))
      (send p spacing al-dlg-item-spacing)
      (set! show-heat-checkbox
            (new check-box% [parent p] [label "Show number of sessions close to the best"]
                 [value #t]
                 [callback (lambda (c e) (on-show-heat (send c get-value)))]))
      (set! heat-percent-input
            (new number-input-field% [parent p]
                 [label "How close? "] [cue-text "0% .. 100%"]
                 [min-value 0] [max-value 100]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-heat-percentile v))])))
    (define zero-base-checkbox
      (let ((p (make-horizontal-pane series-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Start Y axis at 0"])))

    (define (on-show-heat show?)
      (send heat-percent-input enable show?))
    (define (on-heat-percentile p)
      #f)

    (define axis-choices
      (sort default-axis-choices string<? #:key (lambda (x) (send x axis-label))))
    
    (send series-selector clear)
    (for ([a axis-choices])
      (let ((n (send a axis-label)))
        (send series-selector append n)))

    (define/public (get-restore-data)
      (list
       (send name-field get-value)
       (send title-field get-value)
       (send date-range-selector get-restore-data)
       (send sport-selector get-selection)
       (let ((axis (list-ref axis-choices (send series-selector get-selection))))
         (send axis series-name))
       (send zero-base-checkbox get-value)
       (send show-heat-checkbox get-value)
       (send heat-percent-input get-converted-value)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))
      (match-define (list name title dr sport series zero-base? show-heat? heat-pct) data)
      (send name-field set-value name)
      (send title-field set-value title)
      (send date-range-selector restore-from dr)
      (send sport-selector set-selected-sport (car sport) (cdr sport))
      (let ((index (find-axis axis-choices series)))
        (if index
            (send series-selector set-selection index)
            (send series-selector set-selection 0)))
      (send zero-base-checkbox set-value zero-base?)
      (send show-heat-checkbox set-value show-heat?)
      (on-show-heat show-heat?)
      (if (number? heat-pct)
          (send heat-percent-input set-numeric-value heat-pct)
          (send heat-percent-input set-value "")))

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
              (bavg-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send sport-selector get-selection)
               (let ((axis (list-ref axis-choices (send series-selector get-selection))))
                 (send axis series-name))
               (send zero-base-checkbox get-value)
               (send show-heat-checkbox get-value)
               (let ((pct (send heat-percent-input get-converted-value)))
                 (if (number? pct) (/ pct 100.0) pct))))
            #f)))
    ))

(define (candidate-sessions db params)
  (let ((start (bavg-params-start-date params))
        (end (bavg-params-end-date params))
        (sport (bavg-params-sport params)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end)))

(struct tbavg (axis data heat-map plot-fn zero-base?))

(define (fetch-data database params progress-callback)
  (let* ((candidates (candidate-sessions database params))
         (axis-index (find-axis default-axis-choices (bavg-params-series params))))
    (define axis (if axis-index (list-ref default-axis-choices axis-index) #f))
    (unless axis (error "no axis for series"))
    (define data (aggregate-bavg candidates
                                 (send axis series-name)
                                 #:inverted? (send axis inverted-best-avg?)
                                 #:progress-callback progress-callback))
    (define heat-map
      (and (bavg-params-heat-map? params)
           (number? (bavg-params-heat-map-pct params))
           (let ((pct (bavg-params-heat-map-pct params)))
             (aggregate-bavg-heat-map
              data pct candidates
              (send axis series-name)
              #:inverted? (send axis inverted-best-avg?)
              #:as-percentage? #t))))
    (tbavg axis data heat-map
           (aggregate-bavg->spline-fn data)
           (bavg-params-zero-base? params))))

(define (make-render-tree data)
  (let-values (((min-x max-x min-y max-y) (aggregate-bavg-bounds (tbavg-data data))))
    (when (tbavg-zero-base? data) (set! min-y 0))
    (if (tbavg-plot-fn data)
        (let ((rt (list
                   (tick-grid)
                   (function (tbavg-plot-fn data) #:color (send (tbavg-axis data) plot-color) #:width 3))))
          (when (tbavg-heat-map data)
            (let* ((range (* 0.3 (- max-y min-y)))
                   (raw-fn (mk-spline-fn (tbavg-heat-map data)))
                   (fn (lambda (x) (let ((y (raw-fn x))) (+ min-y (* range y))))))
              (set! rt (cons (function-interval
                              (lambda (x) min-y)
                              (lambda (x) (+ min-y range))
                              #:color '(#xdc #xdc #xdc)
                              #:line1-style 'transparent
                              #:line2-style 'transparent)
                             rt))
              (set! rt (cons (function fn #:color '(#xb0 #x30 #x60) #:width 2)
                             rt))))
          (values rt min-x max-x min-y max-y))
        (values #f #f #f #f #f))))

(define (insert-plot-snip canvas axis rt min-x max-x min-y max-y)
  (if rt
      (parameterize ([plot-x-ticks (best-avg-ticks)]
                     [plot-x-label "Duration"]
                     [plot-x-transform log-transform]
                     [plot-y-ticks (send axis plot-ticks)]
                     [plot-y-label (send axis axis-label)])
        (plot-snip/hack canvas rt
                        #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
      (begin
        (send canvas set-snip #f)
        (send canvas set-background-message "No data to plot"))))

(provide bavg-trends-chart%)
(define bavg-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define generation 0)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new bavg-chart-settings%
           [default-name "BestAvg"]
           [default-title "Best Avg"]
           [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f))

    (define/override (export-data-to-file file formatted?)
      (when cached-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      (define data (tbavg-data cached-data))
      (define heat-map (tbavg-heat-map cached-data))
      (write-string "Duration, Value, Sid, Time" out)
      (when heat-map (write-string ", Heat" out))
      (newline out)
      (for (((datum index) (in-indexed data)))
        (match-define (list sid pos duration value) datum)
        (write-string (format "~a, ~a, ~a, ~a"
                              duration
                              value
                              sid
                              pos)
                      out)
        ;; Heat map, if present, should have the same number of items as the
        ;; main data, in the same order for the same durations.  We don't
        ;; check that, though.
        (when heat-map
          (let ((h (list-ref heat-map index)))
            (write-string (format ", ~a" (vector-ref h 1)) out)))
        (newline out)))

    (define/override (put-plot-snip canvas)
      (send canvas set-snip #f)
      (send canvas set-background-message "Working...")
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-params))
            (saved-generation generation))
        (if params
            (queue-task
             "bavg-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress p)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message
                            (format "Working (~a %)..." (exact-round (* p 100.0))))))))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define-values (rt min-x max-x min-y max-y) (make-render-tree data))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (insert-plot-snip canvas (tbavg-axis data) rt
                                      min-x max-x min-y max-y))))))
            (begin
              (send canvas set-snip #f)
              (send canvas set-background-message "No params for plot")))))
    
    ))
