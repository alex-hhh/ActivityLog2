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
 "spline-interpolation.rkt")


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

(define (make-group-box-panel parent (label ""))
  (new group-box-panel%
       [parent parent] [label label] [alignment '(left center)]
       [spacing al-dlg-item-spacing]
       [border 5]))

(provide bavg-chart-settings%)
(define bavg-chart-settings%
  (class al-edit-dialog%
    (init-field database
                [default-name "Bavg"]
                [default-title "Best Avg Chart"])

    (super-new [title "Chart Settings"]
               [icon edit-icon]
               [min-height 10]
               [tablet-friendly? #t])

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)

    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define sport-gb (make-group-box-panel (send this get-client-pane)))
    
    (define sport-selector
      (new sport-selector% [parent sport-gb] [sports-in-use-only? #t]))

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

    (define time-gb (make-group-box-panel (send this get-client-pane)))

    (define date-range-selector (new date-range-selector% [parent time-gb]))

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

(provide bavg-trends-chart%)
(define bavg-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define data-valid? #f)
    (define data '())
    (define data-plot-fn #f)
    (define axis #f)
    (define zero-base? #f)
    (define heat-map #f)

    (define/override (make-settings-dialog)
      (new bavg-chart-settings%
           [default-name "BestAvg"]
           [default-title "Best Avg"]
           [database database]))

    (define/override (invalidate-data)
      (set! data-valid? #f))

    (define/override (put-plot-snip canvas)
      (maybe-fetch-data)
      (when data-valid?
        (let-values (((min-x max-x min-y max-y) (aggregate-bavg-bounds data)))
          (when zero-base? (set! min-y 0))
          (let ((rt (list
                     (tick-grid)
                     (function data-plot-fn #:color (send axis plot-color) #:width 3))))
            (when heat-map
              (let* ((range (* 0.3 (- max-y min-y)))
                     (raw-fn (mk-spline-fn heat-map))
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
            (parameterize ([plot-x-ticks (best-avg-ticks)]
                           [plot-x-label "Duration"]
                           [plot-x-transform log-transform]
                           [plot-y-ticks (send axis plot-ticks)]
                           [plot-y-label (send axis axis-label)])
              (plot-snip/hack canvas rt
                              #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))))))

    (define (maybe-fetch-data)
      (unless data-valid?
        (let ((params (send this get-params)))
          (when params
            (let* ((candidates (candidate-sessions database params))
                   (axis-index (find-axis default-axis-choices (bavg-params-series params))))
              (set! axis (if axis-index (list-ref default-axis-choices axis-index) #f))
              (unless axis (error "no axis for series"))
              (set! data (aggregate-bavg candidates
                                         (send axis series-name)
                                         #:inverted? (send axis inverted-best-avg?)))
              (set! data-plot-fn (aggregate-bavg->spline-fn data))
              (set! zero-base? (bavg-params-zero-base? params))
              (if (and (bavg-params-heat-map? params) (number? (bavg-params-heat-map-pct params)))
                  (let ((pct (bavg-params-heat-map-pct params)))
                    (set! heat-map (aggregate-bavg-heat-map
                                    data pct candidates
                                    (send axis series-name)
                                    #:inverted? (send axis inverted-best-avg?)
                                    #:as-percentage? #t)))
                  (set! heat-map #f))
              (set! data-valid? #t))))))
    ))

