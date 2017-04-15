#lang racket/base

;; trends-bavg.rkt -- aggregate best-average chart
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
 pict
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
 "workers.rkt"
 "snip-canvas.rkt"
 "utilities.rkt"
 "pdmodel.rkt")


(struct bavg-params tc-params (start-date end-date sport series zero-base? heat-map? heat-map-pct cp-params))

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
             [label "Data Series: "]
             [choices '("***************************")]
             [callback (lambda (c e) (on-series-selected (send c get-selection)))])))
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

    (define cp-gb (make-group-box-panel (send this get-client-pane)))

    (define an-range-start-input #f)
    (define an-range-end-input #f)
    (define ae-range-start-input #f)
    (define ae-range-end-input #f)

    (define estimate-cp-checkbox
      (let ((p (make-horizontal-pane cp-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Estimate Critical Power or Velocity (2 Parameter Model)"]
             [value #f]
             [callback (lambda (c e) (on-estimate-cp (send c get-value)))])))

    (let ((p (make-horizontal-pane cp-gb #f)))
      (send p spacing al-dlg-item-spacing)
      (set! an-range-start-input
            (new number-input-field% [parent p]
                 [label "Anaerobic search range "] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-an-range-start v))]))
      (set! an-range-end-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-an-range-end v))])))

    (let ((p (make-horizontal-pane cp-gb #f)))
      (send p spacing al-dlg-item-spacing)
      (set! ae-range-start-input
            (new number-input-field% [parent p]
                 [label "Aerobic search range "] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-ae-range-start v))]))
      (set! ae-range-end-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-ae-range-end v))])))

    ;; Default ranges for the search intervals
    (send an-range-start-input set-numeric-value 120)
    (send an-range-end-input set-numeric-value 300)
    (send ae-range-start-input set-numeric-value 420)
    (send ae-range-end-input set-numeric-value 1200)

    (define (on-series-selected series-index)
      (let* ((axis (list-ref axis-choices series-index))
             (have-cp-estimator? (send axis have-cp-estimate)))
        (send estimate-cp-checkbox enable have-cp-estimator?)
        (on-estimate-cp have-cp-estimator?)))

    (define (on-estimate-cp v)
      (send an-range-start-input enable v)
      (send an-range-end-input enable v)
      (send ae-range-start-input enable v)
      (send ae-range-end-input enable v))

    (define (validate-cp-ranges)
      (let ((anstart (send an-range-start-input get-converted-value))
            (anend (send an-range-end-input get-converted-value))
            (aestart (send ae-range-start-input get-converted-value))
            (aeend (send ae-range-end-input get-converted-value)))
        (when (number? anstart)
          (send an-range-start-input mark-valid
                (or (not anend) (eq? anend 'empty) (< anstart anend))))
        (when (number? anend)
          (send an-range-end-input mark-valid
                (or (not aestart) (eq? aestart 'empty) (< anend aestart))))
        (when (number? aestart)
          (send ae-range-start-input mark-valid
                (or (not aeend) (eq? aeend 'empty) (< aestart aeend))))))

    (define (on-an-range-start s)
      (validate-cp-ranges))
    (define (on-an-range-end e)
      (validate-cp-ranges))
    (define (on-ae-range-start s)
      (validate-cp-ranges))
    (define (on-ae-range-end e)
      (validate-cp-ranges))

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
    (on-series-selected (send series-selector get-selection))

    (define/override (has-valid-data?)
      (or (not (send estimate-cp-checkbox is-enabled?))
          (not (send estimate-cp-checkbox get-value))
          (and
           (number? (send an-range-start-input get-converted-value))
           (number? (send an-range-end-input get-converted-value))
           (number? (send ae-range-start-input get-converted-value))
           (number? (send ae-range-end-input get-converted-value)))))

    (define/public (get-restore-data)
      (hash
       'name (send name-field get-value)
       'title (send title-field get-value)
       'date-range (send date-range-selector get-restore-data)
       'sport (send sport-selector get-selection)
       'series (let ((axis (list-ref axis-choices (send series-selector get-selection))))
                 (send axis series-name))
       'zero-base? (send zero-base-checkbox get-value)
       'show-heat? (send show-heat-checkbox get-value)
       'heat-percent (send heat-percent-input get-converted-value)
       'estimate-cp? (send estimate-cp-checkbox get-value)
       'an-start (send an-range-start-input get-converted-value)
       'an-end (send an-range-end-input get-converted-value)
       'ae-start (send ae-range-start-input get-converted-value)
       'ae-end (send ae-range-end-input get-converted-value)))

    (define/public (restore-from data)
      (when database
        (send date-range-selector set-seasons (db-get-seasons database)))

      (define hdata
        ;; Old data format was just a list, not a hash.  Convert it to a hash
        ;; now.
        (if (list? data)
            (match-let (((list name title dr sport series zero-base? show-heat? heat-pct) data))
              (hash
               'name name
               'title title
               'date-range dr
               'sport sport
               'series series
               'zero-base? zero-base?
               'show-heat? show-heat?
               'heat-percent heat-pct))
            data))

      (send name-field set-value (hash-ref hdata 'name "Bavg"))
      (send title-field set-value (hash-ref hdata 'title "BestAvg Chart"))
      (let ((dr (hash-ref hdata 'date-range #f)))
        (when dr
          (send date-range-selector restore-from dr)))
      (let ((sp (hash-ref hdata 'sport #f)))
        (when sp
          (send sport-selector set-selected-sport (car sp) (cdr sp))))
      (let ((series (hash-ref hdata 'series #f)))
        (when series
          (let ((index (find-axis axis-choices series)))
            (if index
                (send series-selector set-selection index)
                (send series-selector set-selection 0)))))
      (send zero-base-checkbox set-value (hash-ref hdata 'zero-base? #f))
      (let ((show-heat? (hash-ref hdata 'show-heat? #f)))
        (send show-heat-checkbox set-value show-heat?)
        (on-show-heat show-heat?))
      (let ((heat-pct (hash-ref hdata 'heat-percent #f)))
        (if (number? heat-pct)
            (send heat-percent-input set-numeric-value heat-pct)
            (send heat-percent-input set-value "")))
      (send estimate-cp-checkbox set-value (hash-ref hdata 'estimate-cp? #f))
      (let ((anstart (hash-ref hdata 'an-start 120)))
        (if (number? anstart)
            (send an-range-start-input set-numeric-value anstart)
            (send an-range-start-input set-value "")))
      (let ((anend (hash-ref hdata 'an-end 300)))
        (if (number? anend)
            (send an-range-end-input set-numeric-value anend)
            (send an-range-end-input set-value "")))
      (let ((aestart (hash-ref hdata 'ae-start 420)))
        (if (number? aestart)
            (send ae-range-start-input set-numeric-value aestart)
            (send ae-range-start-input set-value "")))
      (let ((aeend (hash-ref hdata 'ae-end 1200)))
        (if (number? aeend)
            (send ae-range-end-input set-numeric-value aeend)
            (send ae-range-end-input set-value "")))

      (validate-cp-ranges)
      (on-series-selected (send series-selector get-selection))
      (on-estimate-cp (send estimate-cp-checkbox get-value)))

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
                 (if (number? pct) (/ pct 100.0) pct))
               (if (send estimate-cp-checkbox get-value)
                   (cp2search
                    (send an-range-start-input get-converted-value)
                    (send an-range-end-input get-converted-value)
                    (send ae-range-start-input get-converted-value)
                    (send ae-range-end-input get-converted-value))
                   #f)))
            #f)))
    ))

(define (candidate-sessions db params)
  (let ((start (bavg-params-start-date params))
        (end (bavg-params-end-date params))
        (sport (bavg-params-sport params)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end)))

(struct tbavg (axis data heat-map plot-fn zero-base? cp-fn cp-pict))

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
    (define plot-fn
      (aggregate-bavg->spline-fn data))
    (define cp-fn #f)
    (define cp-pict #f)
    (when plot-fn
      (when (and (send axis have-cp-estimate) (bavg-params-cp-params params))
        (define cp2 (send axis cp-estimate plot-fn (bavg-params-cp-params params)))
        (set! cp-fn (send axis pd-function cp2))
        (set! cp-pict (send axis pd-data-as-pict cp2 plot-fn))))

    (tbavg axis data heat-map
           plot-fn
           (bavg-params-zero-base? params)
           cp-fn
           cp-pict)))

(define (make-render-tree data)
  (let-values (((min-x max-x min-y max-y) (aggregate-bavg-bounds (tbavg-data data))))
    (when (tbavg-zero-base? data) (set! min-y 0))
    (if (tbavg-plot-fn data)
        (let ((rt (list
                   (tick-grid)
                   (function (tbavg-plot-fn data) #:color (send (tbavg-axis data) plot-color) #:width 3))))
          (when (tbavg-cp-fn data)
            (set! rt
                  (cons (function (tbavg-cp-fn data) #:color "red" #:width 1.5 #:style 'long-dash) rt)))
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
    (define pd-model-snip #f)
    (define saved-pd-model-snip-location #f)

    (define (get-location snip)
      (if snip
          (let ((x (box 0))
                (y (box 0))
                (e (send (send snip get-admin) get-editor)))
            (if (send e get-snip-location snip x y #f)
                (cons (unbox x) (unbox y))
                #f))
          #f))

    (define (place-snip snip location)
      (define editor (send (send snip get-admin) get-editor))
      (if location
          (send editor move-to snip (car location) (cdr location))
          (if saved-pd-model-snip-location
              (send editor move-to snip (car saved-pd-model-snip-location) (cdr saved-pd-model-snip-location))
              (send editor move-to snip 50 50))))

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
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-params))
            (saved-generation generation)
            (saved-location (get-location pd-model-snip)))
        (send canvas set-snip #f)
        (send canvas set-background-message "Working...")
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
                                      min-x max-x min-y max-y)
                    (when (tbavg-cp-pict data)
                      (set! pd-model-snip (new pict-snip% [pict (tbavg-cp-pict data)]))
                      (send canvas set-floating-snip pd-model-snip)
                      (place-snip pd-model-snip saved-location)))))))
            (begin
              (send canvas set-snip #f)
              (send canvas set-background-message "No params for plot")))))

    (define/override (get-restore-data)
      (define sdata (super get-restore-data))
      (if (hash? sdata)
          (hash-set sdata 'pd-model-location (get-location pd-model-snip))
          sdata))

    (define/override (restore-from data)
      ;; Old style data was saved as a list
      (when (hash? data)
        (set! saved-pd-model-snip-location
              (hash-ref data 'pd-model-location #f)))
      (super restore-from data))

    ))
