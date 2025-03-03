#lang racket/base

;; trends-bavg.rkt -- aggregate best-average chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018-2021, 2023-2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         data-frame/spline
         pict
         pict/snip
         plot-container/hover-util
         plot/no-gui
         racket/class
         racket/gui/base
         racket/hash
         racket/match
         racket/math
         "../al-widgets.rkt"
         "../bavg-util.rkt"
         "../database.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../metrics.rkt"
         "../models/critical-power.rkt"
         "../session-df/native-series.rkt"
         "../session-df/series-metadata.rkt"
         "../session-df/xdata-series.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(define default-axis-choices
  (list
   axis-speed
   axis-pace
   axis-gap
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
   axis-combined-pedal-smoothness
   axis-left-power-phase-angle
   axis-left-peak-power-phase-angle
   axis-right-power-phase-angle
   axis-right-peak-power-phase-angle
   ))

;; Axis choices for lap swimming
(define swim-axis-choices
  (list
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-stroke-length
   axis-swim-swolf
   axis-swim-pace))

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

(provide mmax-chart-settings%)
(define mmax-chart-settings%
  (class* edit-dialog-base% (chart-settings-interface<%>)
    (init-field database
                [default-name "Mmax"]
                [default-title "Best Avg Chart"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    ;; determines if the SERIES-SELECTOR contains lap swimming series
    (define lap-swimming-series? #f)
    ;; last selection on the lap swimming series
    (define last-lap-swim-selection #f)
    ;; last selection on the default series
    (define last-non-lap-swim-selection #f)
    (define axis-choices #f)

    (define (install-axis-choices new-choices selection)
      (set! axis-choices
        (sort new-choices string<? #:key
              (lambda (x)
                (if (list? x) (car x) (send x axis-label)))))
      (send series-selector clear)
      (for ([a axis-choices])
        (let ((n (if (list? a) (car a) (send a axis-label))))
          (send series-selector append n)))
      (when (and selection (>= selection 0) (< selection (length axis-choices)))
        (send series-selector set-selection selection))
      (let ((selection (send series-selector get-selection)))
        (when selection
          (on-series-selected selection))))

    (define (on-sport-selected sport)
      (define lap-swimming?
        (and (eq? (car sport) 5) (eq? (cdr sport) 17)))
      (unless (eq? lap-swimming? lap-swimming-series?)
        (if lap-swimming?
            (begin
              (set! last-non-lap-swim-selection (send series-selector get-selection))
              (install-axis-choices
               (append swim-axis-choices (get-available-xdata-metadata database))
               last-lap-swim-selection))
            (begin
              (set! last-lap-swim-selection (send series-selector get-selection))
              (install-axis-choices
               (append default-axis-choices (get-available-xdata-metadata database))
               last-non-lap-swim-selection))))
      (set! lap-swimming-series? lap-swimming?))

    (define name-gb (make-group-box-panel (send this get-client-pane)))
    (define name-field (new text-field% [parent name-gb] [label "Name "]))
    (send name-field set-value default-name)
    (define title-field (new text-field% [parent name-gb] [label "Title "]))
    (send title-field set-value default-title)

    (define session-filter (new session-filter%
                                [database database]
                                [parent (send this get-client-pane)]
                                [sport-selected-callback on-sport-selected]))

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

    (define nm-range-start-input #f)
    (define nm-range-end-input #f)
    (define an-range-start-input #f)
    (define an-range-end-input #f)
    (define ae-range-start-input #f)
    (define ae-range-end-input #f)

    (define estimate-cp-choice
      (let ((p (make-horizontal-pane cp-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new choice%
             [parent p]
             [label "Estimate Critical Power or Velocity  "]
             [choices '("None" "2 Parameter Model (CP2)" "3 Parameter Model (CP3)")]
             [callback (lambda (c e) (on-estimate-cp (send c get-selection)))])))

    (let ((p (new grid-pane% [parent cp-gb]
                  [spacing al-dlg-item-spacing] [columns 3]
                  [alignment '(left center)])))
      (new message% [parent p] [label "Neuromuscular search range"])
      (set! nm-range-start-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-nm-range-start v))]))
      (set! nm-range-end-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-nm-range-end v))]))
      (new message% [parent p] [label "Anaerobic search range"])
      (set! an-range-start-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-an-range-start v))]))
      (set! an-range-end-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
                 [min-value 0] [allow-empty? #f]
                 [stretchable-width #f]
                 [valid-value-cb (lambda (v) (on-an-range-end v))]))
      (new message% [parent p] [label "Aerobic search range"])
      (set! ae-range-start-input
            (new number-input-field% [parent p]
                 [label ""] [cue-text "seconds"]
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
    (send nm-range-start-input set-numeric-value 15)
    (send nm-range-end-input set-numeric-value 45)
    (send an-range-start-input set-numeric-value 120)
    (send an-range-end-input set-numeric-value 300)
    (send ae-range-start-input set-numeric-value 720)
    (send ae-range-end-input set-numeric-value 1200)

    (define (on-series-selected series-index)
      (let* ((axis (list-ref axis-choices series-index))
             (have-cp-estimator? (send axis have-cp-estimate?)))
        (send estimate-cp-choice enable have-cp-estimator?)
        (send estimate-cp-choice set-selection (if have-cp-estimator? 2 0))
        (on-estimate-cp (if have-cp-estimator? 2 0))))

    (define (on-estimate-cp v)
      (case v
        ((0)
         (send nm-range-start-input enable #f)
         (send nm-range-end-input enable #f)
         (send an-range-start-input enable #f)
         (send an-range-end-input enable #f)
         (send ae-range-start-input enable #f)
         (send ae-range-end-input enable #f))
        ((1)                            ; CP2
         (send nm-range-start-input enable #f)
         (send nm-range-end-input enable #f)
         (send an-range-start-input enable #t)
         (send an-range-end-input enable #t)
         (send ae-range-start-input enable #t)
         (send ae-range-end-input enable #t))
        ((2)                            ; CP3
         (send nm-range-start-input enable #t)
         (send nm-range-end-input enable #t)
         (send an-range-start-input enable #t)
         (send an-range-end-input enable #t)
         (send ae-range-start-input enable #t)
         (send ae-range-end-input enable #t))))

    (define (validate-cp-ranges)
      (let ((nmstart (send nm-range-start-input get-converted-value))
            (nmend (send nm-range-end-input get-converted-value))
            (anstart (send an-range-start-input get-converted-value))
            (anend (send an-range-end-input get-converted-value))
            (aestart (send ae-range-start-input get-converted-value))
            (aeend (send ae-range-end-input get-converted-value)))
        (when (number? nmstart)
          (send nm-range-start-input mark-valid
                (or (not nmend) (eq? nmend 'empty) (< nmstart nmend))))
        (when (number? nmend)
          (send nm-range-end-input mark-valid
                (or (not nmstart) (eq? nmstart 'empty) (< nmend anstart))))
        (when (number? anstart)
          (send an-range-start-input mark-valid
                (or (not anend) (eq? anend 'empty) (< anstart anend))))
        (when (number? anend)
          (send an-range-end-input mark-valid
                (or (not aestart) (eq? aestart 'empty) (< anend aestart))))
        (when (number? aestart)
          (send ae-range-start-input mark-valid
                (or (not aeend) (eq? aeend 'empty) (< aestart aeend))))))

    (define (on-nm-range-start s)
      (validate-cp-ranges))
    (define (on-nm-range-end s)
      (validate-cp-ranges))
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

    (install-axis-choices
     (append default-axis-choices (get-available-xdata-metadata database))
     #f)

    (define/override (has-valid-data?)
      (or (not (send estimate-cp-choice is-enabled?))
          (let ((cp-model (send estimate-cp-choice get-selection)))
            (case cp-model
              ((0) #t)
              ((1)
               (and
                (number? (send an-range-start-input get-converted-value))
                (number? (send an-range-end-input get-converted-value))
                (number? (send ae-range-start-input get-converted-value))
                (number? (send ae-range-end-input get-converted-value))))
              ((2)
               (and
                (number? (send nm-range-start-input get-converted-value))
                (number? (send nm-range-end-input get-converted-value))
                (number? (send an-range-start-input get-converted-value))
                (number? (send an-range-end-input get-converted-value))
                (number? (send ae-range-start-input get-converted-value))
                (number? (send ae-range-end-input get-converted-value))))))))

    (define/public (get-chart-settings)
      (define cp-model
        (if (send estimate-cp-choice is-enabled?)
            (send estimate-cp-choice get-selection)
            0))
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value)
        'series (let ((axis (list-ref axis-choices (send series-selector get-selection))))
                  (send axis series-name))
        'zero-base? (send zero-base-checkbox get-value)
        'show-heat? (send show-heat-checkbox get-value)
        'heat-percent (let ((v (send heat-percent-input get-converted-value)))
                        (if (real? v) (/ v 100.0) v))
        'model (case cp-model ((0) 'none) ((1) 'cp2) ((2) 'cp3))
        'estimate-cp? (> cp-model 0)
        'nm-start (send nm-range-start-input get-converted-value)
        'nm-end (send nm-range-end-input get-converted-value)
        'an-start (send an-range-start-input get-converted-value)
        'an-end (send an-range-end-input get-converted-value)
        'ae-start (send ae-range-start-input get-converted-value)
        'ae-end (send ae-range-end-input get-converted-value))))

    (define/public (put-chart-settings data)

      (send session-filter restore-from data)

      (send name-field set-value (hash-ref data 'name "Mmax"))
      (send title-field set-value (hash-ref data 'title "BestAvg Chart"))
      (let ((series (hash-ref data 'series #f)))
        (when series
          (let ((index (find-axis axis-choices series)))
            (if index
                (send series-selector set-selection index)
                (send series-selector set-selection 0)))))
      (send zero-base-checkbox set-value (hash-ref data 'zero-base? #f))
      (let ((show-heat? (hash-ref data 'show-heat? #f)))
        (send show-heat-checkbox set-value show-heat?)
        (on-show-heat show-heat?))
      (let ((heat-pct (hash-ref data 'heat-percent #f)))
        (if (number? heat-pct)
            (send heat-percent-input set-numeric-value (* 100 heat-pct))
            (send heat-percent-input set-value "")))
      (let ((nmstart (hash-ref data 'nm-start 15)))
        (if (number? nmstart)
            (send nm-range-start-input set-numeric-value nmstart)
            (send nm-range-start-input set-value "")))
      (let ((nmend (hash-ref data 'nm-end 45)))
        (if (number? nmend)
            (send nm-range-end-input set-numeric-value nmend)
            (send nm-range-end-input set-value "")))
      (let ((anstart (hash-ref data 'an-start 120)))
        (if (number? anstart)
            (send an-range-start-input set-numeric-value anstart)
            (send an-range-start-input set-value "")))
      (let ((anend (hash-ref data 'an-end 300)))
        (if (number? anend)
            (send an-range-end-input set-numeric-value anend)
            (send an-range-end-input set-value "")))
      (let ((aestart (hash-ref data 'ae-start 720)))
        (if (number? aestart)
            (send ae-range-start-input set-numeric-value aestart)
            (send ae-range-start-input set-value "")))
      (let ((aeend (hash-ref data 'ae-end 1200)))
        (if (number? aeend)
            (send ae-range-end-input set-numeric-value aeend)
            (send ae-range-end-input set-value "")))

      (validate-cp-ranges)
      (on-series-selected (send series-selector get-selection))
      (on-estimate-cp (send estimate-cp-choice get-selection))

      ;; This needs to be set last, since on-series-selected will select CP3
      ;; by default, if the series allows CP estimation...
      (define model
        (case (hash-ref data 'model 'none)
          ((none) 0)
          ((cp2) 1)
          ((cp3) 2)))
      (send estimate-cp-choice set-selection model))

    (define/public (show-dialog parent)
      (send session-filter on-before-show-dialog)
      (and (send this do-edit parent) (get-chart-settings)))


    ))

(define (candidate-sessions db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (let ((sport (hash-ref params 'sport))
        (labels (hash-ref params 'labels))
        (equipment (hash-ref params 'equipment)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end
                              #:label-ids labels #:equipment-ids equipment)))

(struct tmmax (axis data heat-map plot-fn zero-base? cp cp-fn cp-pict))

(define (fetch-data database params progress-callback)
  (let* ((lap-swimming? (is-lap-swimming? (hash-ref params 'sport)))
         (candidates (candidate-sessions database params))
         (axis (find-series-metadata (hash-ref params 'series) lap-swimming?)))
    (unless axis (error "no axis for series"))

    (define (read-session-callback percent)
      (define msg (format "Reading sessions (~a %)"
                          (exact-round (* percent 100.0))))
      (progress-callback msg))
    (define data (get-aggregate-mmax candidates axis read-session-callback))
    (define heat-map
      (and (not (null? candidates))
           (hash-ref params 'show-heat? #f)
           (number? (hash-ref params 'heat-percent #f))
           (let ((pct (hash-ref params 'heat-percent 0.95)))
             (get-aggregate-mmax-heat-map candidates data pct axis))))
    (define (cp3-progress-callback percent)
      (define msg (format "Finding CP3 parameters (~a %)"
                          (exact-round (* percent 100.0))))
      (progress-callback msg))
    (define plot-fn (aggregate-mmax->spline-fn data))
    (define-values (cp cp-fn cp-pict)
      (if (and plot-fn
               (send axis have-cp-estimate?)
               (hash-ref params 'estimate-cp?))
          (let* ((nparams (if (equal? (hash-ref params 'model #f) 'cp3)
                              (hash-set params 'progress-callback cp3-progress-callback)
                              params))
                 (cp (send axis cp-estimate plot-fn nparams)))
            (values
             cp
             (send axis pd-function cp)
             (send axis pd-data-as-pict cp plot-fn)))
          (values #f #f #f)))

    (tmmax axis
           data
           heat-map
           plot-fn
           (hash-ref params 'zero-base?)
           cp
           cp-fn
           cp-pict)))

(define (make-renderer-tree data)
  (define-values (min-x max-x min-y max-y) (aggregate-mmax-bounds (tmmax-data data)))
  (when (tmmax-zero-base? data) (set! min-y 0))
  ;; Adjust MAX-Y (or MIN-Y for inverted plots) to include the CP3 max value
  ;; (CP2 goes to infinity at small values, so it is not useful to adjust the
  ;; plot for it).
  (when (and (tmmax-cp data) (cp3? (tmmax-cp data)))
    (if (send (tmmax-axis data) inverted-mean-max?)
        (set! min-y ((tmmax-cp-fn data) min-x))
        (set! max-y ((tmmax-cp-fn data) min-x))))
  (define rt (list (tick-grid)))
  (define (push-renderer r) (set! rt (cons r rt)))
  (when (tmmax-plot-fn data)
    (push-renderer
     (function (tmmax-plot-fn data)
               #:color (send (tmmax-axis data) plot-color)
               #:width 3)))
  (when (tmmax-cp-fn data)
    (push-renderer
     (function (tmmax-cp-fn data) #:color "red" #:width 2.0 #:style 'long-dash)))
  (when (tmmax-heat-map data)
    (let* ((range (* 0.3 (- max-y min-y)))
           (raw-fn (spline (tmmax-heat-map data)))
           ;; NOTE: splines will have huge peaks when two points with opposing
           ;; tangents are close together, this makes the heat map appear to
           ;; go over 100%.  Fix that manually with the `min` call.
           (fn (lambda (x) (let ((y (min 0.98 (raw-fn x)))) (+ min-y (* range y))))))
      (push-renderer (function-interval
                      (lambda (x) min-y)
                      (lambda (x) (+ min-y range))
                      #:color '(#xdc #xdc #xdc)
                      #:line1-style 'transparent
                      #:line2-style 'transparent))
      (push-renderer (function fn #:color '(#xb0 #x30 #x60) #:width 2))))
  (if (tmmax-plot-fn data)
      (values rt min-x max-x min-y max-y)
      (values #f #f #f #f #f)))

(define (generate-plot output-fn axis renderer-tree)
  (parameterize ([plot-x-ticks (mean-max-ticks)]
                 [plot-x-label "Duration"]
                 [plot-x-transform log-transform]
                 [plot-y-ticks (send axis plot-ticks)]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 30]
                 [plot-y-label (send axis axis-label)])
    (output-fn renderer-tree)))

(define (insert-plot-snip canvas axis rt min-x max-x min-y max-y)
  (if (and rt
           (rational? min-x)
           (rational? max-x)
           (rational? min-y)
           (rational? max-y))
      (generate-plot
       (lambda (renderer-tree)
         (plot-to-canvas renderer-tree canvas
                         #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
       axis rt)
      (begin
        (send canvas clear-all)
        (send canvas set-background-message "No data to plot")
        #f)))

(define (save-plot-to-file file-name width height axis rt min-x max-x min-y max-y)
  (when (and rt
           (rational? min-x)
           (rational? max-x)
           (rational? min-y)
           (rational? max-y))
    (generate-plot
     (lambda (renderer-tree)
       (plot-file renderer-tree file-name
                  #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y
                  #:width width #:height height))
     axis rt)))

(provide mmax-trends-chart%)
(define mmax-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define generation 0)
    (define pd-model-snip #f)
    (define saved-pd-model-snip-location #f)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new mmax-chart-settings%
           [default-name "BestAvg"]
           [default-title "Best Avg"]
           [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-updated #f)
          (hash-ref events 'session-created #f)))

    (define/override (export-data-to-file file formatted?)
      (when cached-data
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      (define data (tmmax-data cached-data))
      (define heat-map (tmmax-heat-map cached-data))
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

    (define (make-plot-hover-callback)
      (define params (send this get-chart-settings))
      (define format-value
        (send (tmmax-axis cached-data) value-formatter (hash-ref params 'sport)))

      (lambda  (snip event x y)
        (define info '())
        (define (add-info tag value) (set! info (cons (list tag value) info)))
        (define renderers '())
        (define (add-renderer r) (set! renderers (cons r renderers)))

        (when (and (good-hover? snip x y event) cached-data)
          (add-renderer (hover-vrule x))
          (let ((closest (lookup-duration (tmmax-data cached-data) x)))
            (when closest
              (match-define (cons (list sid1 ts1 d1 v1) (list sid2 ts2 d2 v2)) closest)
              (add-renderer (hover-markers (list (vector d1 v1) (vector d2 v2))))
              (add-info #f (date-time->string (get-session-start-time sid2)))
              (add-info "Point 2" (string-append (format-value v2) " @ " (duration->string d2)))
              (add-info #f (date-time->string (get-session-start-time sid1)))
              (add-info "Point 1" (string-append (format-value v1) " @ " (duration->string d1)))))
          (let ((cpfn (tmmax-cp-fn cached-data)))
            (when cpfn
              (let ((my (cpfn x)))
                (when my
                  (add-info "Model" (format-value my))))))
          (let ((plotfn (tmmax-plot-fn cached-data)))
            (when plotfn
              (let ((dy (plotfn x)))
                (when dy
                  (add-info (send (tmmax-axis cached-data) name) (format-value dy))))))

          (add-info "Duration" (duration->string x))
          (add-renderer (hover-label x y (make-hover-badge (reverse info)))))

        (set-overlay-renderers snip renderers)))

    (define/override (put-plot-snip canvas)
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-chart-settings))
            (saved-generation generation)
            (saved-location (get-snip-location pd-model-snip)))
        (send canvas clear-all)
        (send canvas set-background-message "Working...")
        (if params
            (queue-task
             "mmax-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress message)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message message))))
                 ;; Let the GUI thread run...
                 (sleep 0))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define-values (rt min-x max-x min-y max-y) (make-renderer-tree data))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (define snip (insert-plot-snip canvas (tmmax-axis data) rt
                                                   min-x max-x min-y max-y))
                    (when snip (set-mouse-event-callback snip (make-plot-hover-callback)))
                    (when (tmmax-cp-pict data)
                      (set! pd-model-snip (new pict-snip% [pict (tmmax-cp-pict data)]))
                      (send canvas set-floating-snip pd-model-snip 0 0)
                      (move-snip-to pd-model-snip (or saved-location saved-pd-model-snip-location))))))))
            (begin
              (send canvas clear-all)
              (send canvas set-background-message "No params for plot")))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (let ((data cached-data)
            (params (send this get-chart-settings)))
        (when (and params data)
          (define-values (rt min-x max-x min-y max-y) (make-renderer-tree data))
          (when rt
            (save-plot-to-file file-name width height
                               (tmmax-axis data)
                               rt min-x max-x min-y max-y)))))

    (define/override (get-chart-settings)
      (define sdata (super get-chart-settings))
      (if (hash? sdata)
          (let ((location (or (get-snip-location pd-model-snip)
                              saved-pd-model-snip-location)))
            (if location
                (hash-set sdata 'pd-model-location location)
                sdata))
          sdata))

    (define/override (put-chart-settings data)
      (set! saved-pd-model-snip-location
            (hash-ref data 'pd-model-location #f))
      (super put-chart-settings data))

    ))
