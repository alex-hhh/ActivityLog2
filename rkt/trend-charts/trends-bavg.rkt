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

(require pict
         plot/no-gui
         racket/class
         racket/gui/base
         racket/hash
         racket/match
         racket/math
         "../al-widgets.rkt"
         "../bavg-util.rkt"
         "../data-frame/meanmax.rkt"
         "../data-frame/spline.rkt"
         "../database.rkt"
         "../fmt-util.rkt"
         "../metrics.rkt"
         "../pdmodel.rkt"
         "../plot-hack.rkt"
         "../plot-util.rkt"
         "../series-meta.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

(struct mmax-params tc-params
  (start-date end-date sport labels equipment
              series zero-base? heat-map? heat-map-pct cp-params))

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
  (class edit-dialog-base%
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
              (install-axis-choices swim-axis-choices last-lap-swim-selection))
            (begin
              (set! last-lap-swim-selection (send series-selector get-selection))
              (install-axis-choices default-axis-choices last-non-lap-swim-selection))))
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

    (let ((p (new grid-pane% [parent cp-gb]
                  [spacing al-dlg-item-spacing] [columns 3]
                  [alignment '(left center)])))
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
    (send an-range-start-input set-numeric-value 120)
    (send an-range-end-input set-numeric-value 300)
    (send ae-range-start-input set-numeric-value 720)
    (send ae-range-end-input set-numeric-value 1200)

    (define (on-series-selected series-index)
      (let* ((axis (list-ref axis-choices series-index))
             (have-cp-estimator? (send axis have-cp-estimate?)))
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
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value)
        'series (let ((axis (list-ref axis-choices (send series-selector get-selection))))
                  (send axis series-name))
        'zero-base? (send zero-base-checkbox get-value)
        'show-heat? (send show-heat-checkbox get-value)
        'heat-percent (send heat-percent-input get-converted-value)
        'estimate-cp? (send estimate-cp-checkbox get-value)
        'an-start (send an-range-start-input get-converted-value)
        'an-end (send an-range-end-input get-converted-value)
        'ae-start (send ae-range-start-input get-converted-value)
        'ae-end (send ae-range-end-input get-converted-value))))

    (define/public (restore-from data)

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

      (send session-filter restore-from hdata)

      (send name-field set-value (hash-ref hdata 'name "Mmax"))
      (send title-field set-value (hash-ref hdata 'title "BestAvg Chart"))
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
      (let ((aestart (hash-ref hdata 'ae-start 720)))
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
      (send session-filter on-before-show-dialog)
      (if (send this do-edit parent)
          (get-settings)
          #f))

    (define/public (get-settings)
      (let ((dr (send session-filter get-date-range)))
        (if dr
            (let ((start-date (car dr))
                  (end-date (cdr dr)))
              (mmax-params
               (send name-field get-value)
               (send title-field get-value)
               start-date
               end-date
               (send session-filter get-sport)
               (send session-filter get-labels)
               (send session-filter get-equipment)
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
  (let ((start (mmax-params-start-date params))
        (end (mmax-params-end-date params))
        (sport (mmax-params-sport params))
        (labels (mmax-params-labels params))
        (equipment (mmax-params-equipment params)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end
                              #:label-ids labels #:equipment-ids equipment)))

(struct tmmax (axis data heat-map plot-fn zero-base? cp-fn cp-pict))

(define (fetch-data database params progress-callback)
  (let* ((lap-swimming? (is-lap-swimming? (mmax-params-sport params)))
         (axis-choices (if lap-swimming? swim-axis-choices default-axis-choices))
         (candidates (candidate-sessions database params))
         (axis-index (find-axis axis-choices (mmax-params-series params))))
    (define axis (if axis-index (list-ref axis-choices axis-index) #f))
    (unless axis (error "no axis for series"))
    (define data (get-aggregate-mmax candidates axis progress-callback))
    (define heat-map
      (and (mmax-params-heat-map? params)
           (number? (mmax-params-heat-map-pct params))
           (let ((pct (mmax-params-heat-map-pct params)))
             (get-aggregate-mmax-heat-map candidates data pct axis))))
    (define plot-fn
      (aggregate-mmax->spline-fn data))
    (define cp-fn #f)
    (define cp-pict #f)
    (when plot-fn
      (when (and (send axis have-cp-estimate?) (mmax-params-cp-params params))
        (define cp2 (send axis cp-estimate plot-fn (mmax-params-cp-params params)))
        (when cp2
          (set! cp-fn (send axis pd-function cp2))
          (set! cp-pict (send axis pd-data-as-pict cp2 plot-fn)))))

    (tmmax axis data heat-map
           plot-fn
           (mmax-params-zero-base? params)
           cp-fn
           cp-pict)))

(define (make-renderer-tree data)

  (let-values (((min-x max-x min-y max-y) (aggregate-mmax-bounds (tmmax-data data))))
    (when (tmmax-zero-base? data) (set! min-y 0))
    (if (tmmax-plot-fn data)
        (let ((rt (list
                   (tick-grid)
                   (function (tmmax-plot-fn data)
                             #:color (send (tmmax-axis data) plot-color)
                             #:width 3))))
          (when (tmmax-cp-fn data)
            (set! rt
                  (cons (function (tmmax-cp-fn data) #:color "red" #:width 1.5 #:style 'long-dash) rt)))
          (when (tmmax-heat-map data)
            (let* ((range (* 0.3 (- max-y min-y)))
                   (raw-fn (spline (tmmax-heat-map data)))
                   ;; NOTE: splines will have huge peaks when two points with
                   ;; opposing tangents are close together, this makes the
                   ;; heat map appear to go over 100%.  Fix that manually with
                   ;; the `min` call.
                   (fn (lambda (x) (let ((y (min 0.98 (raw-fn x)))) (+ min-y (* range y))))))
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
  (if rt
      (generate-plot
       (lambda (renderer-tree)
         (plot-snip/hack canvas renderer-tree
                         #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
       axis rt)
      (begin
        (send canvas set-snip #f)
        (send canvas set-background-message "No data to plot")
        #f)))

(define (save-plot-to-file file-name width height axis rt min-x max-x min-y max-y)
  (generate-plot
   (lambda (renderer-tree)
     (plot-file renderer-tree file-name
                #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y
                #:width width #:height height))
   axis rt))

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

    (define (plot-hover-callback snip event x y)
      (define info '())
      (define (add-info tag value) (set! info (cons (list tag value) info)))
      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))

      (when (and (good-hover? x y event) cached-data)
        (let ((params (send this get-params))
              (fmtval (send (tmmax-axis cached-data) value-formatter)))
          (add-renderer (pu-vrule x))
          (let ((closest (lookup-duration (tmmax-data cached-data) x)))
            (when closest
              (match-define (cons (list sid1 ts1 d1 v1) (list sid2 ts2 d2 v2)) closest)
              (add-renderer (pu-markers (list (vector d1 v1) (vector d2 v2))))
              (add-info #f (date-time->string (get-session-start-time sid2)))
              (add-info "Point 2" (string-append (fmtval v2) " @ " (duration->string d2)))
              (add-info #f (date-time->string (get-session-start-time sid1)))
              (add-info "Point 1" (string-append (fmtval v1) " @ " (duration->string d1)))))
          (let ((cpfn (tmmax-cp-fn cached-data)))
            (when cpfn
              (let ((my (cpfn x)))
                (when my
                  (add-info "Model" (fmtval my))))))
          (let ((plotfn (tmmax-plot-fn cached-data)))
            (when plotfn
              (let ((dy (plotfn x)))
                (when dy
                  (add-info (send (tmmax-axis cached-data) name) (fmtval dy)))))))

        (add-info "Duration" (duration->string x))
        (add-renderer (pu-label x y (make-hover-badge (reverse info)))))

      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-params))
            (saved-generation generation)
            (saved-location (get-snip-location pd-model-snip)))
        (send canvas set-snip #f)
        (send canvas set-background-message "Working...")
        (if params
            (queue-task
             "mmax-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress p)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message
                            (format "Working (~a %)..." (exact-round (* p 100.0))))))))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define-values (rt min-x max-x min-y max-y) (make-renderer-tree data))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (define snip (insert-plot-snip canvas (tmmax-axis data) rt
                                                   min-x max-x min-y max-y))
                    (when snip (set-mouse-event-callback snip plot-hover-callback))
                    (when (tmmax-cp-pict data)
                      (set! pd-model-snip (new pict-snip% [pict (tmmax-cp-pict data)]))
                      (send canvas set-floating-snip pd-model-snip)
                      (move-snip-to pd-model-snip (or saved-location saved-pd-model-snip-location))))))))
            (begin
              (send canvas set-snip #f)
              (send canvas set-background-message "No params for plot")))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (let ((data cached-data)
            (params (send this get-params)))
        (when (and params data)
          (define-values (rt min-x max-x min-y max-y) (make-renderer-tree data))
          (when rt
            (save-plot-to-file file-name width height
                               (tmmax-axis data)
                               rt min-x max-x min-y max-y)))))

    (define/override (get-restore-data)
      (define sdata (super get-restore-data))
      (if (hash? sdata)
          (let ((location (or (get-snip-location pd-model-snip)
                              saved-pd-model-snip-location)))
            (if location
                (hash-set sdata 'pd-model-location location)
                sdata))
          sdata))

    (define/override (restore-from data)
      ;; Old style data was saved as a list
      (when (hash? data)
        (set! saved-pd-model-snip-location
              (hash-ref data 'pd-model-location #f)))
      (super restore-from data))

    ))
