#lang racket/base
;; trends-scatter.rkt -- aggregate scatter chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017, 2018, 2019, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         data-frame/slr
         plot-container/hover-util
         plot-container
         plot
         pict
         racket/class
         racket/gui/base
         racket/hash
         racket/list
         racket/match
         racket/math
         racket/string
         racket/format
         "../al-widgets.rkt"
         "../metrics.rkt"
         "../session-df/native-series.rkt"
         "../session-df/series-metadata.rkt"
         "../session-df/xdata-series.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "trends-chart.rkt")

;; Return a function renderer for the linear regression defined by SLR
(define (slr-renderer slr)
  (function
   (lambda (x) (+ (slr-alpha slr) (* (slr-beta slr) x)))
   #:color '(#x2f #x4f #x4f)
   #:width 2
   #:label (format "r = ~a" (~r (slr-r slr) #:precision 2))))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)
  (for/first ([(axis index) (in-indexed axis-list)]
              #:when
              (let ((sn (if (list? axis)
                            (string-join
                             (map (lambda (m) (send m series-name)) (cdr axis))
                             "+")
                            (send axis series-name))))
                (equal? series-name sn)))
    index))

;; Axis choices for all non lap swimming sports.
(define default-axis-choices
  (list
   axis-speed
   axis-pace
   axis-gap
   axis-speed-zone
   axis-grade
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-vratio
   axis-stride
   axis-power
   axis-power-zone
   axis-left-right-balance
   (list "Torque Effectiveness (%)" axis-left-torque-effectiveness axis-right-torque-effectiveness)
   (list "Pedal Smoothness (%)" axis-left-pedal-smoothness axis-right-pedal-smoothness)
   axis-combined-pedal-smoothness
   (list "Platform Center Offset" axis-left-platform-centre-offset axis-right-platform-centre-offset)
   (list "Power Phase Start" axis-left-power-phase-start axis-right-power-phase-start)
   (list "Power Phase End" axis-left-power-phase-end axis-right-power-phase-end)
   (list "Power Phase Angle" axis-left-power-phase-angle axis-right-power-phase-angle)
   (list "Peak Power Phase Start" axis-left-peak-power-phase-start axis-right-peak-power-phase-start)
   (list "Peak Power Phase End" axis-left-peak-power-phase-end axis-right-peak-power-phase-end)
   (list "Peak Power Phase Angle" axis-left-peak-power-phase-angle axis-right-peak-power-phase-angle)
   ))

;; Axis choices for lap swimming
(define swim-axis-choices
  (list
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-stroke-length
   axis-swim-swolf
   axis-swim-pace))


;;.............................................. scatter-chart-settings% ....

(provide scatter-chart-settings%)
(define scatter-chart-settings%
  (class edit-dialog-base%
    (init-field database
                [default-name "Scatter"]
                [default-title "Scatter Plot"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define series1-selector #f)
    (define series2-selector #f)
    (define axis-choices #f)
    ;; determines if the SERIES-SELECTOR contains lap swimming series
    (define lap-swimming-series? #f)
    ;; last selection on the lap swimming series
    (define last-lap-swim-selection1 #f)
    (define last-lap-swim-selection2 #f)
    ;; last selection on the default series
    (define last-non-lap-swim-selection1 #f)
    (define last-non-lap-swim-selection2 #f)

    (define (install-axis-choices new-choices selection1 selection2)
      (set! axis-choices
        (sort new-choices string<? #:key
              (lambda (x)
                (if (list? x) (car x) (send x axis-label)))))

      (send series1-selector clear)
      (send series2-selector clear)
      (for ([a axis-choices])
        (let ((n (if (list? a) (car a) (send a axis-label))))
          (send series1-selector append n)
          (send series2-selector append n)))

      (define (valid? selection)
        (and selection (>= selection 0) (< selection (length axis-choices))))

      (when (valid? selection1)
        (send series1-selector set-selection selection1))

      (when (valid? selection2)
        (send series2-selector set-selection selection2))

      )

    (define (on-sport-selected sport)
      (define lap-swimming?
        (and (eq? (car sport) 5) (eq? (cdr sport) 17)))
      (unless (eq? lap-swimming? lap-swimming-series?)
        (if lap-swimming?
            (begin
              (set! last-non-lap-swim-selection1 (send series1-selector get-selection))
              (set! last-non-lap-swim-selection2 (send series2-selector get-selection))
              (install-axis-choices
               (append swim-axis-choices (get-available-xdata-metadata database))
               last-lap-swim-selection1 last-lap-swim-selection2))
            (begin
              (set! last-lap-swim-selection1 (send series1-selector get-selection))
              (set! last-lap-swim-selection2 (send series2-selector get-selection))
              (install-axis-choices
               (append default-axis-choices (get-available-xdata-metadata database))
               last-non-lap-swim-selection1 last-non-lap-swim-selection2))))
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
    (set! series1-selector
          (let ((p (make-horizontal-pane series-gb #f)))
            (send p spacing al-dlg-item-spacing)
            (new choice% [parent p]
                 [label "Data Series (X Axis): "] [choices '("***************************")])))
    (set! series2-selector
          (let ((p (make-horizontal-pane series-gb #f)))
            (send p spacing al-dlg-item-spacing)
            (new choice% [parent p]
                 [label "Data Series (Y Axis): "] [choices '("***************************")])))

    (define outlier-percentile-field #f)
    (define outlier-handling-choice #f)

    (let ((p (make-horizontal-pane series-gb)))
      (set! outlier-percentile-field
            (new number-input-field% [parent p]
                 [label "Outlier Percentile (%): "] [cue-text "0..50%"]
                 [min-value 0] [max-value 50]
                 [stretchable-width #f]))

      (set! outlier-handling-choice
            (new choice% [parent p]
                 [label ""] [choices '("Mark outliers" "Crop outliers")]
                 [stretchable-width #f])))

    (define/override (has-valid-data?)
      (send outlier-percentile-field has-valid-value?))

    (install-axis-choices
     (append default-axis-choices (get-available-xdata-metadata database)) #f #f)

    (define (get-selected-series-name series-selector)
      (let* ((index (send series-selector get-selection))
             (axis (list-ref axis-choices index)))
        (if (list? axis)
            (string-join
             (map (lambda (m) (send m series-name)) (cdr axis))
             "+")
            (send axis series-name))))

    (define (get-outlier-percentile)
      (let ((raw (send outlier-percentile-field get-converted-value)))
        (if (number? raw) (/ raw 100.0) #f)))

    (define (get-outlier-handling)
      (if (= (send outlier-handling-choice get-selection) 0)
          'mark
          'crop))

    (define/public (get-chart-settings)
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value)
        'series1 (get-selected-series-name series1-selector)
        'series2 (get-selected-series-name series2-selector)
        'opct (get-outlier-percentile)
        'ohandling (get-outlier-handling))))

    (define/public (put-chart-settings data)
      (send session-filter restore-from data)
      (when (hash? data)
        (send name-field set-value (hash-ref data 'name "Scatter"))
        (send title-field set-value (hash-ref data 'title "Scatter Plot"))
        (let ((series1 (hash-ref data 'series1 #f)))
          (when series1
            (let ((index (find-axis series1 axis-choices)))
              (when index
                (send series1-selector set-selection index)))))
        (let ((series2 (hash-ref data 'series2 #f)))
          (when series2
            (let ((index (find-axis series2 axis-choices)))
              (when index
                (send series2-selector set-selection index)))))
        (let ((opct (hash-ref data 'opct #f)))
          (if (number? opct)
              (send outlier-percentile-field set-numeric-value (* opct 100.0))
              (send outlier-percentile-field set-value "")))
        (let ((ohandling (hash-ref data 'ohandling 'mark)))
          (send outlier-handling-choice set-selection (if (eq? ohandling 'crop) 1 0)))))

    (define/public (show-dialog parent)
      (send session-filter on-before-show-dialog)
      (and (send this do-edit parent) (get-chart-settings)))

    ))

;; Fetch a list of session IDs from the database DB corresponding to
;; parameters in PARAMS (a SCATTER-PARAMS instance).  Sessions are fetched based
;; on start and end date and the selected sport.
(define (candidate-sessions db params)
  (match-define (cons start end) (hash-ref params 'timestamps (cons 0 0)))
  (let ((sport (hash-ref params 'sport))
        (labels (hash-ref params 'labels))
        (equipment (hash-ref params 'equipment)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end
                              #:label-ids labels #:equipment-ids equipment)))

(struct scatter (axis1 axis2 data bounds qbounds slr) #:transparent)
(define empty-bounds (vector #f #f #f #f))

;; Create a new bounds vector from the union of b1 and b2.
(define (union-bounds b1 b2)
  (match-define (vector b1-left b1-right b1-low b1-high) b1)
  (match-define (vector b2-left b2-right b2-low b2-high) b2)

  (define (u-min v1 v2) (if (and v1 v2) (min v1 v2) (or v1 v2)))
  (define (u-max v1 v2) (if (and v1 v2) (max v1 v2) (or v1 v2)))

  (vector
   (u-min b1-left b2-left)
   (u-max b1-right b2-right)
   (u-min b1-low b2-low)
   (u-max b1-high b2-high)))

;; Retrieve from the database DB scatter data based on PARAMS, which is a hash
;; that defines the sessions and data series, as returned by
;; `get-chart-settings`.  Returns a list of `scatter` structures.  The list
;; will contain a single entry if single series are involved, or multiple
;; entries (usually 2) if there dual series are involved, such as left and
;; right Torque Effectiveness.
;;
;; The aggregate scater is retrieved by `aggregate-scatter` which retrieves
;; from a database cache, or calculates it from the session data and updates
;; the cache.  This means that the first run for a large data set will be
;; slow, but subsequent ones should be much faster.
;; 
(define (fetch-data db params progress)

  ;; Series can be dual series, separated by a "+" sign, like "lteff+rteff"
  (define series1 (string-split (hash-ref params 'series1) "+"))
  (define series2 (string-split (hash-ref params 'series2) "+"))
  (define candidates (candidate-sessions db params))
  (define opct (hash-ref params 'opct #f))
  (define lap-swim? (is-lap-swimming? (hash-ref params 'sport)))

  ;; Helpers to adjust the progress callback to report the correct percentages
  ;; across all the scatter data that we produce (given that some series might
  ;; be "dual" series.
  (define total-rounds
    (if (= (length series1) (length series2))
        (length series1)
        (* (length series1) (length series2))))
  (define current-round -1)

  ;; Construct a `scatter` structure for a pair of series, S1 and S2
  (define (make-scatter candidates opct lap-swimming? s1 s2)

    ;; Patch up the progress monitor to report just our segment of progress...
    (set! current-round (add1 current-round))
    (define (pg n) (progress (/ (+ current-round n) total-rounds)))

    (let* ((meta1 (find-series-metadata s1 lap-swimming?))
           (meta2 (find-series-metadata s2 lap-swimming?))
           (data (aggregate-scatter candidates s1 s2 #:progress-callback pg))
           (bounds (aggregate-scatter-bounds
                    data
                    (send meta1 fractional-digits)
                    (send meta2 fractional-digits)))
           (qbounds (if (number? opct)
                        (aggregate-scatter-bounds/quantile data opct)
                        empty-bounds))
           (slr (aggregate-scatter-slr data)))
      (scatter meta1 meta2 (and (> (hash-count data) 0) data) bounds qbounds slr)))

  ;; If both series are dual, we pair them up, as this makes more sense for
  ;; the dual series that we have (left and right pedal).  This works nicely
  ;; if the user wants to plot Torque Effectiveness against Pedal Smoothness,
  ;; since they will get 2 plots one for the left and one for the right pedal,
  ;; if they plot TE against TE, they will get a 1:1 plot.
  (define scatter-data
    (if (= (length series1) (length series2))
        (for/list ([s1 (in-list series1)]
                   [s2 (in-list series2)])
          (make-scatter candidates opct lap-swim? s1 s2))
        (for*/list ([s1 (in-list series1)]
                    [s2 (in-list series2)])
          (make-scatter candidates opct lap-swim? s1 s2))))

  (if (>= 1 (length scatter-data))
      scatter-data
      ;; Ensure all scatter data structures have the same bounds and bounds,
      ;; so the plots line up nicely side-by-side
      (let-values ([(bounds qbounds)
                    (for/fold ((b empty-bounds) (qb empty-bounds))
                              ([s (in-list scatter-data)])
                      (values (union-bounds b (scatter-bounds s))
                              (union-bounds qb (scatter-qbounds s))))])
        (for/list ([s (in-list scatter-data)])
          (struct-copy scatter s
                       [bounds bounds]
                       [qbounds qbounds])))))

(define (make-render-tree data params [add-label? #f])
  (if (scatter-data data)
      (let ((rt (list (tick-grid)
                      (scatter-group-renderer
                       (scatter-data data)
                       #:label (and add-label? (send (scatter-axis2 data) plot-label))
                       #:color (send (scatter-axis2 data) plot-color)))))
        (when (scatter-slr data)
          (set! rt (cons (slr-renderer (scatter-slr data)) rt)))
        (reverse rt))
      #f))

(define (generate-plot output-fn data params rt)
  (let ((outlier-handling (hash-ref params 'ohandling))
        (bounds (scatter-bounds data))
        (qbounds (scatter-qbounds data))
        (rt rt))
    (when (eq? outlier-handling 'mark)
      (match-define (vector left right low high) qbounds)
      (when left
        (set! rt (cons (vrule left #:color "blue" #:style 'short-dash) rt)))
      (when right
        (set! rt (cons (vrule right #:color "blue" #:style 'short-dash) rt)))
      (when low
        (set! rt (cons (hrule low #:color "blue" #:style 'short-dash) rt)))
      (when high
        (set! rt (cons (hrule high #:color "blue" #:style 'short-dash) rt))))
    (parameterize ([plot-x-ticks (send (scatter-axis1 data) plot-ticks)]
                   [plot-x-label (send (scatter-axis1 data) axis-label)]
                   [plot-y-ticks (send (scatter-axis2 data) plot-ticks)]
                   [plot-y-label (send (scatter-axis2 data) axis-label)])
      (match-define (vector x-min x-max y-min y-max)
        (if (eq? outlier-handling 'mark) bounds qbounds))
      (output-fn rt x-min x-max y-min y-max))))

(define (insert-plot-snip canvas data params rt)
  (let ([plot-count (if rt (length rt) 0)])
    (cond
      ((= plot-count 0)
       (send canvas clear-all)
       (send canvas set-background-message "No data to plot"))
      ((= plot-count 1)
       (let-values ([(w h) (send canvas cell-dimensions plot-count #:columns 1)])
         (define snip
           (generate-plot
            (lambda (renderer-tree min-x max-x min-y max-y)
              (plot-snip
               renderer-tree
               #:width w #:height h
               #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
            (car data) params (car rt)))
         (send canvas set-snip snip)))
      (#t
       (let-values ([(w h) (send canvas cell-dimensions plot-count #:columns 2 #:spacing 5)])
         (define snips
           (for/list ([r (in-list rt)] [d (in-list data)])
             (generate-plot
              (lambda (renderer-tree min-x max-x min-y max-y)
                (plot-snip
                 renderer-tree
                 #:width w #:height h
                 #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
              d params r)))
         (send canvas set-snips/layout (keyword-apply cgroup '() '() 2 snips #:spacing 5)))))))


(define (pict->png the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))
  
(define (save-plot-to-file file-name width height data params rt)
  (let ([plot-count (if rt (length rt) 0)])
    (cond
      ((= plot-count 0)
       (void))
      ((= plot-count 1)
       (generate-plot
        (lambda (renderer-tree min-x max-x min-y max-y)
          (plot-file renderer-tree file-name
                     #:width width #:height height
                     #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
        (car data) params (car rt)))
      (#t
       (let-values ([(w h) (values (exact-round (/ (- width (* 5 (sub1 plot-count))) plot-count)) height)])
         (define snips
           (for/list ([r (in-list rt)] [d (in-list data)])
             (generate-plot
              (lambda (renderer-tree min-x max-x min-y max-y)
                (plot-pict
                 renderer-tree
                 #:width w #:height h
                 #:x-min min-x #:x-max max-x #:y-min min-y #:y-max max-y))
              d params r)))
         (define pict (apply hc-append 5 snips))
         (pict->png pict file-name 'png))))))

(provide scatter-trends-chart%)
(define scatter-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define cached-renderer-tree #f)
    (define generation 0)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new scatter-chart-settings%
           [default-name "Scatter"]
           [default-title "Scatter Plot"]
           [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated-data #f)))

    (define/override (export-data-to-file file formatted?)
      ;; TODO: unclear how to implement it.  Also note that cached-data is a
      ;; list of scatter structures for scatter plots.
      (void))

    (define/override (put-plot-snip canvas)
      (send canvas clear-all)
      (send canvas set-background-message "Working...")
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-chart-settings))
            (saved-generation generation))
        (if params
            (queue-task
             "scatter-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress p)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message
                            (format "Working (~a %)..." (exact-round (* p 100.0))))))))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define add-label? (> (length data) 1))
               (define rt (for*/list ([d (in-list data)]
                                      [r (in-value (make-render-tree d params add-label?))]
                                      #:when r)
                            r))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (set! cached-renderer-tree rt)
                    (insert-plot-snip canvas data params rt))))))
            (begin
              (send canvas clear-all)
              (send canvas set-background-message "No params for plot")))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (let ((data cached-data)
            (rt cached-renderer-tree)
            (params (send this get-chart-settings)))
        (when (and data params rt)
          (save-plot-to-file file-name width height data params rt))))

    ))
