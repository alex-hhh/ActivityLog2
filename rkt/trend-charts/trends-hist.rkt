#lang racket/base

;; trends-hist.rkt -- aggregate histogram chart
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
 plot/no-gui
 racket/class
 racket/gui/base
 racket/math
 racket/string
 racket/list
 racket/string
 racket/hash
 racket/format
 racket/match
 "trends-chart.rkt"
 "../widgets/main.rkt"
 "../al-widgets.rkt"
 "../session-df/native-series.rkt"
 "../session-df/series-metadata.rkt"
 "../metrics.rkt"
 "../utilities.rkt"
 "../plot-util.rkt"
 "../fmt-util.rkt"
 "../data-frame/histogram.rkt"
 "../sport-charms.rkt")

(provide hist-trends-chart%)


;;....................................................... axis selection ....

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


;;................................................. hist-chart-settings% ....

(define hist-chart-settings%
  (class* edit-dialog-base% (chart-settings-interface<%>)
    (init-field database
                [default-name "Hist"]
                [default-title "Histogram Chart"])

    (super-new [title "Chart Settings"]
               [icon (edit-icon)]
               [min-height 10])

    (define series-selector #f)
    (define axis-choices #f)
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
        (send series-selector set-selection selection)))

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
                                [parent (send this get-client-pane)]
                                [database database]
                                [sport-selected-callback on-sport-selected]))

    (define series-gb (make-group-box-panel (send this get-client-pane)))
    (set! series-selector
          (let ((p (make-horizontal-pane series-gb #f)))
            (send p spacing al-dlg-item-spacing)
            (new choice% [parent p]
                 [label "Data Series: "] [choices '("***************************")])))

    (define other-gb (make-group-box-panel (send this get-client-pane)))
    (define include-zeroes-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Include Zeroes"])))
    (define color-by-zone-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Color by Zone"])))
    (define show-as-pct-checkbox
      (let ((p (make-horizontal-pane other-gb #f)))
        (send p spacing al-dlg-item-spacing)
        (new check-box% [parent p] [label "Show as Percentage"])))
    (define bucket-width-field
      (new number-input-field% [parent other-gb]
           [label "Bucket Width "]
           [cue-text "1 to 100"]
           [min-value 1] [max-value 100]
           [stretchable-width #f]))
    (define outlier-trim-field
      (new number-input-field% [parent other-gb]
           [label "Outlier Trim (%) "] [cue-text "0 .. 100%"]
           [min-value 0] [max-value 100]
           [stretchable-width #f]))

    (define/override (has-valid-data?)
      (and
       (send bucket-width-field has-valid-value?)
       (send outlier-trim-field has-valid-value?)))

    (install-axis-choices default-axis-choices #f)

    (define (get-selected-series-name)
      (let* ((index (send series-selector get-selection))
             (axis (list-ref axis-choices index)))
        (if (list? axis)
            (string-join
             (map (lambda (m) (send m series-name)) (cdr axis))
             "+")
            (send axis series-name))))

    (define/public (get-chart-settings)
      (hash-union
       (send session-filter get-restore-data)
       (hash
        'name (send name-field get-value)
        'title (send title-field get-value)
        'series (get-selected-series-name)
        'include-zeroes? (send include-zeroes-checkbox get-value)
        'color-by-zone? (send color-by-zone-checkbox get-value)
        'show-as-pct? (send show-as-pct-checkbox get-value)
        'bucket-width (send bucket-width-field get-converted-value)
        'outlier-trim (send outlier-trim-field get-converted-value))))

    (define/public (put-chart-settings data)
      (send session-filter restore-from data)
      (when (hash? data)
        (send name-field set-value (hash-ref data 'name "Hist"))
        (send title-field set-value (hash-ref data 'title "Histogram Chart"))
        (let ((series (hash-ref data 'series #f)))
          (when series
            (let ((index (find-axis series axis-choices)))
              (when index
                (send series-selector set-selection index)))))
        (send include-zeroes-checkbox set-value (hash-ref data 'include-zeroes? #f))
        (send color-by-zone-checkbox set-value (hash-ref data 'color-by-zone? #f))
        (send show-as-pct-checkbox set-value (hash-ref data 'show-as-pct? #f))
        (let ((bw (hash-ref data 'bucket-width 'empty)))
          (if (eq? bw 'empty)
              (send bucket-width-field set-value "")
              (send bucket-width-field set-numeric-value bw)))
        (let ((otrim (hash-ref data 'outlier-trim 'empty)))
          (if (eq? otrim 'empty)
              (send outlier-trim-field set-value "")
              (send outlier-trim-field set-numeric-value otrim)))
        ))

    (define/public (show-dialog parent)
      (send session-filter on-before-show-dialog)
      (and (send this do-edit parent) (get-chart-settings)))

    ))

;; Fetch a list of session IDs from the database DB corresponding to
;; parameters in PARAMS (a HIST-PARAMS instance).  Sessions are fetched based
;; on start and end date and the selected sport.
(define (candidate-sessions db params)
  (match-define (cons start end) (hash-ref params 'timestamps))
  (let ((sport (hash-ref params 'sport))
        (labels (hash-ref params 'labels))
        (equipment (hash-ref params 'equipment)))
    (fetch-candidate-sessions db (car sport) (cdr sport) start end
                              #:label-ids labels #:equipment-ids equipment)))

;; AXIS is a list of series-metadata% instances (one for a single series, two
;; for a dual series), data is a list of histograms (as returned by
;; AGGREGATE-HIST), one for a single series, two for dual series.
(struct hist (axis data) #:transparent)

(define (fetch-data db params progress)
  (let* ((candidates (candidate-sessions db params))
         ;; Series can be "lteff+rteff" for dual series!
         (series (string-split (hash-ref params 'series) "+")))
    (hist
     (for/list ([s series]) (find-series-metadata s (is-lap-swimming? (hash-ref params 'sport))))
     (for/list ([s series]) (aggregate-hist candidates s #:progress-callback progress)))))

;; Prepare a histogram ready for rendering or export based on histogram DATA
;; and PARAMS.  The returned object is either a histogram/c or a
;; combined-histogram/c
(define (prepare-histogram data params)
  (define aspct? (hash-ref params 'show-as-pct?))
  (define zeroes? (hash-ref params 'include-zeroes?))

  (define dual? (>= (length (hist-axis data)) 2))
  (define axis1 (first (hist-axis data)))
  (define axis2 (and dual? (second (hist-axis data))))
  (define hist1 (first (hist-data data)))
  (define hist2 (and dual? (second (hist-data data))))
  (define bwidth-base
    (let ((bw (hash-ref params 'bucket-width)))
      (if (eq? bw 'empty) 1 (if (> bw 1) bw 1))))
  (define bwidth1 (* bwidth-base (send axis1 histogram-bucket-slot)))
  (define bwidth2 (and axis2 (* bwidth-base (send axis2 histogram-bucket-slot))))

  (define h1 (expand-histogram hist1
                               #:include-zeroes? zeroes?
                               #:bucket-width bwidth1
                               #:as-percentage? aspct?))
  (define h2 (and hist2
                  (expand-histogram hist2
                                    #:include-zeroes? zeroes?
                                    #:bucket-width bwidth2
                                    #:as-percentage? aspct?)))

  (define otrim
    (let ((ot (hash-ref params 'outlier-trim)))
      (if (eq? ot 'empty) 0 ot)))

  (when (> otrim 0)
    (let ((trim (/ otrim 100)))
      (when h1
        (set! h1 (trim-histogram-outliers h1 trim)))
      (when h2
        (set! h2 (trim-histogram-outliers h2 trim)))))

  (if h2 (combine-histograms h1 h2) h1))

(define (make-renderer-tree histogram data params)
  (define dual? (>= (length (hist-axis data)) 2))
  (define axis1 (first (hist-axis data)))
  (define axis2 (and dual? (second (hist-axis data))))
  (define factor-fn (and (hash-ref params 'color-by-zone?)
                         (send axis1 factor-fn (hash-ref params 'sport))))
  (define factor-colors (send axis1 factor-colors))

  (define (valid?)
    (and histogram
         (> (vector-length histogram) 0)
         (or (not dual?) (= 3 (vector-length (vector-ref histogram 0))))))

  (and (valid?)
       (list
        (tick-grid)
        (cond (dual?
               (histogram-renderer/dual
                histogram (send axis1 plot-label) (send axis2 plot-label)
                #:x-value-formatter (send axis1 value-formatter)
                #:color1 (send axis1 plot-color)
                #:color2 (send axis2 plot-color)))
              (factor-fn
               (histogram-renderer/factors
                histogram factor-fn factor-colors
                #:x-value-formatter (send axis1 value-formatter)))
             (#t
              (histogram-renderer
               histogram
               #:x-value-formatter (send axis1 value-formatter)
               #:color (send axis1 plot-color)))))))

(define (generate-plot output-fn axis params renderer-tree)
  (let* ((aspct? (hash-ref params 'show-as-pct?))
         (lap-swim? (is-lap-swimming? (hash-ref params 'sport)))
         (label (if aspct? "pct %" (if lap-swim? "# of lengths" "time"))))
    (parameterize ([plot-y-label label]
                   [plot-y-ticks (if (or aspct? lap-swim?)
                                     (linear-ticks)
                                     (time-ticks))]
                   [plot-x-ticks (send axis plot-ticks)]
                   [plot-x-label (send axis axis-label)])
      (output-fn renderer-tree))))

(define (insert-plot-snip canvas axis params renderer-tree)
  (if renderer-tree
      (generate-plot
       (lambda (renderer-tree)
         (plot-to-canvas renderer-tree canvas))
       axis params renderer-tree)
      (begin
        (send canvas set-snip #f)
        (send canvas set-background-message "No data to plot")
        #f)))

(define (save-plot-to-file file-name width height axis params renderer-tree)
  (generate-plot
   (lambda (renderer-tree)
     (plot-file renderer-tree file-name #:width width #:height height))
   axis params renderer-tree))

(define hist-trends-chart%
  (class trends-chart%
    (init-field database) (super-new)

    (define cached-data #f)
    (define histogram-data #f)
    (define generation 0)

    (define (get-generation) generation)

    (define/override (make-settings-dialog)
      (new hist-chart-settings%
           [default-name "Histogram"]
           [default-title "Histogram"]
           [database database]))

    (define/override (invalidate-data)
      (set! cached-data #f)
      (set! histogram-data #f))

    (define/override (is-invalidated-by-events? events)
      (or (hash-ref events 'session-deleted #f)
          (hash-ref events 'session-created #f)
          (hash-ref events 'session-updated-data #f)))

    (define/override (export-data-to-file file formatted?)
      (when (and cached-data histogram-data)
        (call-with-output-file file export-data-as-csv
          #:mode 'text #:exists 'truncate)))

    (define (export-data-as-csv out)
      (define data cached-data)
      (define params (send this get-chart-settings))
      (define histogram histogram-data)

      (define dual? (>= (length (hist-axis data)) 2))
      (define axis1 (first (hist-axis data)))
      (define axis2 (and dual? (second (hist-axis data))))

      (if dual?
          (write-string
           (format "Value, Rank ~a, Rank ~a~%"
                   (send axis1 series-name)
                   (send axis2 series-name))
           out)
          (write-string (format "Value, Rank ~a~%" (send axis1 series-name)) out))

      (define format-value (send axis1 value-formatter))
      (define ndigits (if (hash-ref params 'show-as-pct?)
                          2
                          (send axis1 fractional-digits)))
      (for ((item (in-vector histogram)))
        (write-string (format-value (vector-ref item 0)) out)
        (for ((rank (in-vector item 1)))
          (write-string ", " out)
          (write-string (~r rank #:precision ndigits) out))))

    (define (plot-hover-callback snip event x y)
      (define renderers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))

      (when (and (good-hover? x y event) cached-data histogram-data)
        (define dual? (>= (length (hist-axis cached-data)) 2))
        (define params (send this get-chart-settings))
        (define skip (if dual? 2.5 1.0))
        (define gap (if dual? 0.15 1/8))
        (define-values (series slot) (xposition->histogram-slot x skip gap))
        (when (and slot (< slot (vector-length histogram-data)))
          (define item (vector-ref histogram-data slot))
          (when (and series (< series (sub1 (vector-length item))))
            ;; NOTE first item in the vector is the bucket name, not the value
            (define value (vector-ref item (add1 series)))
            (when (<= y value)
              (let ((tag (cond ((hash-ref params 'show-as-pct?)
                                (format "~a %" (~r value #:precision 1)))
                               ((is-lap-swimming? (hash-ref params 'sport))
                                (format "~a pool lengths" (~r value #:precision 1)))
                               (#t
                                (duration->string value)))))
                (add-renderer (pu-label x y tag)))))))

      (set-overlay-renderers snip renderers))

    (define/override (put-plot-snip canvas)
      (send canvas set-snip #f)
      (send canvas set-background-message "Working...")
      (set! generation (add1 generation))
      (let ((previous-data cached-data)
            (params (send this get-chart-settings))
            (saved-generation generation))
        (if params
            (queue-task
             "hist-trends-chart%/put-plot-snip"
             (lambda ()
               (define (report-progress p)
                 (queue-callback
                  (lambda ()
                    (when (= saved-generation (get-generation))
                      (send canvas set-background-message
                            (format "Working (~a %)..." (exact-round (* p 100.0))))))))
               (define data (or previous-data (fetch-data database params report-progress)))
               (define hist (prepare-histogram data params))
               (define rt (make-renderer-tree hist data params))
               (queue-callback
                (lambda ()
                  (when (= saved-generation (get-generation))
                    (set! cached-data data) ; put it back, or put the fresh one here
                    (set! histogram-data hist)
                    (define snip (insert-plot-snip canvas (first (hist-axis data)) params rt))
                    (when snip (set-mouse-event-callback snip plot-hover-callback)))))))
            (begin
              (send canvas set-snip #f)
              (send canvas set-background-message "No params for plot")))))

    (define/override (save-plot-image file-name width height)
      ;; We assume the data is ready, and don't do anything if it is not.
      (let ((data cached-data)
            (params (send this get-chart-settings)))
        (when (and params histogram-data data)
          (let ((rt (make-renderer-tree histogram-data data params)))
            (when rt
              (save-plot-to-file file-name width height (first (hist-axis data)) params rt))))))

    ))
