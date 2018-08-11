#lang racket/base
;; inspect-scatter.rkt -- scatter plot for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require math/statistics
         plot/no-gui
         racket/class
         racket/gui/base
         racket/list
         racket/match
         racket/math
         racket/string
         "../data-frame/df.rkt"
         "../data-frame/scatter.rkt"
         "../data-frame/slr.rkt"
         "../plot-hack.rkt"
         "../plot-util.rkt"
         "../series-meta.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt")

(provide scatter-plot-panel%)

;; Filter AXIS-LIST to remove any axis definition that don't have a data
;; series in DF, a data-frame%
(define (filter-axis-list df axis-list)
  (define al
    (for/list ([axis axis-list]
               #:when
               (if (list? axis)
                   (let ()
                     (match-define (list name a1 a2) axis)
                     (df-contains? df (send a1 series-name) (send a2 series-name)))
                   (df-contains? df (send axis series-name))))
      axis))
  (sort al string<?
        #:key (lambda (a) (if (list? a) (first a) (send a headline)))))

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

;; Axis choices for all non lap swimming sports.  Any pair of axis from this
;; list is valid for the scatter plot.
(define default-axis-choices
  (list
   axis-distance
   axis-timer-time
   axis-elapsed-time
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
   axis-swim-distance
   axis-swim-time
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-stroke-length
   axis-swim-swolf
   axis-swim-pace))

;; Find the bounds of the DATA-SERIES. The bounds are rounded to X-DIGITS and
;; Y-DIGITS and made slightly larger so they fit all the data points nicely.
;; A vector of xmin, xmax, ymin, ymax is returned.
(define (find-bounds data-series x-digits y-digits)
  (define (good-or-false num)
    (and (number? num) (not (nan? num)) (not (infinite? num)) num))

  (define (round n digits)
    (* (exact-round (* n (expt 10 digits))) (expt 10 (- digits))))

  (let ((xmin #f)
        (xmax #f)
        (ymin #f)
        (ymax #f))
    (for ([item data-series])
      (define x (vector-ref item 0))
      (define y (vector-ref item 1))
      (set! xmin (if xmin (min xmin x) x))
      (set! xmax (if xmax (max xmax x) x))
      (set! ymin (if ymin (min ymin y) y))
      (set! ymax (if ymax (max ymax y) y)))

    ;; Round the min and max to the actual digits, so we don't cut them out in
    ;; case we have values along the boundary
    (set! xmin (and xmin (round xmin x-digits)))
    (set! xmax (and xmax (round xmax x-digits)))
    (set! ymin (and ymin (round ymin y-digits)))
    (set! ymax (and ymax (round ymax y-digits)))

    (define xrange (if (and xmin xmax) (- xmax xmin) #f))
    (define yrange (if (and ymin ymax) (- ymax ymin) #f))

    (when xrange
      (when xmin (set! xmin (- xmin (* xrange 0.05))))
      (when xmax (set! xmax (+ xmax (* xrange 0.05)))))
    (when yrange
      (when ymin (set! ymin (- ymin (* yrange 0.05))))
      (when ymax (set! ymax (+ ymax (* yrange 0.05)))))
    (vector
       (good-or-false xmin)
       (good-or-false xmax)
       (good-or-false ymin)
       (good-or-false ymax))))

;; NOTE: this is almost (but not quite) like the find-bounds/quantile from
;; inspect-quadrant.rkt, perhaps we could refactor it.
(define (find-bounds/quantile data-series q)
  (define (good-or-false num)
    (and (number? num) (not (nan? num)) (not (infinite? num)) num))
  (define x-data (make-vector (vector-length data-series) 0))
  (define y-data (make-vector (vector-length data-series) 0))
  (for ([(item index) (in-indexed data-series)])
    (match-define (vector x y _) item)
    (vector-set! x-data index x)
    (vector-set! y-data index y))
  (define xmin (quantile q < x-data))
  (define xmax (quantile (- 1 q) < x-data))
  (define ymin (quantile q < y-data))
  (define ymax (quantile (- 1 q) < y-data))
  (define xrange (if (and xmin xmax) (- xmax xmin) #f))
  (define yrange (if (and ymin ymax) (- ymax ymin) #f))
  (when xrange
    (when xmin (set! xmin (- xmin (* xrange 0.05))))
    (when xmax (set! xmax (+ xmax (* xrange 0.05)))))
  (when yrange
    (when ymin (set! ymin (- ymin (* yrange 0.05))))
    (when ymax (set! ymax (+ ymax (* yrange 0.05)))))
  (vector
   (good-or-false xmin)
   (good-or-false xmax)
   (good-or-false ymin)
   (good-or-false ymax)))

(define (extract-data data-frame x-axis y-axis)
  (let ((xnam (send x-axis series-name))
        (ynam (send y-axis series-name)))
    (when (df-contains? data-frame xnam ynam)
      (df-select* data-frame xnam ynam "elapsed" #:filter valid-only))))

;; Scatter plot state, contains data that is calculated in a separate thread
;; and passed to the plot routines.
(struct spstate (data                   ; data, as produced by `extract-data'
                 bounds                 ; bounds of the plot
                 qbounds                ; quantile bounds
                 rt)                    ; the render tree for the plot
  #:transparent)

(define empty-bounds (vector #f #f #f #f))
(define empty-spstate (spstate #f empty-bounds empty-bounds #f))

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

;; Compute linear regression parameters for DATA (a list of samples).  Note
;; that we compute this on the time delayed series.
(define (slr-params data)
  (define xs '())
  (define ys '())
  (for ([d data])
    (set! xs (cons (vector-ref d 0) xs))
    (set! ys (cons (vector-ref d 1) ys)))
  (make-slr xs ys))

;; Update a scatter plot state and return a new one. STATE is the old state,
;; if the data member is valid, data will not be extracted again). DF is the
;; data frame; XAXIS, YAXIS are the series meta data objects for the X and Y
;; axis; DELAY specifies the amount (in seconds) to delay the Y series), OPCT
;; is the outlier percentile, passed to `find-bounds/quantile'.  If ADD-LABEL?
;; is #t, a label will be added to the plot (this is useful to add things like
;; "Left Pedal", "Right Pedal" to dual plots).
(define (update-spstate state df xaxis yaxis delay opct (add-label? #f))
  (let ((ds (spstate-data state)))
    (unless ds (set! ds (extract-data df xaxis yaxis)))
    (let* ((x-digits (send xaxis fractional-digits))
           (y-digits (send yaxis fractional-digits))
           (color (send yaxis plot-color))
           (bounds (find-bounds ds x-digits y-digits))
           (qbounds (if opct
                        (find-bounds/quantile ds opct)
                        ;; use the default bounds, as they have a safety band
                        ;; around them!
                        bounds))
           (delayed (if delay (time-delay-series ds delay) ds))
           (grouped (group-samples delayed x-digits y-digits))
           (slr (slr-params delayed))
           (renderer (list
                      (scatter-group-renderer
                       grouped
                       #:color color
                       #:label (and add-label?
                                    (or (send yaxis plot-label)
                                        (send xaxis plot-label))))
                      (slr-renderer slr))))
      (spstate ds bounds qbounds renderer))))

(define scatter-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:scatter-plot)

    ;; Variables that control the look of the plot

    (define axis-choices '())
    (define x-axis-index 0)
    (define y-axis-index 0)
    (define delay-amount #f)
    (define outlier-percentile #f)
    (define outlier-handling 'mark)

    ;; Map a sport to an X-Y axis selection, to be restored when a similar
    ;; sport is selected.
    (define axis-by-sport (make-hash))

    ;; Map a delay value to an X-Y-Sport axsis selection, to be restored when
    ;; a similar axis choice is present.
    (define params-by-axis (make-hash))

    ;; Restore the preferences now, we do it so the controls can be
    ;; initialized with the correct values.
    (let ((pref (get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 2))
        (match-define (list abs dba) pref)
        (set! axis-by-sport (hash-copy abs))
        (set! params-by-axis (hash-copy dba))))

    ;; Root widget of the entire scatter plot panel
    (define panel
      (new (class vertical-panel% (init) (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image)))
           [parent parent] [border 5] [spacing 5]
           [alignment '(center top)]))

    ;;; Holds the widgets that control the look of the plot
    (define control-panel
      (new horizontal-panel%
           [parent panel] [spacing 10] [border 0]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define x-axis-choice
      (new choice% [parent control-panel] [choices '()] [min-width 300]
           [label "X Axis: "]
           [callback (lambda (c e) (on-x-axis-changed (send c get-selection)))]))

    (define y-axis-choice
      (new choice% [parent control-panel] [choices '()] [min-width 300]
           [label "Y Axis: "]
           [callback (lambda (c e) (on-y-axis-changed (send c get-selection)))]))

    (define delay-amount-field
      (new number-input-field% [parent control-panel]
           [label "Delay Amount: "] [cue-text "seconds"] [min-value 0]
           [stretchable-width #f]
           [valid-value-cb (lambda (v) (on-delay-amount v))]))

    (define outlier-percentile-field
      (new number-input-field% [parent control-panel]
           [label "Outlier Percentile (%): "] [cue-text "0..50%"]
           [min-value 0] [max-value 50]
           [stretchable-width #f]
           [valid-value-cb (lambda (v) (on-outlier-percentile v))]))

    (define outlier-handling-choice
      (new choice% [parent control-panel]
           [label ""] [choices '("Mark outliers" "Crop outliers")]
           [stretchable-width #f]
           [callback (lambda (c e) (on-outlier-handling (send c get-selection)))]))

    ;; Initialize the delay-amount and outlier percentile fields
    (if (number? delay-amount)
        (send delay-amount-field set-numeric-value delay-amount)
        (send delay-amount-field set-value ""))
    (if (number? outlier-percentile)
        (send outlier-percentile-field set-numeric-value outlier-percentile)
        (send outlier-percentile-field set-value ""))
    (send outlier-handling-choice set-selection
          (if (eq? outlier-handling 'mark) 0 1))

    ;; Pasteboard to hold the actual plot
    (define plot-pane (new horizontal-pane% [parent panel]))
    (define plot-left-pb (new snip-canvas% [parent plot-pane]))
    (define plot-right-pb (new snip-canvas% [parent plot-pane] [style '(deleted)]))

    ;;; Data from the session we inspect
    (define data-frame #f)
    ;; State for the plot on the left side, for single plots, this is the only
    ;; one displayed
    (define lstate empty-spstate)
    ;; State for the plot o the right side, for dual plots (e.g. when we have
    ;; left and right pedal data), this will be displayed in addition to the
    ;; LSTATE plot.
    (define rstate empty-spstate)
    (define inhibit-refresh #f)         ; when #t, refresh-plot will do nothing
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define export-file-name #f)

    (define (current-sport)
      (if data-frame (df-get-property data-frame 'sport) #f))

    ;; get the label of the axis at INDEX.  This is compicated by the fact
    ;; that some entries in AXIS-CHOICES are dual axes.
    (define (axis-label index)
      (if (and (>= index 0) (< index (length axis-choices)))
          (let ((axis (list-ref axis-choices index)))
            (if (list? axis)
                (string-join
                 (map (lambda (m) (send m series-name)) (cdr axis))
                 "+")
                (send axis series-name)))
          #f))

    (define (invalidate-data)
      (set! lstate empty-spstate)
      (set! rstate empty-spstate)
      (refresh-plot))

    ;; Update the axis selection check-boxes with AXIS-LIST
    (define (install-axis-choices axis-list)
      (send y-axis-choice clear)
      (send x-axis-choice clear)
      (for ([a axis-list])
        (let ((n (if (list? a) (car a) (send a axis-label))))
          (send x-axis-choice append n)
          (send y-axis-choice append n))))

    (define (on-x-axis-changed new-index)
      (unless (equal? x-axis-index new-index)
        (save-params-for-axis)
        (set! x-axis-index new-index)
        (set! export-file-name #f)
        (restore-params-for-axis)
        (invalidate-data)))

    (define (on-y-axis-changed new-index)
      (unless (equal? y-axis-index new-index)
        (save-params-for-axis)
        (set! y-axis-index new-index)
        (set! export-file-name #f)
        (restore-params-for-axis)
        (invalidate-data)))

    (define (on-delay-amount amount)
      (when (eq? amount 'empty)
        (set! amount #f))
      (unless (equal? delay-amount amount)
        (set! delay-amount amount)
        ;; no need to invalidate the data
        (refresh-plot)))

    (define (on-outlier-percentile percentile)
      (when (eq? percentile 'empty)
        (set! percentile #f))
      (unless (equal? percentile outlier-percentile)
        (set! outlier-percentile percentile)
        ;; no need to invalidate the data
        (refresh-plot)))

    (define (on-outlier-handling choice)
      (if (eq? choice 0)
          (set! outlier-handling 'mark)
          (set! outlier-handling 'crop))
      ;; No need to refresh the plot data at all, just rebuild
      (put-plot-snip))

    ;; Prepare the plot snip and insert it into the pasteboard. Assumes the
    ;; render tree is ready (if it is #f, there is no data for the plot).
    (define (put-plot-snip)

      ;; Get the quantile bounds of the plot, takes into consideration dual
      ;; plots.  If we have dual plots, we use the same bounds, so that the
      ;; two plots can be compared directly.
      (define (qbounds)
        (if (and (spstate-rt lstate) (spstate-rt rstate))
            (union-bounds (spstate-qbounds lstate)
                          (spstate-qbounds rstate))
            (spstate-qbounds lstate)))

      ;; Get the bounds of the plot, takes into consideration dual plots.  If
      ;; we have dual plots, we use the same bounds, so that the two plots can
      ;; be compared directly.
      (define (bounds)
        (if (and (spstate-rt lstate) (spstate-rt rstate))
            (union-bounds (spstate-bounds lstate)
                          (spstate-bounds rstate))
            (spstate-bounds lstate)))

      ;; Return the axis at INDEX.  LEFT-OR-RIGHT indicates which axis we
      ;; prefer for dual axis plots.
      (define (get-axis index left-or-right)
        (let ((axis (list-ref axis-choices index)))
          (if (list? axis)
              (if (eq? left-or-right 'left) (list-ref axis 1) (list-ref axis 2))
              axis)))

      (when (spstate-rt lstate)
        (let ((rt (list (tick-grid) (spstate-rt lstate))))
          (when (eq? outlier-handling 'mark)
            (match-define (vector left right low high) (qbounds))
            (when left
              (set! rt (cons (vrule left #:color "blue" #:style 'short-dash) rt)))
            (when right
              (set! rt (cons (vrule right #:color "blue" #:style 'short-dash) rt)))
            (when low
              (set! rt (cons (hrule low #:color "blue" #:style 'short-dash) rt)))
            (when high
              (set! rt (cons (hrule high #:color "blue" #:style 'short-dash) rt))))
          (let ((x-axis (get-axis x-axis-index 'left))
                (y-axis (get-axis y-axis-index 'left)))
            (parameterize ([plot-x-ticks (send x-axis plot-ticks)]
                           [plot-x-label (send x-axis axis-label)]
                           [plot-y-ticks (send y-axis plot-ticks)]
                           [plot-y-label (send y-axis axis-label)])
              (match-define (vector x-min x-max y-min y-max)
                (if (eq? outlier-handling 'mark) (bounds) (qbounds)))
              (plot-snip/hack plot-left-pb rt
                              #:x-min x-min #:x-max x-max
                              #:y-min y-min #:y-max y-max))))

        ;; Right side plot is never active by itself, so this WHEN clause is
        ;; inside the LSTATE one.
        (when (spstate-rt rstate)
          (let ((rt (list (tick-grid) (spstate-rt rstate))))
          (when (eq? outlier-handling 'mark)
            (match-define (vector left right low high) (qbounds))
            (when left
              (set! rt (cons (vrule left #:color "blue" #:style 'short-dash) rt)))
            (when right
              (set! rt (cons (vrule right #:color "blue" #:style 'short-dash) rt)))
            (when low
              (set! rt (cons (hrule low #:color "blue" #:style 'short-dash) rt)))
            (when high
              (set! rt (cons (hrule high #:color "blue" #:style 'short-dash) rt))))
          (let ((x-axis (get-axis x-axis-index 'right))
                (y-axis (get-axis y-axis-index 'right)))
            (parameterize ([plot-x-ticks (send x-axis plot-ticks)]
                           [plot-x-label (send x-axis axis-label)]
                           [plot-y-ticks (send y-axis plot-ticks)]
                           [plot-y-label (send y-axis axis-label)])
              (match-define (vector x-min x-max y-min y-max)
                (if (eq? outlier-handling 'mark) (bounds) (qbounds)))
              (plot-snip/hack plot-right-pb rt
                              #:x-min x-min #:x-max x-max
                              #:y-min y-min #:y-max y-max)))))))

    ;; Build a plot render tree (PLOT-RT) based on current selections.  Note
    ;; that procesing happens in a separate task, and the render tree will
    ;; become available at a later time.  Once the new render tree is
    ;; available, it will be automatically inserted into the pasteboard.
    (define (refresh-plot)
      (unless inhibit-refresh
        (send plot-left-pb set-background-message "Working...")
        (send plot-right-pb set-background-message "Working...")
        (send plot-left-pb set-snip #f)
        (send plot-right-pb set-snip #f)
        ;; Capture all relavant vars, as we are about to queue up a separate
        ;; task
        (let* ((x (list-ref axis-choices x-axis-index))
               (y (list-ref axis-choices y-axis-index))
               (df data-frame)
               (old-lstate lstate)
               (old-rstate rstate)
               (damt delay-amount)
               (opct (if outlier-percentile (/ outlier-percentile 100.0) #f))
               (dual? (or (list? x) (list? y)))) ; #t if we have two plots

          (send plot-pane change-children
                (lambda (old)
                  (if dual? (list plot-left-pb plot-right-pb) (list plot-left-pb))))

          (queue-task
           "inspect-scatter%/refresh-plot"
           (lambda ()
             (if dual?
                 (let ((x-left (if (list? x) (list-ref x 1) x))
                       (y-left (if (list? y) (list-ref y 1) y))
                       (x-right (if (list? x) (list-ref x 2) x))
                       (y-right (if (list? y) (list-ref y 2) y)))
                   (let ((new-lstate (update-spstate old-lstate df x-left y-left damt opct #t))
                         (new-rstate (update-spstate old-rstate df x-right y-right damt opct #t)))
                     (queue-callback
                      (lambda ()
                        (set! lstate new-lstate)
                        (set! rstate new-rstate)
                        (if (spstate-rt lstate)
                            (put-plot-snip)
                            (send plot-left-pb set-background-message "No data to plot"))))))
                 ;; Single plot
                 (let ((new-lstate (update-spstate old-lstate df x y damt opct)))
                   (queue-callback
                    (lambda ()
                      (set! lstate new-lstate)
                      (set! rstate empty-spstate)
                      (if (spstate-rt lstate)
                          (put-plot-snip)
                          (send plot-left-pb set-background-message "No data to plot")))))))))))

    ;; Store the plot parameters for the current sport, this includes axis
    ;; selection and the parameters for the current axis selection.
    (define (save-params-for-sport)
      (when (current-sport)
        (save-params-for-axis)
        (let ((sport (current-sport))
              (x-name (axis-label x-axis-index))
              (y-name (axis-label y-axis-index)))
          (hash-set! axis-by-sport sport (list x-name y-name)))))

    ;; Save the parameters for the currently selected axis combination
    (define (save-params-for-axis)
      (when (current-sport)
        (let ((sport (current-sport))
              (x-name (axis-label x-axis-index))
              (y-name (axis-label y-axis-index)))
          (hash-set! params-by-axis (list sport x-name y-name)
                     (hash
                      'delay-amount delay-amount
                      'outlier-percentile outlier-percentile
                      'outlier-handling outlier-handling)))))

    ;; Restore parameters for rhe current sport.  This assumes that a new
    ;; sport (data frame) was installed, it will set the axis selection and
    ;; axis parameters to what was uses last for the same sport.
    (define (restore-params-for-sport)
      (when (current-sport)
        (let ((data (hash-ref axis-by-sport (current-sport) (lambda () (list 0 0)))))
          (match-define (list x-name y-name) data)
          (set! x-axis-index (or (find-axis x-name axis-choices) 0))
          (set! y-axis-index (or (find-axis y-name axis-choices) 0)))
        (when (> (send x-axis-choice get-number) x-axis-index)
          (send x-axis-choice set-selection x-axis-index))
        (when (> (send y-axis-choice get-number) y-axis-index)
          (send y-axis-choice set-selection y-axis-index))
        (set! export-file-name #f)
        (restore-params-for-axis)))

    ;; Restore parameters for the current axis selection.  This assumes a new
    ;; axis was selected and will set the parameters for that axis
    ;; combination.
    (define (restore-params-for-axis)
      (when (current-sport)
        (let* ((sport (current-sport))
               (x-name (axis-label x-axis-index))
               (y-name (axis-label y-axis-index))
               (sport-data (hash-ref params-by-axis (list sport x-name y-name) #f)))
          (if (hash? sport-data)
              (begin
                (set! delay-amount (hash-ref sport-data 'delay-amount #f))
                (set! outlier-percentile (hash-ref sport-data 'outlier-percentile #f))
                (set! outlier-handling (hash-ref sport-data 'outlier-handling 'mark)))
              (begin
                (set! delay-amount #f)
                (set! outlier-percentile #f)
                (set! outlier-handling 'mark))))
        (if (number? delay-amount)
            (send delay-amount-field set-numeric-value delay-amount)
            (send delay-amount-field set-value ""))
        (if (number? outlier-percentile)
            (send outlier-percentile-field set-numeric-value outlier-percentile)
            (send outlier-percentile-field set-value ""))
        (send outlier-handling-choice set-selection (if (eq? outlier-handling 'mark) 0 1))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (let ((data (list axis-by-sport params-by-axis)))
        (put-pref pref-tag data)))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define (get-default-export-file-name)
      (or export-file-name
          (let ((sid (df-get-property data-frame 'session-id))
                (x-axis (list-ref axis-choices x-axis-index))
                (y-axis (list-ref axis-choices y-axis-index)))
            (cond ((and sid x-axis y-axis)
                   (format "scatter-~a-~a-~a.png" sid
                           (send x-axis series-name)
                           (send y-axis series-name)))
                  (#t
                   "scatter.png")))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (get-default-export-file-name) "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! export-file-name file)
          ;; TODO: this needs to be fixed for dual plots
          (send plot-left-pb export-image-to-file file))))

    (define/public (set-session session df)
      (set! inhibit-refresh #f)
      (save-params-for-sport)
      (set! data-frame df)
      (set! delay-amount #f)
      (set! outlier-percentile #f)
      (set! outlier-handling 'mark)
      (set! export-file-name #f)
      (define lap-swimming? (df-get-property data-frame 'is-lap-swim?))
      (set! axis-choices
            (filter-axis-list
             data-frame
             (if lap-swimming? swim-axis-choices default-axis-choices)))
      (install-axis-choices axis-choices)
      (restore-params-for-sport)
      (send delay-amount-field enable (not lap-swimming?))
      (set! inhibit-refresh #f)
      (invalidate-data))

    ))
