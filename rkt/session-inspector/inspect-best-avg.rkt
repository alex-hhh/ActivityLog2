#lang racket/base
;; inspect-mean-max.rkt -- mean-max plot view for a session.  This is not
;; supported for swimming activites.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db/base
         plot/no-gui
         racket/class
         racket/date
         racket/gui/base
         racket/list
         racket/match
         "../utilities.rkt"
         "../session-df/native-series.rkt"
         "../data-frame/meanmax.rkt"
         "../data-frame/df.rkt"
         "../dbapp.rkt"
         "../metrics.rkt"
         "../bavg-util.rkt"
         "../pdmodel.rkt"
         "../fmt-util.rkt"
         "../plot-util.rkt"
         "../database.rkt")

(provide mean-max-plot-panel%)

;; Filter AXIS-LIST to remove any axis definition that don't have a data
;; series in DF, a data-frame%
(define (filter-axis-list df axis-list)

  (define (valid? axis)
    (if (list? axis)
        (let ()
          (match-define (list name a1 a2) axis)
          (df-contains? df (send a1 series-name) (send a2 series-name)))
        (df-contains? df (send axis series-name))))

  (define (sort-key axis)
    (if (list? axis) (first axis) (send axis headline)))

  (sort (filter valid? axis-list) string<? #:key sort-key))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)

  (define (match? axis)
    (let ((sn (if (list? axis)
                  (car axis)
                  (send axis series-name))))
      (equal? series-name sn)))

  (for/first ([(axis index) (in-indexed axis-list)]
              #:when (match? axis))
    index))

;; Axis choices for all non lap swimming sports.  note that some axis choices
;; don't make sense, so they are not listed here, to keep the list smaller.
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

;; Return the start of today as a UNIX timestamp (in seconds)
(define (this-day-start)
  (let ((now (current-date)))
    (date->seconds
     (date 0 0 0
           (date-day now) (date-month now) (date-year now)
           0 0 0 (date-time-zone-offset now)))))

;; Default time periods for the "Show bests" plot.
(define default-periods
  (list
   (list "None" (lambda () #f))
   (list "Last Month"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 30 24 3600)) (+ end (* 24 3600))))))
   (list "Last 6 Weeks"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 42 24 3600)) (+ end (* 24 3600))))))
   (list "Last 3 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 90 24 3600)) (+ end (* 24 3600))))))
   (list "Last 6 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 180 24 3600)) (+ end (* 24 3600))))))
   (list "Last 12 Months"
         (lambda ()
           (let ((end (this-day-start)))
             (cons (- end (* 365 24 3600)) (+ end (* 24 3600))))))
   (list "All Time"
         (lambda ()
           (let ((end (this-day-start)))
             (cons #f #f))))))

;; Return a list of time intervals for each defined season in the database.
;; The list is in the same format as `default-periods'
(define (make-periods-from-seasons db)
  (for/list (((name start end)
              (in-query db "select name, start_date, end_date from SEASON order by start_date")))
    (list (format "~a season" name) (lambda () (cons start end)))))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-period name period-list)

  (define (match? ti) (equal? name (first ti)))

  (for/first ([(ti index) (in-indexed period-list)]
              #:when (match? ti))
    index))

;; Return a list of session id based that match SPORT, a (Vectorof sport-id
;; sub-sport-id) and TIME-INTERVAL, a (Cons start end).
(define (get-candidate-sessions db sport period)
  (if period
      (match-let (((vector sport-id sub-sport-id) sport)
                  ((cons start end) period))
        (fetch-candidate-sessions db sport-id sub-sport-id start end))
      '()))

;; Return suitable plot bounds for a mean-max plor, considering all input
;; data.  Returns four values: MIN-X, MAX-X, MIN-Y, MAX-Y.
(define (plot-bounds axis zero-base? mean-max-data bests-data)
  (let-values (((min-x max-x min-y max-y) (get-mean-max-bounds mean-max-data))
               ((bmin-x bmax-x bmin-y bmax-y) (aggregate-mmax-bounds bests-data)))
    (let ((inverted? (send axis inverted-mean-max?)))
      (values
       min-x
       max-x
       (if zero-base? 0 (if inverted? (if bmin-y (min bmin-y min-y) min-y) min-y))
       (if (not inverted?) (if bmax-y (max bmax-y max-y) max-y) max-y)
       ))))

;; Create a CP2 structure, containing critical power information) from the
;; data frame DF.  This is used to plot the critical power curve for the
;; respective axis.
;;
;; Return #f if no critical power data is present in the data frame.
(define (make-cp2-from-df df)
  (let ((cp (df-get-property df 'critical-power))
        (wprime (df-get-property df 'wprime)))
    (if (and cp wprime)
        (cp2 cp wprime (make-cp-fn cp wprime) #f #f #f)
        #f)))

(define mean-max-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:mean-max-plot)

    (define axis-choices '())
    (define period-choices '())

    (define selected-axis 0)
    (define selected-aux-axis 0)
    (define selected-period 0)
    (define zero-base? #f)

    ;; Store plot parameters by sport type, to be restored when a session from
    ;; the same sport is used.  See `save-params-for-sport',
    ;; 'restore-params-for-sport'.
    ;;
    ;; maps SPORT => params-by-series hash
    (define params-by-sport (make-hash))

    ;; Store plot parameters for each selected series (what AUX axis and what
    ;; Bests period to show).  See `save-params-for-series',
    ;; 'restore-params-for-series'
    ;;
    ;; maps SERIES NAME -> (List AUX-SERIES-NAME BESTS-PERIOD-NAME ZERO-BASE?)
    (define params-by-series (make-hash))

    ;; Restore the saved preferences now.
    (let ((pref (get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 1))
        (set! params-by-sport (hash-copy (first pref)))))

    ;; Root widget of the entire scatter plot panel
    (define panel
      (new (class vertical-panel%
             (init) (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image))
             (define/public (interactive-export-data formatted?)
               (on-interactive-export-data formatted?)))
           [parent parent] [border 5] [spacing 5]
           [alignment '(center top)]))

    ;; Holds the widgets that control the look of the plot
    (define control-panel
      (new horizontal-panel%
           [parent panel] [spacing 10] [border 0]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define axis-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Best Avg: "]
           [callback (lambda (c e) (on-axis-changed (send c get-selection)))]))

    (define aux-axis-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Auxiliary: "]
           [callback (lambda (c e) (on-aux-axis-changed (send c get-selection)))]))

    (define period-choice-box
      (new choice% [parent control-panel] [choices '()]
           [min-width 300] [label "Show Best: "]
           [callback (lambda (c e) (on-period-changed (send c get-selection)))]))

    (define zero-base-check-box
      (new check-box% [parent control-panel]
           [value zero-base?] [label "Zero Base"]
           [callback (lambda (c e) (on-zero-base (send c get-value)))]))

    ;; Pasteboard to display the actual MEAN-MAX plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;; Graph data
    (define data-frame #f)
    (define cp-data #f)
    (define mean-max-data '())
    (define mean-max-plot-fn #f)
    (define mean-max-aux-data '())
    (define mean-max-aux-plot-fn #f)
    (define mean-max-aux-invfn #f)
    (define mean-max-pd-fn #f)
    (define inhibit-refresh 0)
    (define plot-rt #f)
    ;; The plot render tree for the "bests" plot
    (define best-rt #f)
    (define best-fn #f)
    (define best-rt-generation 0)
    (define bests-data '())
    (define data-cache (make-hash))
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define img-export-file-name #f)
    (define data-export-file-name #f)

    (define pd-model-snip #f)
    (define saved-pd-model-snip-location #f)

    (define (current-sport)
      (if data-frame (df-get-property data-frame 'sport) #f))

    (define (get-series-axis)
      (list-ref axis-choices selected-axis))

    (define (get-aux-axis)
      (and (> selected-aux-axis 0)
           (list-ref axis-choices (sub1 selected-aux-axis))))

    (define (get-best-rt-generation) best-rt-generation)

    (define (install-axis-choices)
      (let ((alist (if (df-get-property data-frame 'is-lap-swim?)
                       swim-axis-choices
                       default-axis-choices)))
        (set! axis-choices (filter-axis-list data-frame alist)))
      (send axis-choice-box clear)
      (send aux-axis-choice-box clear)
      (send aux-axis-choice-box append "None")
      (for ([a axis-choices])
        (let ((n (send a axis-label)))
          (send axis-choice-box append n)
          (send aux-axis-choice-box append n))))

    (define (install-period-choices)
      (set! period-choices
            (append default-periods
                    (make-periods-from-seasons (current-database))))
      (send period-choice-box clear)
      (for ([t period-choices])
        (send period-choice-box append (car t))))

    (define (on-period-changed new-index)
      (unless (equal? selected-period new-index)
        (set! selected-period new-index)
        (refresh-bests-plot)))

    ;; When 'dont-save-previous' is #t, previous axis params are not saved
    ;; before setting up the new axis.  This is used when
    ;; 'restore-params-for-sport' installs the new axis and there is no
    ;; previous axis to save (otherwise the selected axis params will be
    ;; crossing from one sport to the other).
    (define (on-axis-changed new-index (dont-save-previous #f))
      (unless (equal? selected-axis new-index)
        (unless dont-save-previous
          (save-params-for-series))
        (set! selected-axis new-index)
        (set! img-export-file-name #f)
        (set! data-export-file-name #f)
        (restore-params-for-series)
        (refresh-bests-plot)
        (refresh-plot)))

    (define (on-aux-axis-changed new-index)
      (unless (equal? selected-aux-axis new-index)
        (set! selected-aux-axis new-index)
        (set! img-export-file-name #f)
        (set! data-export-file-name #f)
        (refresh-plot)))

    (define (on-zero-base flag)
      (unless (equal? zero-base? flag)
        (set! zero-base? flag)
        (refresh-plot)))

    (define (plot-hover-callback snip event x y)
      (define info '())
      (define renderers '())
      (define markers '())
      (define (add-renderer r) (set! renderers (cons r renderers)))

      (define (add-info tag value)
        (set! info (cons (list tag value) info)))

      (define (add-data-point name yfn format-fn)
        (when yfn
          (let ((py (yfn x)))
            (when py
              (add-info name (format-fn py))
              (set! markers (cons (vector x py) markers))))))

      (when (good-hover? x y event)
        (add-renderer (pu-vrule x))

        ;; The aux values need special treatment: they are scaled to match the
        ;; main axis coordinate system, this works for the plot itself, but we
        ;; need to convert the value back for display.
        (when (and mean-max-aux-plot-fn mean-max-aux-invfn)
          (let* ((ay (mean-max-aux-plot-fn x))
                 (aux-axis (get-aux-axis))
                 (format-value (send aux-axis value-formatter)))
            (when ay
              (let ((actual-ay ((invertible-function-f mean-max-aux-invfn) ay)))
                (add-info (send aux-axis name) (format-value actual-ay))
                (set! markers (cons (vector x ay) markers))))))

        (define axis (get-series-axis))
        (define format-value (send axis value-formatter))
        (add-data-point "Model" mean-max-pd-fn format-value)

        ;; Find the closest point on the bests plot and put the date on which
        ;; it was achieved.  Technically, the hover will be between two such
        ;; measurements, but for simplicity we show only the one that it is
        ;; closest to the mouse.  The trends-mmax plot shows both points.
        (when bests-data
          (let ((closest (lookup-duration/closest bests-data x)))
            (when closest
              (define sid (car closest))
              (add-info #f (date-time->string (get-session-start-time sid))))))
        (add-data-point "Best" best-fn format-value)
        (add-data-point (send axis name) mean-max-plot-fn format-value)
        (add-info "Duration" (duration->string x))
        (unless (empty? info)
          (add-renderer (pu-markers markers))
          (add-renderer (pu-label x y (make-hover-badge (reverse info))))))

      (set-overlay-renderers snip renderers))


    (define (put-plot-snip)
      (when plot-rt
        (let ((rt (list (tick-grid) plot-rt)))
          (when best-rt
            (set! rt (cons best-rt rt)))
          (let ((mean-max-axis (get-series-axis))
                (aux-axis (get-aux-axis))
                ;; get the location of the pd-model-snip here, it will be lost
                ;; once we insert a new plot in the canvas.
                (saved-location (get-snip-location pd-model-snip)))
            (let-values (((min-x max-x min-y max-y) (plot-bounds (get-series-axis) zero-base? mean-max-data bests-data)))
              ;; aux data might not exist, if an incorrect/invalid aux-axis is
              ;; selected
              (if (> (length mean-max-aux-data) 0)
                  (let ((ivs (mk-invertible-function mean-max-aux-data mean-max-data zero-base?)))
                    (set! mean-max-aux-invfn ivs)
                    (parameterize ([plot-x-ticks (mean-max-ticks)]
                                   [plot-x-label "Duration"]
                                   [plot-x-transform log-transform]
                                   [plot-x-tick-label-anchor 'top-right]
                                   [plot-x-tick-label-angle 30]
                                   [plot-y-ticks (send mean-max-axis plot-ticks)]
                                   [plot-y-label (send mean-max-axis axis-label)]
                                   [plot-y-far-ticks (ticks-scale (send aux-axis plot-ticks) ivs)]
                                   [plot-y-far-label (send aux-axis axis-label)])
                      (define snip (plot-to-canvas rt plot-pb
                                                   #:x-min min-x #:x-max max-x
                                                   #:y-min min-y #:y-max max-y
                                                   ))
                      (set-mouse-event-callback snip plot-hover-callback)))
                  (parameterize ([plot-x-ticks (mean-max-ticks)]
                                 [plot-x-label "Duration"]
                                 [plot-x-transform log-transform]
                                 [plot-x-tick-label-anchor 'top-right]
                                 [plot-x-tick-label-angle 30]
                                 [plot-y-ticks (send mean-max-axis plot-ticks)]
                                 [plot-y-label (send mean-max-axis axis-label)])
                    (define snip (plot-to-canvas rt plot-pb
                                                 #:x-min min-x #:x-max max-x
                                                 #:y-min min-y #:y-max max-y))
                    (set-mouse-event-callback snip plot-hover-callback)))
              (when (and cp-data (send mean-max-axis have-cp-estimate?) mean-max-data)
                ;; NOTE: this is inefficient, as the plot-fn is already
                ;; computed in the `mean-max-renderer` and we are computing it
                ;; here a second time.
                (let* ((fn (mean-max->spline mean-max-data))
                       (pict (send mean-max-axis pd-data-as-pict cp-data fn)))
                  (set! pd-model-snip (new pict-snip% [pict pict]))
                  (send plot-pb set-floating-snip pd-model-snip)
                  (move-snip-to pd-model-snip (or saved-location saved-pd-model-snip-location)))))))))

    (define (refresh-plot)
      (set! saved-pd-model-snip-location
            (or (get-snip-location pd-model-snip)
                saved-pd-model-snip-location))
      (set! plot-rt #f)
      (set! pd-model-snip #f)
      (send plot-pb set-background-message "Working...")
      (send plot-pb set-snip #f)
      (unless (> inhibit-refresh 0)
        ;; Capture all needed data, as we will work in a different thread.
        (let ((df data-frame)
              (cp cp-data)
              (cache data-cache)
              (axis (get-series-axis))
              (aux-axis (get-aux-axis))
              (zerob? zero-base?)
              (renderer-tree '()))
          (queue-task
           "inspect-mean-max%/refresh-plot"
           (lambda ()
             (define data
               (or (hash-ref cache axis #f)
                   (get-session-mmax df axis)))
             (hash-set! cache axis data)
             ;; rebuild auxiliary data here
             (define aux-data
               (if aux-axis
                   (let ((series (send aux-axis series-name)))
                     (and (df-contains? df series)
                          (df-mean-max-aux df series data)))
                   '()))
             (when (> (length data) 0)  ; might not have any data points at all
               (set! renderer-tree
                     (cons
                      (mean-max-renderer
                       data aux-data
                       #:color1 (send axis plot-color)
                       #:color2 (and aux-axis (send aux-axis plot-color))
                       #:zero-base? zerob?)
                      renderer-tree)))
             (define pd-function
               (if (and cp (send axis have-cp-estimate?))
                   (send axis pd-function cp)
                   #f))
             (when pd-function
               (set! renderer-tree
                     (cons (function pd-function #:color "red" #:width 1.5 #:style 'long-dash)
                           renderer-tree)))
             ;; NOTE: these are already calculated in mean-max-renderer!
             (define plot-fn (mean-max->spline data))
             (define aux-plot-fn (mean-max->spline (normalize-aux aux-data data zerob?)))
             (queue-callback
              (lambda ()
                (cond ((not (null? renderer-tree))
                       (set! mean-max-data data)
                       (set! mean-max-aux-data aux-data)
                       (set! mean-max-plot-fn plot-fn)
                       (set! mean-max-aux-plot-fn aux-plot-fn)
                       (set! mean-max-pd-fn pd-function)
                       (set! plot-rt renderer-tree)
                       (set! cache data-cache)
                       (put-plot-snip))
                      (#t
                       (send plot-pb set-background-message "No data to plot"))))))))))

    (define (refresh-bests-plot)
      (define debug-tag "inspect-mean-max%/refresh-bests-plot")
      (unless (> inhibit-refresh 0)
        (set! best-rt-generation (add1 best-rt-generation))
        (set! best-rt #f)
        (let ((axis (get-series-axis))
              (generation best-rt-generation)
              (time-interval selected-period))
          (queue-task
           debug-tag
           (lambda ()
             (define candidates
               (get-candidate-sessions
                (current-database)
                (df-get-property data-frame 'sport)
                ((second (list-ref period-choices time-interval)))))

             (define inverted? (send axis inverted-mean-max?))
             (define mmax (get-aggregate-mmax candidates axis #f))

             (define fn (aggregate-mmax->spline-fn mmax))
             (define brt
               (and fn
                    (function-interval
                     ;; The bottom (or top) of the shaded area needs to cover
                     ;; the entire plot and it is too difficult to find out
                     ;; the true min/max X value of the plot, so we just put
                     ;; what we hope are large enough values.  The plot will
                     ;; be clipped at the right spot.
                     (if inverted? (lambda (x) 10000) (lambda (x) -10000))
                     fn
                     #:color (send axis plot-color)
                     #:alpha 0.1
                     #:line2-color "black"
                     #:line2-width 0.5
                     #:line1-style 'transparent)))
             (queue-callback
              (lambda ()
                ;; Discard changes if there was a new request since ours was
                ;; submitted.
                (let ((current-generation (get-best-rt-generation)))
                  (when (= generation current-generation)
                    (set! best-rt brt)
                    (set! best-fn fn)
                    (set! bests-data mmax)
                    (put-plot-snip))))))))))

    (define (save-params-for-sport)
      (when (current-sport)
        (save-params-for-series)
        (let ((axis (get-series-axis)))
          (hash-set! params-by-sport
                     (current-sport)
                     (list 'gen1 (send axis series-name) params-by-series)))))

    (define (restore-params-for-sport)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (let ((params (hash-ref params-by-sport (current-sport) #f)))
        (if (and params (eq? (car params) 'gen1))
            (match-let (((list tag series-name pbs) params))
              (let ((selection 0))
                (when series-name
                  (let ((index (find-axis series-name axis-choices)))

                    (set! selection (min (or index 0) (sub1 (length axis-choices))))))
                (on-axis-changed selection #t)
                (set! params-by-series (hash-copy pbs))))
            (begin
              (on-axis-changed 0 #t)
              (set! params-by-series (make-hash))))
        (send axis-choice-box set-selection selected-axis)
        (restore-params-for-series)
        (set! inhibit-refresh (sub1 inhibit-refresh))))

    (define (save-params-for-series)
      (when (current-sport)
        (let ((axis (get-series-axis))
              (aux-axis (get-aux-axis))
              (period (list-ref period-choices selected-period)))
          (hash-set!
           params-by-series
           (send axis series-name)
           (hash
            'aux-series-name (and aux-axis (send aux-axis series-name))
            'period-name (first period)
            'zero-base? zero-base?
            'pd-model-snip-location (get-snip-location pd-model-snip))))))

    (define (restore-params-for-series)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (let* ((axis (get-series-axis))
             (series-name (send axis series-name))
             (params (hash-ref params-by-series series-name #f)))
        (if (and params (hash? params))
            (begin
              (let ((aux-series-name (hash-ref params 'aux-series-name #f))
                    (selection 0))
                (when aux-series-name
                  (let ((index (find-axis aux-series-name axis-choices)))
                    (set! selection (add1 (min (or index 0) (sub1 (length axis-choices)))))))
                (on-aux-axis-changed selection))
              (let ((period-name (hash-ref params 'period-name #f))
                    (selection 0))
                (when period-name
                  (let ((index (find-period period-name period-choices)))
                    (set! selection (min (or index 0) (sub1 (length period-choices))))))
                (on-period-changed selection))
              (let ((zb? (hash-ref params 'zero-base? #f)))
                (on-zero-base zb?))
              (set! saved-pd-model-snip-location
                    (hash-ref params 'pd-model-snip-location #f)))
            (begin
              (on-aux-axis-changed 0)
              (on-period-changed 0))))
      (send aux-axis-choice-box set-selection selected-aux-axis)
      (send period-choice-box set-selection selected-period)
      (send zero-base-check-box set-value zero-base?)
      (set! inhibit-refresh (sub1 inhibit-refresh)))

    (define/public (save-visual-layout)
      (when (> (length axis-choices) 0)
        (save-params-for-sport)
        (put-pref pref-tag (list params-by-sport))))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define (get-default-export-file-name (extenstion "png"))
      (let ((sid (df-get-property data-frame 'session-id))
            (axis1 (get-series-axis))
            (axis2 (get-aux-axis)))
        (cond ((and sid axis1 axis2)
               (format "mean-max-~a-~a-~a.~a" sid
                       (send axis1 series-name)
                       (send axis2 series-name)
                       extenstion))
              ((and sid axis1)
               (format "mean-max-~a-~a.~a" sid
                       (send axis1 series-name)
                       extenstion))
              (#t
               (format "mean-max.~a" extenstion)))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (or img-export-file-name (get-default-export-file-name "png"))
                            "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! img-export-file-name file)
          (send plot-pb export-image-to-file file))))

    (define/public (on-interactive-export-data formatted?)
      (let ((file (put-file "Select file to export to" #f #f
                            (or data-export-file-name (get-default-export-file-name "csv"))
                            "csv" '()
                            '(("CSV Files" "*.csv") ("Any" "*.*")))))
        (when file
          (set! data-export-file-name file)
          (call-with-output-file file
            (lambda (out) (export-data-as-csv out formatted?))
            #:mode 'text #:exists 'truncate))))

    (define (export-data-as-csv out formatted?)
      (define have-aux? (and mean-max-aux-data (= (length mean-max-aux-data) (length mean-max-data))))
      (define have-bests? (and bests-data (<= (length mean-max-data) (length bests-data))))
      (write-string "Timestamp, Duration, Value" out)
      (when have-aux? (write-string ", Aux Value" out))
      (when have-bests? (write-string ", Best SID, Best Timestamp, Best Value" out))
      (newline out)
      (for ((index (in-range (length mean-max-data))))
        (match-define (vector d m s)
          (list-ref mean-max-data index))
        (if m
            (write-string (format "~a, ~a, ~a"
                                  (exact->inexact s)
                                  (exact->inexact d)
                                  (exact->inexact m))
                          out)
            (write-string (format ", , ") out))
        (when have-aux?
          (match-define (vector d m s)
            (list-ref mean-max-aux-data index))
          (if m
              (write-string (format ", ~a" (exact->inexact m)) out)
              (write-string ", " out)))
        (when have-bests?
          (match-define (list sid ts d m)
            (list-ref bests-data index))
          (format "bests: ~a ~a ~a ~a~%" sid ts d m)
          (if m
              (write-string (format ", ~a, ~a, ~a" sid (exact->inexact ts) (exact->inexact m)) out)
              (write-string (format ", , ,") out)))
        (newline out)))

    (define/public (set-session s df)
      (set! inhibit-refresh (add1 inhibit-refresh))
      (save-params-for-sport)
      (set! data-frame df)
      (set! cp-data (make-cp2-from-df df))
      (set! data-cache (make-hash))
      (set! img-export-file-name #f)
      (set! data-export-file-name #f)
      (set! plot-rt #f)
      (set! best-rt #f)
      (install-axis-choices)
      (if (> (length axis-choices) 0)
          (begin
            (install-period-choices)
            (restore-params-for-sport)
            (set! inhibit-refresh (sub1 inhibit-refresh))
            (refresh-bests-plot)
            (refresh-plot))
          (set! inhibit-refresh (sub1 inhibit-refresh))))

    ))
