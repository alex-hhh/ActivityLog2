#lang racket/base
;; inspect-quadrant.rkt -- Quadrant Plot for a session
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

;; A quadrant plot is a scatter plot displaying cadence vs torque, a threshold
;; power line plots the constant power: you can output the same power by
;; decreasing the cadence and increasing the torque.  A threshold cadence and
;; the corresponding torque for the threshold power divide the plot into four
;; quadrants: Q1 is high candence, high torque, Q2 is high cadence, low
;; torque, Q3 is low cadence, low torque and Q4 is low cadence, high torque.
;;
;; NOTE: the quadrant plot is also defined for run and swim activities, using
;; pace instead of power.  The Y line is stride instead of torque.

(require math/statistics
         plot/no-gui
         racket/class
         racket/function
         racket/gui/base
         racket/match
         racket/math
         "../color-theme.rkt"
         "../data-frame/df.rkt"
         "../data-frame/scatter.rkt"
         "../plot-hack.rkt"
         "../plot-util.rkt"
         "../series-meta.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt")

(provide quadrant-plot-panel%)

;; speed is in mps, cadence is in SPM
(define (cadence->stride speed cadence)
  (if (> cadence 0)
      (let ((step/sec (/ (* cadence 2.0) 60.0)))
        (/ speed step/sec))
      +inf.0))

(define (cadence->torque power cadence)
  (if (> cadence 0)
      (let ((angular-velocity (* (/ cadence 60.0) (* 2 pi))))
        (/ power angular-velocity))
      +inf.0))

(define (filter-torque val)
  (match-define (vector cad torq) val)
  ;; NOTE: torque values bigger than 150 are probably incorrect: on a 172.5mm
  ;; crank, a torque of 150 Nm is achieved when a 88kg athlete puts its entire
  ;; body weight on a single pedal.  A lighter athlete would not be able to
  ;; achieve this torque.
  (and cad torq (> cad 0) (< torq 150)))

(define (filter-cadence val)
  (match-define (vector cad stride) val)
  (and cad stride (> cad 0) (> stride 0)
       (not (eq? stride +inf.0))
       (not (eq? stride +inf.f))))

;; NOTE: duplicated from inspect-scatter!
(define (find-bounds data-series)
  (define (good-or-false num)
    (and (number? num) (not (nan? num)) (not (infinite? num)) num))
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

(define (find-bounds/quantile data-series q)
  (define (good-or-false num)
    (and (number? num) (not (nan? num)) (not (infinite? num)) num))
  (define x-data (make-vector (vector-length data-series) 0))
  (define y-data (make-vector (vector-length data-series) 0))
  (for ([(item index) (in-indexed data-series)])
    (match-define (vector x y) item)
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

(define zcolors (map cdr (zone-colors)))

(define (make-sport-zone-renderers zones yval-fn)
  (define zone-fns (for/list ([z zones] #:when (>= z 0))
                    (curry yval-fn z)))
  (for/list ([low zone-fns]
             [high (cdr zone-fns)]
             [idx (in-range (length zone-fns))])
    (function-interval low high
                       #:color (list-ref zcolors idx)
                       #:alpha 0.1)))

(define quadrant-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:quadrant-plot)

    ;; Variables that control the look of the plot
    (define show-zones? #f)
    (define threshold-speed #f)
    (define threshold-power #f)
    (define threshold-cadence #f)
    (define params-by-sport (make-hash))

    ;; Restore the preferences now.
    (let ((pref (get-pref pref-tag (lambda () #f))))
      (when (and pref (> (length pref) 0) (eq? (car pref) 'gen2))
        (match-define (list tag pbs sz?) pref)
        (set! params-by-sport (hash-copy pbs))
        (set! show-zones? sz?)))

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

    (define run-pace-field
      (new pace-input-field% [parent control-panel]
           [label "Threshold Pace: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
            [valid-value-cb (lambda (v) (on-threshold-speed v))]))

    (define swim-pace-field
      (new swim-pace-input-field% [parent control-panel]
           [label "Threshold Pace: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [valid-value-cb (lambda (v) (on-threshold-speed v))]))

    (define power-field
      (new number-input-field% [parent control-panel]
           [label "Threshold Power: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [cue-text "watts"]
           [min-value 0] [max-value 10000]
           [valid-value-cb (lambda (v) (on-threshold-power v))]))

    (define cadence-field
      (new number-input-field% [parent control-panel]
           [label "Cadence: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [min-value 0] [max-value 300]
           [valid-value-cb (lambda (v) (on-threshold-cadence v))]))

    (define show-zones-check-box
      (new check-box% [parent control-panel] [value show-zones?]
           [label "Show Zones"]
           [callback (lambda (c e) (on-show-zones (send c get-value)))]))

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

    (define (on-show-zones flag)
      (unless (equal? show-zones? flag)
        (set! show-zones? flag)
        (put-plot-snip)))

    (define (on-threshold-cadence cadence)
      (unless (equal? threshold-cadence cadence)
        (set! threshold-cadence (if (number? cadence) cadence #f))
        (put-plot-snip)))

    (define (on-threshold-speed speed)
      (unless (equal? threshold-speed speed)
        (set! threshold-speed (if (number? speed) speed #f))
        (if (and (number? threshold-speed) yval-fn)
            (set! threshold-fn (curry yval-fn threshold-speed))
            (set! threshold-fn #f))
        (put-plot-snip)))

    (define (on-threshold-power power)
      (unless (equal? threshold-power power)
        (set! threshold-power (if (number? power) power #f))
        (if (and (number? threshold-power) yval-fn)
            (set! threshold-fn (curry yval-fn threshold-power))
            (set! threshold-fn #f))
        (put-plot-snip)))

    (define (on-outlier-percentile percentile)
      (when (eq? percentile 'empty)
        (set! percentile #f))
      (unless (equal? percentile outlier-percentile)
        (set! outlier-percentile percentile)
        (if data-series
            ;; Compute the quantiles here, without having to refresh the
            ;; entire plot.
            (let ((opct (if outlier-percentile (/ outlier-percentile 100.0) #f)))
              (set! quantile-bounds
                    (if opct
                        (find-bounds/quantile data-series opct)
                        (vector #f #f #f #f)))
              (put-plot-snip))
            ;; No data series, refresh entire plot to get one and compute the
            ;; quantiles.
            (refresh-plot))))

    (define (on-outlier-handling choice)
      (if (eq? choice 0)
          (set! outlier-handling 'mark)
          (set! outlier-handling 'crop))
      ;; No need to refresh the plot data at all, just rebuild
      (put-plot-snip))


    ;; Pasteboard to display the actual plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;; Data from the session we inspect
    (define data-frame #f)
    ;; will be cadence->torque for bike, cadence->stride for run and swim
    (define yval-fn #f)
    ;; will be axis-cadence for bike and run, axis-swim-avg-cadence for swim
    (define x-axis axis-cadence)
    ;; will be axis-torque for bike, axis-stride for run,
    ;; axis-swim-stroke-length for swim
    (define y-axis #f)
    ;; Sport zones for the current activity
    (define zones #f)
    (define data-series #f)
    (define data-bounds (vector #f #f #f #f))
    (define quantile-bounds (vector #f #f #f))
    ;; Function that plots the line for threshold speed or power
    (define threshold-fn #f)
    ;; Filter function used to extract the data for the plot.  For bike, it
    ;; will filter our unrealistic torque values.
    (define filter-fn filter-cadence)
    (define plot-rt #f)                 ; plot render tree
    (define zone-rt #f)                 ; sport zone render tree
    (define inhibit-refresh #f)
    (define outlier-percentile #f)
    (define outlier-handling 'mark)
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when the session
    ;; changes
    (define export-file-name #f)

    ;; Initialize the delay-amount and outlier percentile fields
    (if (number? outlier-percentile)
        (send outlier-percentile-field set-numeric-value outlier-percentile)
        (send outlier-percentile-field set-value ""))
    (send outlier-handling-choice set-selection
          (if (eq? outlier-handling 'mark) 0 1))

    (define (put-plot-snip)
      (when (and plot-rt (not inhibit-refresh))
        (let ((rt (list plot-rt)))
          (set! rt (cons (tick-grid) rt))
          (when (eq? outlier-handling 'mark)
            (when (vector-ref quantile-bounds 0)
              (set! rt (cons (vrule (vector-ref quantile-bounds 0)
                                    #:color "blue" #:style 'short-dash) rt)))
            (when (vector-ref quantile-bounds 1)
              (set! rt (cons (vrule (vector-ref quantile-bounds 1)
                                    #:color "blue" #:style 'short-dash) rt)))
            (when (vector-ref quantile-bounds 2)
              (set! rt (cons (hrule (vector-ref quantile-bounds 2)
                                    #:color "blue" #:style 'short-dash) rt)))
            (when (vector-ref quantile-bounds 3)
              (set! rt (cons (hrule (vector-ref quantile-bounds 3)
                                    #:color "blue" #:style 'short-dash) rt))))
          (when (and show-zones? zone-rt)
            (set! rt (cons zone-rt rt)))
          (when threshold-fn
            (set! rt (cons (function threshold-fn) rt)))
          (when (and threshold-fn threshold-cadence)
            (set! rt (cons (vrule threshold-cadence)
                           (cons (hrule (threshold-fn threshold-cadence))
                                 rt))))
          (parameterize ([plot-x-ticks (send x-axis plot-ticks)]
                         [plot-x-label (send x-axis axis-label)]
                         [plot-y-ticks (send y-axis plot-ticks)]
                         [plot-y-label (send y-axis axis-label)])
            (match-define (vector x-min x-max y-min y-max)
              (if (eq? outlier-handling 'mark) data-bounds quantile-bounds))
            (plot-snip/hack
             plot-pb (reverse rt)
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)))))

    (define (refresh-plot)
      (unless inhibit-refresh
        (set! plot-rt #f)
        (send plot-pb set-background-message "Working...")
        (send plot-pb set-snip #f)
        ;; Capture all relavant vars, as we are about to queue up a separate
        ;; task
        (let ((x x-axis)
              (y y-axis)
              (df data-frame)
              (opct (if outlier-percentile (/ outlier-percentile 100.0) #f)))
          (queue-task
           "quadrant-plot-panel%/refresh-plot"
           (lambda ()
             (define ds
               (and x y
                    (let ((xnam (send x series-name))
                          (ynam (send y series-name)))
                      (if (df-contains? df xnam ynam)
                          (df-select* df xnam ynam #:filter filter-fn)
                          (error (format "data-frame is missing ~a+~a" xnam ynam))))))
             (define bounds (and ds (find-bounds ds)))
             (define qbounds
               (if (and ds opct)
                   (find-bounds/quantile ds opct)
                   (vector #f #f #f #f)))
             (define grouped
               (and ds
                    (group-samples ds
                                   (send x fractional-digits)
                                   (send y fractional-digits))))
             (define rt
               (and grouped
                    (scatter-group-renderer
                     grouped
                     #:color (send y-axis plot-color))))
             (queue-callback
              (lambda ()
                (set! plot-rt rt)
                (set! data-series ds)
                (set! data-bounds bounds)
                (set! quantile-bounds qbounds)
                (unless plot-rt
                  (send plot-pb set-background-message "No data to plot"))
                (put-plot-snip))))))))

    (define (save-params-for-sport)
      (when data-frame
        (let ((sport (df-get-property data-frame 'sport))
              (data (list threshold-speed threshold-power threshold-cadence outlier-percentile outlier-handling)))
          (hash-set! params-by-sport sport data))))

    (define (restore-params-for-sport)
      (when data-frame
        (let* ((sport (df-get-property data-frame 'sport))
               (data (hash-ref params-by-sport sport (lambda () (list #f #f #f #f #f)))))
          (unless (= (length data) 5) ; opct and ohandling were recently added
            (match-define (list tspeed tpower tcad) data)
            (set! data (list tspeed tpower tpower #f #f)))
          (match-define (list tspeed tpower tcad opct ohandling) data)
          (if tspeed
              (cond ((equal? (vector-ref sport 0) 1) ; running
                     (send run-pace-field set-pace-value tspeed))
                    ((equal? (vector-ref sport 0) 5) ; swimming
                     (send swim-pace-field set-pace-value tspeed)))
              (begin
                (send run-pace-field set-value "")
                (send swim-pace-field set-value "")))
          (if tpower
              (send power-field set-numeric-value tpower)
              (send power-field set-value ""))
          (if tcad
              (send cadence-field set-numeric-value tcad)
              (send cadence-field set-value ""))
          (if (number? opct)
              (send outlier-percentile-field set-numeric-value opct)
              (send outlier-percentile-field set-value ""))
          (send outlier-handling-choice set-selection (if (eq? ohandling 'mark) 0 1))
          (set! threshold-speed #f)
          (set! threshold-power #f)
          (set! threshold-cadence #f)
          (let ((old inhibit-refresh))
            (set! inhibit-refresh #t)
            (on-threshold-speed tspeed)
            (on-threshold-power tpower)
            (on-threshold-cadence tcad)
            (on-outlier-percentile opct)
            (on-outlier-handling (if (eq? ohandling 'mark) 0 1))
            (set! inhibit-refresh old)))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (let ((data (list 'gen2 params-by-sport show-zones?)))
        (put-pref pref-tag data)))

    ;; Return #t if the quadrant plot can be displayed for a data-frame% (DF).
    ;; It can display if the data frame contains the required series
    ;; (depending on the sport)
    (define/public (should-display-for-data-frame? df)
      (let ((sport (df-get-property df 'sport)))
        (or
         (and (equal? (vector-ref sport 0) 1) ; running
              (df-contains? df "spd" "cad"))
         (and (equal? (vector-ref sport 0) 5) ; swim
              (df-contains? df "spd" "cad"))
         (and (equal? (vector-ref sport 0) 2) ; bike
              (df-contains? df "pwr" "cad")))))

    (define/public (set-session session df)
      (save-params-for-sport)
      (set! inhibit-refresh #t)
      (set! data-frame df)
      (set! data-series #f)
      (set! export-file-name #f)
      (define current-sport (df-get-property data-frame 'sport))
      (define session-id (df-get-property data-frame 'session-id))
      (cond
        ((and (equal? (vector-ref current-sport 0) 1) ; running
              (df-contains? data-frame "spd" "cad"))
         (set! x-axis axis-cadence)
         (set! y-axis axis-stride)
         (set! zones (get-session-sport-zones session-id 2))
         (set! yval-fn cadence->stride)
         (set! filter-fn filter-cadence)
         (send control-panel change-children
               (lambda (old) (list run-pace-field cadence-field
                                   show-zones-check-box
                                   outlier-percentile-field outlier-handling-choice))))
        ((and (equal? (vector-ref current-sport 0) 5) ; swim
              (df-contains? data-frame "spd" "cad"))
         (set! x-axis axis-swim-avg-cadence)
         (set! y-axis axis-swim-stroke-length)
         ;; Add the torque series if not present
         (set! zones (get-session-sport-zones session-id 3))
         (set! yval-fn cadence->stride)
         (set! filter-fn filter-cadence)
         (send control-panel change-children
               (lambda (old) (list swim-pace-field cadence-field
                                   show-zones-check-box
                                   outlier-percentile-field outlier-handling-choice))))
        ((and (equal? (vector-ref current-sport 0) 2) ; bike
              (df-contains? data-frame "pwr" "cad"))
         (set! x-axis axis-cadence)
         (set! y-axis axis-torque)
         (set! zones (get-session-sport-zones session-id 3))
         (set! yval-fn cadence->torque)
         (set! filter-fn filter-torque)
         (send control-panel change-children
               (lambda (old) (list power-field cadence-field
                                   show-zones-check-box
                                   outlier-percentile-field outlier-handling-choice))))
        (#t
         (set! zones #f)
         (set! x-axis #f)
         (set! y-axis #f)
         (set! yval-fn #f)
         (send control-panel change-children (lambda (old) '()))))
      (restore-params-for-sport)
      (set! zone-rt
            (and zones yval-fn
                 (make-sport-zone-renderers zones yval-fn)))
      (set! inhibit-refresh #f)
      (refresh-plot))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id.
    (define (get-default-export-file-name)
      (or export-file-name
          (let ((sid (df-get-property data-frame 'session-id)))
            (if sid (format "quadrant-~a.png" sid) "quadrant.png"))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (get-default-export-file-name) "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! export-file-name file)
          (send plot-pb export-image-to-file file))))

    ))
