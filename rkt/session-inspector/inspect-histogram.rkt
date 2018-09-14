#lang racket/base
;; inspect-histogram.rkt -- histogram plot view for a session.
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

(require plot/no-gui
         racket/class
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/string
         "../data-frame/df.rkt"
         "../data-frame/histogram.rkt"
         "../fmt-util.rkt"
         "../plot-util.rkt"
         "../session-df/native-series.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt")

(provide histogram-plot-panel%)

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

(define histogram-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:histogram-plot)

    ;; Variables that control the look of the plot
    (define axis-choices '())
    (define y-axis-index 0)
    (define show-as-percentage? #f)
    (define include-zeroes? #t)
    (define color-by-zone? #f)
    (define bucket-width 1)
    (define outlier-trim 0)

    ;; Map a sport to an Y axis selection, to be restored when a similar sport
    ;; is selected.
    (define axis-by-sport (make-hash))

    ;; Map a (sport, axis) selection to histogram parameters.  Each sport axis
    ;; combination have their own parameters.
    (define params-by-axis (make-hash))

    ;; Restore the preferences now.
    (let ((pref (get-pref pref-tag (lambda () #f))))
      (when (and pref (> (length pref) 0) (eq? (car pref) 'gen2))
        (match-define (list tag abs pba as-pct?) pref)
        (set! axis-by-sport (hash-copy abs))
        (set! params-by-axis (hash-copy pba))
        (set! show-as-percentage? as-pct?)))

    ;; Root widget of the entire scatter plot panel
    (define panel
      (new (class vertical-panel%
             (init) (super-new)
             (define/public (interactive-export-image)
               (on-interactive-export-image)))
           [parent parent] [border 5] [spacing 5]
           [alignment '(center top)]))

    ;; Holds the widgets that control the look of the plot
    (define control-panel
      (new horizontal-panel%
           [parent panel] [spacing 10] [border 0]
           [alignment '(center center)]
           [stretchable-height #f]))

    (define y-axis-choice
      (new choice% [parent control-panel]
           [choices '()] [min-width 300] [label "Data to plot: "]
           [callback (lambda (c e) (on-y-axis-changed (send c get-selection)))]))

    (define show-as-percentage-check-box
      (new check-box% [parent control-panel]
           [value show-as-percentage?] [label "Show as Percentage"]
           [callback (lambda (c e) (on-show-as-percentage (send c get-value)))]))

    (define bucket-width-field
      (new number-input-field% [parent control-panel]
           [label "Bucket Width "] [cue-text "1 to 100"]
           [min-value 1] [max-value 100]
           [stretchable-width #f]
           [valid-value-cb (lambda (v) (on-bucket-width (if (eq? v 'empty) 1 v)))]))

    (define include-zeroes-check-box
      (new check-box% [parent control-panel]
           [value include-zeroes?] [label "Include Zeroes"]
           [callback (lambda (c e) (on-include-zeroes (send c get-value)))]))

    (define color-by-zone-check-box
      (new check-box% [parent control-panel]
           [value color-by-zone?] [label "Color by Zone"]
           [callback (lambda (c e) (on-color-by-zone (send c get-value)))]))

    (define outlier-trim-field
      (new number-input-field% [parent control-panel]
           [label "Outlier Trim (%) "] [cue-text "0 .. 100%"]
           [min-value 0] [max-value 100]
           [stretchable-width #f]
           [valid-value-cb
            (lambda (v) (let ((trim (if (eq? v 'empty) 0 v)))
                          (on-outlier-trim (/ trim 100))))]))

    ;; Pasteboard to hold the actual plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;; Data from the session we inspect
    (define data-frame #f)
    (define inhibit-refresh #f)         ; when #t, refresh-plot will do nothing
    (define plot-rt #f)                 ; plot render tree
    (define histogram-data #f)
    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define export-file-name #f)

    (define (current-sport)
      (if data-frame (df-get-property data-frame 'sport) #f))

    (define (lap-swimming?)
      (if data-frame (df-get-property data-frame 'is-lap-swim?) #f))

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

    ;; Update the axis selection checkboxes with AXIS-LIST
    (define (install-axis-choices axis-list)
      (send y-axis-choice clear)
      (for ([a axis-list])
        (let ((n (if (list? a) (car a) (send a axis-label))))
          (send y-axis-choice append n))))

    ;; Enable the "color by zone" checkbox if the selected series has a factor
    ;; function.
    (define (maybe-enable-color-by-zone-checkbox)
      (let ((y-axis (list-ref axis-choices y-axis-index))
            (sport (and data-frame (df-get-property data-frame 'sport)))
            (sid (and data-frame (df-get-property data-frame 'session-id))))
        (when (list? y-axis) (set! y-axis (second y-axis)))
        (send color-by-zone-check-box enable
              (send y-axis factor-fn sport sid))))

    (define (on-y-axis-changed new-index)
      (unless (equal? y-axis-index new-index)
        (save-params-for-axis)
        (set! y-axis-index new-index)
        (restore-params-for-axis)
        (set! export-file-name #f)
        (maybe-enable-color-by-zone-checkbox)
        (refresh-plot)))

    (define (on-show-as-percentage flag)
      (unless (equal? show-as-percentage? flag)
        (set! show-as-percentage? flag)
        (refresh-plot)))

    (define (on-color-by-zone flag)
      (unless (equal? color-by-zone? flag)
        (set! color-by-zone? flag)
        (refresh-plot)))

    (define (on-bucket-width width)
      (unless (equal? bucket-width width)
        (set! bucket-width width)
        (refresh-plot)))

    (define (on-include-zeroes flag)
      (unless (equal? include-zeroes? flag)
        (set! include-zeroes? flag)
        (refresh-plot)))

    (define (on-outlier-trim trim)
      (unless (equal? outlier-trim trim)
        (set! outlier-trim trim)
        (refresh-plot)))

    (define (plot-hover-callback snip event x y)
      (define renderer #f)
      (when (good-hover? x y event)
        (define dual?
          (list? (list-ref axis-choices y-axis-index)))
        (define skip (if dual? 2.5 1.0))
        (define gap (if dual? 0.15 1/8))
        (define-values (series slot) (xposition->histogram-slot x skip gap))
        (when (and slot histogram-data (< slot (vector-length histogram-data)))
          (define item (vector-ref histogram-data slot))
          (when (and series (< series (sub1 (vector-length item))))
            ;; NOTE first item in the vector is the bucket name, not the value
            (define value (vector-ref item (add1 series)))
            (when (<= y value)
              (let ((tag (cond (show-as-percentage?
                                (format "~a %" (~r value #:precision 1)))
                               ((lap-swimming?)
                                (format "~a pool lengths" (~r value #:precision 1)))
                               (#t
                                (duration->string value)))))
                (set! renderer (list (pu-label x y tag))))))))
      (set-overlay-renderers snip renderer))

    ;; Prepare the plot snip and insert it into the pasteboard. Assumes the
    ;; render tree is ready (if it is #f, there is no data for the plot).
    (define (put-plot-snip)
      (when plot-rt
        (let ((rt plot-rt))
          (set! rt (cons (tick-grid) rt))
          (let ((y-axis (list-ref axis-choices y-axis-index)))
            (when (list? y-axis) (set! y-axis (second y-axis)))
            (parameterize ([plot-y-label (if show-as-percentage? "pct %"
                                             (if (lap-swimming?) "# of lengths" "time"))]
                           [plot-y-ticks (if (or show-as-percentage? (lap-swimming?))
                                             (linear-ticks)
                                             (time-ticks))]
                           [plot-x-ticks (send y-axis plot-ticks)]
                           [plot-x-label (send y-axis axis-label)])
              (define snip (plot-to-canvas rt plot-pb))
              (set-mouse-event-callback snip plot-hover-callback))))))

    ;; Build a plot render tree (PLOT-RT) based on current selections.  Note
    ;; that procesing happens in a separate task, and the render tree will
    ;; become available at a later time.  Once the new render tree is
    ;; available, it will be automatically inserted into the pasteboard.
    (define (refresh-plot)

      (define (get-factor-fn axis df)
        (let ((sport (df-get-property df 'sport))
              (sid (df-get-property df 'session-id)))
          (send axis factor-fn sport sid)))

      (unless inhibit-refresh
        (set! plot-rt #f)
        (send plot-pb set-background-message "Working...")
        (send plot-pb set-snip #f)
        ;; Capture all relevant vars, as we are about to queue up a separate
        ;; task
        (let ((axis (list-ref axis-choices y-axis-index))
              (df data-frame)
              (as-pct? show-as-percentage?)
              (zeroes? include-zeroes?)
              (cbz? color-by-zone?)
              (trim outlier-trim)
              (bw bucket-width))
          (queue-task
           "inspect-histogram%/refresh-plot"
           (lambda ()
             (let* ((axis1 (if (list? axis) (second axis) axis))
                    (axis2 (if (list? axis) (third axis) #f))
                    (factor-fn (and cbz? (get-factor-fn axis1 df)))
                    (factor-colors (send axis1 factor-colors))
                    (sname1 (send axis1 series-name))
                    (sname2 (if axis2 (send axis2 series-name) #f))
                    (bw (* bw (send axis1 histogram-bucket-slot)))
                    (h1 (df-histogram df sname1
                                      #:bucket-width bw #:include-zeroes? zeroes?
                                      #:as-percentage? as-pct? #:trim-outliers trim))
                    (h2 (if sname2
                            (df-histogram df sname2
                                          #:bucket-width bw #:include-zeroes? zeroes?
                                          #:as-percentage? as-pct? #:trim-outliers trim)
                            #f))
                    (combined-histograms (if h2 (combine-histograms h1 h2) h1))
                    (rt (cond
                          ((and axis2 combined-histograms)
                           (histogram-renderer/dual
                            combined-histograms
                            (send axis1 plot-label)
                            (send axis2 plot-label)
                            #:x-value-formatter (send axis1 value-formatter)
                            #:color1 (send axis1 plot-color)
                            #:color2 (send axis2 plot-color)))
                          (h1
                           (if factor-fn
                               (histogram-renderer/factors
                                h1 factor-fn factor-colors
                                #:x-value-formatter (send axis1 value-formatter))
                               (list (histogram-renderer
                                      h1
                                      #:x-value-formatter (send axis1 value-formatter)
                                      #:color (send axis1 plot-color)))))
                          (#t #f))))
               (queue-callback
                (lambda ()
                  (if rt
                      (begin
                        (set! plot-rt rt)
                        (set! histogram-data (or combined-histograms h1))
                        (put-plot-snip))
                      (send plot-pb set-background-message "No data to plot"))))))))))

    ;; Save the axis specific plot parameters for the current axis
    (define (save-params-for-axis)
      (when (current-sport)
        (let* ((sport (current-sport))
               (axis-name (axis-label y-axis-index))
               (key (cons sport axis-name)))
          (hash-set! params-by-axis key
                     (hash
                      'bucket-width bucket-width
                      'include-zeroes? include-zeroes?
                      'outlier-trim outlier-trim
                      'color-by-zone? color-by-zone?)))))

    ;; Set default parameters for an axis, used when no params for this axis
    ;; have been set yet (and thus cannot be restored).
    (define (set-default-params-for-axis)
      (send bucket-width-field set-numeric-value 1)
      (on-bucket-width 1)
      (send include-zeroes-check-box set-value #f)
      (set! include-zeroes? #f)
      (send color-by-zone-check-box set-value #f)
      (set! color-by-zone? #f)
      (send outlier-trim-field set-numeric-value 0)
      (on-outlier-trim 0))

    ;; Restore the axis specific parameters for the current axis
    (define (restore-params-for-axis)
      (when (current-sport)
        (let* ((sport (current-sport))
               (axis-name (axis-label y-axis-index))
               (key (cons sport axis-name))
               (val (hash-ref params-by-axis key (lambda () #f))))
          (if (hash? val)
              (let ((bw (hash-ref val 'bucket-width 1))
                    (incz (hash-ref val 'include-zeroes? #f))
                    (trim (hash-ref val 'outlier-trim 0))
                    (cbz (hash-ref val 'color-by-zone? #f)))
                (send bucket-width-field set-numeric-value bw)
                (on-bucket-width bw)
                (send include-zeroes-check-box set-value incz)
                (set! include-zeroes? incz)
                (send color-by-zone-check-box set-value cbz)
                (set! color-by-zone? cbz)
                (send outlier-trim-field set-numeric-value (* trim 100))
                (on-outlier-trim trim))
              (set-default-params-for-axis)))))

    ;; Save all parameters (axis and their associated parameters) for the
    ;; current sport.
    (define (save-params-for-sport)
      (when (current-sport)
        (save-params-for-axis)
        (let ((name (axis-label y-axis-index)))
          (hash-set! axis-by-sport (current-sport) name))))

    ;; Restore the selected axis and its parameters for the current sport.
    (define (restore-params-for-sport)
      (when (current-sport)
        (let ((name (hash-ref axis-by-sport (current-sport) #f)))
          (let ((index (find-axis name axis-choices)))
            (set! y-axis-index (or index 0)))))
      (when (<= y-axis-index 0 (sub1 (send y-axis-choice get-number)))
        (send y-axis-choice set-selection y-axis-index))
      (set! export-file-name #f)
      (restore-params-for-axis))

    (define/public (save-visual-layout)
      (when (> (length axis-choices) 0)
        (save-params-for-sport)
        (let ((data (list 'gen2 axis-by-sport params-by-axis show-as-percentage?)))
          (put-pref pref-tag data))))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define (get-default-export-file-name)
      (or export-file-name
          (let ((sid (df-get-property data-frame 'session-id))
                (axis (list-ref axis-choices y-axis-index)))
            (cond ((and sid (list? axis))
                   (format "histogram-~a-~a-~a.png" sid
                           (send (second axis) series-name)
                           (send (third axis) series-name)))
                  ((and sid axis)
                   (format "histogram-~a-~a.png" sid
                           (send axis series-name)))
                  (#t
                   "histogram.png")))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (get-default-export-file-name) "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! export-file-name file)
          (send plot-pb export-image-to-file file))))

    (define/public (set-session session df)
      (set! inhibit-refresh #t)
      (save-params-for-sport)
      (set! data-frame df)
      (set! axis-choices
            (filter-axis-list
             data-frame
             (if (lap-swimming?) swim-axis-choices default-axis-choices)))
      (install-axis-choices axis-choices)
      (if (> (length axis-choices) 0)
          (begin
            (restore-params-for-sport)
            (set! inhibit-refresh #f)
            (set! export-file-name #f)
            (maybe-enable-color-by-zone-checkbox)
            (refresh-plot))
          (begin
            (set! inhibit-refresh #f))))

    ))
