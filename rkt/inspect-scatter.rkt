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

(require plot
         racket/class
         racket/gui/base
         racket/list
         racket/match
         "activity-util.rkt"
         "al-prefs.rkt"
         "plot-axis-def.rkt"
         "plot-hack.rkt"
         "snip-canvas.rkt"
         "data-frame.rkt"
         "widgets.rkt"
         "workers.rkt")

(provide scatter-plot-panel%)

;; Filter AXIS-LIST to remove any axis definition that don't have a data
;; series in DF, a data-frame%
(define (filter-axis-list df axis-list)
  (define al
    (for/list ([axis axis-list]
               #:when (let ((sn (send axis get-series-name)))
                        (send df contains? sn)))
      axis))
  (sort al string<? #:key (lambda (a) (send a get-axis-title))))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)
  (for/first ([(axis index) (in-indexed axis-list)]
              #:when (equal? series-name (send axis get-series-name)))
    index))

;; Axis choices for all non lap swimming sports.  Any pair of axis from this
;; list is valid for the scatter plot.
(define default-axis-choices
  (list
   axis-distance
   axis-elapsed-time
   axis-timer-time
   axis-speed
   axis-pace
   axis-speed-zone
   axis-corrected-elevation
   axis-grade
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
   axis-left-right-balance
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-platform-centre-offset
   axis-right-platform-centre-offset
   axis-left-power-phase-start
   axis-left-power-phase-end
   axis-left-power-phase-angle
   axis-left-peak-power-phase-start
   axis-left-peak-power-phase-end
   axis-left-peak-power-phase-angle
   axis-right-power-phase-start
   axis-right-power-phase-end
   axis-right-power-phase-angle
   axis-right-peak-power-phase-start
   axis-right-peak-power-phase-end
   axis-right-peak-power-phase-angle
   ))

;; Axis choices for lap swimming
(define swim-axis-choices
  (list
   axis-swim-distance
   axis-swim-time
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-swolf
   axis-swim-pace))

(define (find-bounds data-series)
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
    (vector xmin xmax ymin ymax)))

(define (extract-data data-frame x-axis y-axis)
  (let ((xnam (send x-axis get-series-name))
        (ynam (send y-axis get-series-name)))
    (when (send data-frame contains? xnam ynam)
      (send data-frame select*
            xnam ynam "elapsed"
            #:filter valid-only))))

(define scatter-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:scatter-plot)

    ;; Variables that control the look of the plot

    (define axis-choices '())
    (define x-axis-index 0)
    (define y-axis-index 0)
    (define show-grid? #f)
    (define delay-amount #f)
    
    ;; Map a sport to an X-Y axis selection, to be restored when a similar
    ;; sport is selected.
    (define axis-by-sport (make-hash))

    ;; Map a delay value to an X-Y-Sport axsis selection, to be restored when
    ;; a similar axis choice is present.
    (define params-by-axis (make-hash))
    
    ;; Restore the preferences now, we do it so the controls can be
    ;; initialized with the correct values.
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 3))
        (match-define (list sg? abs dba) pref)
        (set! show-grid? sg?)
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
    
    (define show-grid-check-box
      (new check-box% [parent control-panel] [value show-grid?]
           [label "Show Grid"]
           [callback (lambda (c e) (on-show-grid (send c get-value)))]))

    (define delay-amount-field
      (new number-input-field% [parent control-panel] 
           [label "Delay Amount: "] [cue-text "seconds"] [min-value 0]
           [stretchable-width #f]
           [valid-value-cb (lambda (v) (on-delay-amount v))]))

    ;; Initialize the delay-amount field
    (if (number? delay-amount)
        (send delay-amount-field set-numeric-value delay-amount)
        (send delay-amount-field set-value ""))

    ;; Pasteboard to hold the actual plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;;; Data from the session we inspect
    (define generation -1)
    (define data-frame #f)
    (define data-series #f)
    (define data-bounds (vector #f #f #f #f))
    (define inhibit-refresh #f)         ; when #t, refresh-plot will do nothing
    (define plot-rt #f)                 ; plot render tree

    (define (current-sport)
      (if data-frame (send data-frame get-property 'sport) #f))

    (define (invalidate-data)
      (set! data-series #f)
      (set! data-bounds #f)
      (set! plot-rt #f)
      (refresh-plot))

    ;; Update the axis selection checkboxes with AXIS-LIST
    (define (install-axis-choices axis-list)
      (send x-axis-choice clear)
      (send y-axis-choice clear)
      (for ([a axis-list])
        (let ((n (send a get-axis-title)))
          (send x-axis-choice append n)
          (send y-axis-choice append n))))

    (define (on-x-axis-changed new-index)
      (unless (equal? x-axis-index new-index)
        (save-params-for-axis)
        (set! x-axis-index new-index)
        (restore-params-for-axis)
        (invalidate-data)))

    (define (on-y-axis-changed new-index)
      (unless (equal? y-axis-index new-index)
        (save-params-for-axis)
        (set! y-axis-index new-index)
        (restore-params-for-axis)
        (invalidate-data)))

    (define (on-show-grid flag)
      (unless (equal? show-grid? flag)
        (set! show-grid? flag)
        ;; no need to invalidate the data
        (put-plot-snip)))

    (define (on-delay-amount amount)
      (when (eq? amount 'empty)
        (set! amount #f))
      (unless (equal? delay-amount amount)
        (set! delay-amount amount)
        ;; no need to invalidate the data
        (refresh-plot)))

    ;; Prepare the plot snip and insert it into the pasteboard. Assumes the
    ;; render tree is ready (if it is #f, there is no data for the plot).
    (define (put-plot-snip)
      (when plot-rt
        (let ((rt (list plot-rt)))
          (when show-grid?
            (set! rt (cons (tick-grid) rt)))
          (let ((x-axis (list-ref axis-choices x-axis-index))
                (y-axis (list-ref axis-choices y-axis-index)))
            (parameterize ([plot-x-ticks (send x-axis get-axis-ticks)]
                           [plot-x-label (send x-axis get-axis-label)]
                           [plot-y-ticks (send y-axis get-axis-ticks)]
                           [plot-y-label (send y-axis get-axis-label)])
              (match-define (vector x-min x-max y-min y-max) data-bounds)
              (plot-snip/hack plot-pb rt
                              #:x-min x-min #:x-max x-max
                              #:y-min y-min #:y-max y-max))))))

    ;; Build a plot render tree (PLOT-RT) based on current selections.  Note
    ;; that procesing happens in a separate task, and the render tree will
    ;; become available at a later time.  Once the new render tree is
    ;; available, it will be automatically inserted into the pasteboard.
    (define (refresh-plot)

      ;; HACK: some get-line-color methods return 'smart, we should fix this
      (define (get-color axis)
        (let ((color (send axis get-line-color)))
          (if (or (not color) (eq? color 'smart))
              '(0 148 255)
              color)))
      
      (unless inhibit-refresh
        (set! plot-rt #f)
        (send plot-pb set-background-message "Working...")
        (send plot-pb set-snip #f)
        ;; Capture all relavant vars, as we are about to queue up a separate
        ;; task
        (let ((x (list-ref axis-choices x-axis-index))
              (y (list-ref axis-choices y-axis-index))
              (df data-frame)
              (ds data-series)
              (damt delay-amount))
          (queue-task
           "inspect-scatter%/refresh-plot"
           (lambda ()
             (let ((ds (or ds (extract-data df x y))))
               (if ds
                   (let* ((x-digits (send x get-fractional-digits))
                          (y-digits (send y get-fractional-digits))
                          (color (get-color y))
                          (bounds (find-bounds ds))
                          (delayed (if damt (time-delay-series ds damt) ds))
                          (grouped (group-samples delayed x-digits y-digits))
                          (renderer (make-scatter-group-renderer grouped color)))
                     (queue-callback
                      (lambda ()
                        (set! data-series ds)
                        (set! data-bounds bounds)
                        (set! plot-rt renderer)
                        (put-plot-snip))))
                   (queue-callback
                    (lambda ()
                      (send plot-pb set-background-message "No data to plot"))))))))))

    ;; Store the plot parameters for the current sport, this includes axis
    ;; selection and the parameters for the current axis selection.
    (define (save-params-for-sport)
      (when (current-sport)
        (let* ((sport (current-sport))
               (x (list-ref axis-choices x-axis-index))
               (y (list-ref axis-choices y-axis-index))
               (xseries (send x get-series-name))
               (yseries (send y get-series-name)))
          (hash-set! axis-by-sport sport (list xseries yseries))
          (hash-set! params-by-axis (list sport xseries yseries) delay-amount))))

    ;; Save the parameters for the currently selected axis combination
    (define (save-params-for-axis)
      (let* ((sport (current-sport))
             (x (list-ref axis-choices x-axis-index))
             (y (list-ref axis-choices y-axis-index))
             (xseries (send x get-series-name))
             (yseries (send y get-series-name)))
        (hash-set! params-by-axis (list sport xseries yseries) delay-amount)))

    ;; Restore parameters for rhe current sport.  This assumes that a new
    ;; sport (data frame) was installed, it will set the axis selection and
    ;; axis parameters to what was uses last for the same sport.
    (define (restore-params-for-sport)
      (when (current-sport)
        (let ((data (hash-ref axis-by-sport (current-sport) (lambda () (list 0 0)))))
          (match-define (list xseries yseries) data)
          (set! x-axis-index (or (find-axis xseries axis-choices) 0))
          (set! y-axis-index (or (find-axis yseries axis-choices) 0)))
        (let ((sport (current-sport))
              (xseries (if (< x-axis-index (length axis-choices))
                           (send (list-ref axis-choices x-axis-index) get-series-name)
                           #f))
              (yseries (if (< y-axis-index (length axis-choices))
                           (send (list-ref axis-choices y-axis-index) get-series-name)
                           #f)))
          (set! delay-amount (hash-ref params-by-axis (list sport xseries yseries) #f)))
        (when (> (send x-axis-choice get-number) x-axis-index)
          (send x-axis-choice set-selection x-axis-index))
        (when (> (send y-axis-choice get-number) y-axis-index)
          (send y-axis-choice set-selection y-axis-index))
        (if (number? delay-amount)
            (send delay-amount-field set-numeric-value delay-amount)
            (send delay-amount-field set-value ""))))

    ;; Restore parameters for the current axis selection.  This assumes a new
    ;; axis was selected and will set the parameters for that axis
    ;; combination.
    (define (restore-params-for-axis)
      (when (current-sport)
        (let ((sport (current-sport))
              (xseries (if (< x-axis-index (length axis-choices))
                           (send (list-ref axis-choices x-axis-index) get-series-name)
                           #f))
              (yseries (if (< y-axis-index (length axis-choices))
                           (send (list-ref axis-choices y-axis-index) get-series-name)
                           #f)))
          (set! delay-amount (hash-ref params-by-axis (list sport xseries yseries) #f)))
        (if (number? delay-amount)
            (send delay-amount-field set-numeric-value delay-amount)
            (send delay-amount-field set-value ""))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (let ((data (list show-grid? axis-by-sport params-by-axis)))
        (al-put-pref pref-tag data)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send plot-pb export-image-to-file file))))

    (define/public (set-session session df)
      (set! inhibit-refresh #f)
      (save-params-for-sport)
      (set! generation (+ 1 generation))
      (set! data-frame df)
      (set! delay-amount #f)
      (define lap-swimming? (send data-frame get-property 'is-lap-swim?))
      (set! axis-choices
            (filter-axis-list
             data-frame
             (if lap-swimming? swim-axis-choices default-axis-choices)))
      (install-axis-choices axis-choices)
      (restore-params-for-sport)
      (send delay-amount-field enable (not lap-swimming?))
      (set! inhibit-refresh #f)
      (invalidate-data))

    (define/public (get-generation) generation)

    ))
