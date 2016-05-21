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

(define (filter-axis-choices data-frame axis-choices)
  (for/list ([axis axis-choices]
             #:when (send data-frame contains? (send axis get-series-name)))
    axis))

(define (find-axis axis-choices series-name)
  (for/first ([axis axis-choices]
              [index (in-range (length axis-choices))]
              #:when (equal? series-name (send axis get-series-name)))
    index))

;; Axis choices for all non lap swimming sports.  Any pair of axis from this
;; list is valid for the scatter plot.
(define (default-axis-choices)
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
(define (swim-axis-choices)
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
    (values xmin xmax ymin ymax)))

(define scatter-plot-panel%
  (class object%
    (init parent)
    (super-new)

    (define pref-tag 'activity-log:scatter-plot)

    (define x-axis-index 0)
    (define y-axis-index 0)
    (define show-grid? #f)
    (define delay-amount #f)

    (define axis-choices '())

    (define x-axis-choice #f)
    (define y-axis-choice #f)
    (define delay-amount-field #f)
    (define show-grid-check-box #f)

    ;; The selection for the X-Y axis is stored per sport in a hash table, to
    ;; be restored when a similar sport is selected.  This hash table is also
    ;; stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    (define data-series #f)
    (define x-min #f)
    (define x-max #f)
    (define y-min #f)
    (define y-max #f)
    
    ;; Restore the preferences now. 
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 2))
        (set! show-grid? (first pref))
        (set! axis-choice-by-sport (hash-copy (second pref)))))

    (define panel (new (class vertical-panel%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
		       [parent parent]
		       [border 5]
		       [spacing 5]
		       [alignment '(center top)]))

    (let ((cp (new horizontal-panel% 
                   [parent panel] [spacing 10] [border 0]
                   [alignment '(center center)]
                   [stretchable-height #f])))
      (set! x-axis-choice
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "X Axis: "]
                 [callback (lambda (c e)
                             (on-x-axis-changed (send c get-selection)))]))
      (set! y-axis-choice
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "Y Axis: "]
                 [callback (lambda (c e)
                             (on-y-axis-changed (send c get-selection)))]))
      (set! show-grid-check-box
            (new check-box% [parent cp]
                 [value show-grid?]
                 [label "Show Grid"]
                 [callback (lambda (c e)
                             (set! show-grid? (send c get-value))
                             (refresh-plot))]))

      (set! delay-amount-field
            (new number-input-field%
                 [parent cp] 
                 [label "Delay Amount: "]
                 [cue-text "seconds"]
                 [min-value 0]
                 [stretchable-width #f]
                 ;; [min-width 100]
                 [valid-value-cb (lambda (v) (on-delay-amount v))]))
      
      (if (number? delay-amount)
          (send delay-amount-field set-numeric-value delay-amount)
          (send delay-amount-field set-value ""))

      )

    ;; Update the axis selection checkboxes with AXIS-CHOICE-LIST
    (define (install-axis-choices axis-choice-list)
      (send x-axis-choice clear)
      (send y-axis-choice clear)
      (for ((a (in-list axis-choice-list)))
        (let ((n (send a get-axis-label)))
          (send x-axis-choice append n)
          (send y-axis-choice append n))))

    (define graph-render-tree #f)

    ;; Pasteboard to display the actual plot
    (define graph-pb (new snip-canvas% [parent panel]))

    (define (on-x-axis-changed new-axis-index)
      (unless (equal? x-axis-index new-axis-index)
        (set! x-axis-index new-axis-index)
        (set! data-series #f)
        (refresh-plot)))

    (define (on-y-axis-changed new-axis-index)
      (unless (equal? y-axis-index new-axis-index)
        (set! y-axis-index new-axis-index)
        (set! data-series #f)
        (refresh-plot)))

    (define (on-delay-amount new-delay [dont-refresh #f])
      (when (eq? new-delay 'empty)
        (set! new-delay #f))
      (unless (equal? delay-amount new-delay)
        (set! delay-amount new-delay)
        (unless dont-refresh (refresh-plot))))

    (define (put-plot-snip canvas)
      (if (not graph-render-tree)
          (begin
            (send canvas set-background-message "No data for graph")
            (send canvas set-snip #f))
          (let ((rt (list graph-render-tree)))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((x-axis (list-ref axis-choices x-axis-index))
                  (y-axis (list-ref axis-choices y-axis-index)))
              (parameterize ([plot-x-ticks (send x-axis get-axis-ticks)]
                             [plot-x-label (send x-axis get-axis-label)]
                             [plot-y-ticks (send y-axis get-axis-ticks)]
                             [plot-y-label (send y-axis get-axis-label)])
                (plot-snip/hack canvas rt #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max))))))

    (define (refresh-plot)
      (set! graph-render-tree #f)
      (send graph-pb set-background-message "Working...")
      (queue-task
       "inspect-scatter%/refresh-plot"
       (lambda ()
         (unless data-series
           (let* ((x (list-ref axis-choices x-axis-index))
                  (y (list-ref axis-choices y-axis-index))
                  (xnam (send x get-series-name))
                  (ynam (send y get-series-name)))
             (when (send data-frame contains? xnam ynam)
               (set! data-series
                     (send data-frame select*
                           xnam ynam
                           "elapsed"
                           #:filter valid-only)))))
         (when data-series
           (let-values (((xmin xmax ymin ymax) (find-bounds data-series)))
             (set! x-min xmin)
             (set! x-max xmax)
             (set! y-min ymin)
             (set! y-max ymax))
           (let ((x (list-ref axis-choices x-axis-index))
                 (y (list-ref axis-choices y-axis-index)))
             (let* ((maybe-delayed
                     (if delay-amount
                         (time-delay-series data-series delay-amount)
                         data-series))
                    (grouped
                     (group-samples maybe-delayed
                                    (send x get-fractional-digits)
                                    (send y get-fractional-digits))))
               (set! graph-render-tree
                     (make-scatter-group-renderer
                      grouped
                      (send y get-line-color))))))
         (put-plot-snip graph-pb))))

    (define (save-params-for-sport)
      (when current-sport
        (let* ((x (list-ref axis-choices x-axis-index))
               (y (list-ref axis-choices y-axis-index))
               (xseries (send x get-series-name))
               (yseries (send y get-series-name)))
          (hash-set! axis-choice-by-sport current-sport 
                     (list xseries yseries delay-amount)))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (al-put-pref pref-tag (list show-grid? axis-choice-by-sport)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send graph-pb export-image-to-file file))))

    (define generation -1)
    (define data-frame #f)

    (define/public (set-session session df)
      (save-params-for-sport)
      
      (set! generation (+ 1 generation))
      (set! data-frame df)
      (define lap-swimming? (send data-frame get-property 'is-lap-swim?))
      (set! axis-choices
            (filter-axis-choices
             data-frame
             (if lap-swimming? (swim-axis-choices) (default-axis-choices))))
      (install-axis-choices axis-choices)
      (set! data-series #f)
      (set! current-sport (send data-frame get-property 'sport))

      (let ((data (hash-ref axis-choice-by-sport current-sport #f)))
        (if data
            (let ()
              (match-define (list xseries yseries delay) data)
              (set! x-axis-index (find-axis axis-choices xseries))
              (set! y-axis-index (find-axis axis-choices yseries))
              (set! delay-amount delay))
            (begin
              (set! x-axis-index 0)
              (set! y-axis-index 0)
              (set! delay-amount #f))))
      
      (send x-axis-choice set-selection x-axis-index)
      (send y-axis-choice set-selection y-axis-index)
      (if (number? delay-amount)
          (send delay-amount-field set-numeric-value delay-amount)
          (send delay-amount-field set-value ""))

      (if lap-swimming?
          (begin
            (send delay-amount-field enable #f))
          (begin
            (let ((amt delay-amount))
              (set! delay-amount #f)
              (on-delay-amount amt #t))
            (send delay-amount-field enable #t)))
        
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
