#lang racket/gui
;; inspect-histogram.rkt -- histogram plot view for a session.
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

(require pict
         plot
         "activity-util.rkt"
         "al-prefs.rkt"
         "al-widgets.rkt"
         "fmt-util.rkt"
         "plot-axis-def.rkt"
         "plot-builder.rkt"
         "snip-canvas.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide histogram-plot-panel%)

;; Axis choices for all non lap swimming sports.
(define (default-axis-choices)
  (list
   axis-speed
   axis-pace
   axis-speed-zone
   axis-hr-bpm
   axis-hr-pct
   axis-hr-zone
   axis-cadence
   axis-vertical-oscillation
   axis-stance-time
   axis-stance-time-percent
   axis-stride
   axis-power
   axis-power-zone
   axis-left-right-balance
   (list "Torque Effectiveness (%)" axis-left-torque-effectiveness axis-right-torque-effectiveness)
   (list "Pedal Smoothness (%)" axis-left-pedal-smoothness axis-right-pedal-smoothness)
   (list "Platform Centre Offset" axis-left-platform-centre-offset axis-right-platform-centre-offset)
   (list "Power Phase Start" axis-left-power-phase-start axis-right-power-phase-start)
   (list "Power Phase End" axis-left-power-phase-end axis-right-power-phase-end)
   (list "Power Phase Angle" axis-left-power-phase-angle axis-right-power-phase-angle)
   (list "Peak Power Phase Start" axis-left-peak-power-phase-start axis-right-peak-power-phase-start)
   (list "Peak Power Phase End" axis-left-peak-power-phase-end axis-right-peak-power-phase-end)
   (list "Peak Power Phase Angle" axis-left-peak-power-phase-angle axis-right-peak-power-phase-angle)
   ))

;; Axis choices for lap swimming
(define (swim-axis-choices)
  (list
   axis-swim-avg-cadence
   axis-swim-stroke-count
   axis-swim-swolf
   axis-swim-pace))

(define histogram-plot-panel%
  (class object%
    (init parent)
    (super-new)

    (define pref-tag 'activity-log:histogram-plot)

    (define y-axis-index 0)
    (define show-grid? #f)
    (define show-as-percentage? #f)
    (define include-zeroes? #t)
    (define bucket-width 1)
    (define filter-amount 0)

    (define default-drb (new plot-builder%))
    (define swim-drb (new swim-plot-builder%))

    (define drb default-drb)
    (define axis-choices '())

    (define y-axis-choice #f)
    (define show-grid-check-box #f)
    (define bucket-width-field #f)
    (define show-as-percentage-check-box #f)
    (define include-zeroes-check-box #f)
    (define filter-amount-choice #f)

    ;; The selection for the Y axis and bucket width is stored per sport in a
    ;; hash table, to be restored when a similar sport is selected.  This hash
    ;; table is also stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    ;; Restore the preferences now. 
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 5))
        (set! show-grid? (first pref))
        (set! show-as-percentage? (second pref))
        (set! include-zeroes? (third pref))
        (set! axis-choice-by-sport (hash-copy (fourth pref)))
        (set! filter-amount (fifth pref))))

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
      (set! y-axis-choice
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "Data to plot: "]
                 [callback (lambda (c e)
                             (on-y-axis-changed (send c get-selection)))]))
      (set! bucket-width-field
            (new number-input-field%
                 [parent cp] 
                 [label "Bucket Width "]
                 [cue-text "1 to 100"]
                 [min-value 1]
                 [max-value 100]
                 [stretchable-width #f]
                 ;; [min-width 100]
                 [valid-value-cb 
                  (lambda (v) (unless (eq? v 'empty) (on-bucket-width v)))]))

      (set! show-as-percentage-check-box
            (new check-box% [parent cp]
                 [value show-as-percentage?]
                 [label "Show as Percentage"]
                 [callback (lambda (c e)
                             (set! show-as-percentage? (send c get-value))
                             (refresh-plot))]))

      (set! include-zeroes-check-box
            (new check-box% [parent cp]
                 [value include-zeroes?]
                 [label "Include Zeroes"]
                 [callback (lambda (c e)
                             (set! include-zeroes? (send c get-value))
                             (refresh-plot))]))

      (set! show-grid-check-box
            (new check-box% [parent cp]
                 [value show-grid?]
                 [label "Show Grid"]
                 [callback (lambda (c e)
                             (set! show-grid? (send c get-value))
                             (refresh-plot))]))

      (set! filter-amount-choice
            (new choice% [parent cp]
                 [label "Filter Amount: "]
                 [choices '("None" "Small" "Medium" "Large" "Huge")]
                 [callback (lambda (c e)
                             (on-filter-amount (send c get-selection)))]))
      )

    ;; Update the axis selection checkboxes with AXIS-CHOICE-LIST
    (define (install-axis-choices axis-choice-list)
      (send y-axis-choice clear)
      (for ([a axis-choice-list])
        (let ((n (if (list? a)
                     (car a)
                     (send a get-axis-label))))
          (send y-axis-choice append n))))

    (define graph-render-tree #f)

    ;; Pasteboard to display the actual plot
    (define graph-pb (new snip-canvas% [parent panel]))

    (define (on-y-axis-changed new-axis-index)
      (unless (equal? y-axis-index new-axis-index)
        (set! y-axis-index new-axis-index)
        (let ([axis (list-ref axis-choices y-axis-index)])
          (if (list? axis)
              (begin
                (send drb set-y-axis (second axis))
                (send drb set-secondary-y-axis (third axis)))
              (begin
                (send drb set-y-axis axis)
                (send drb set-secondary-y-axis #f))))
        (refresh-plot)))

    (define (on-bucket-width new-width)
      (unless (equal? bucket-width new-width)
        (set! bucket-width new-width)
        (refresh-plot)))

    (define (on-filter-amount a [dont-refresh #f])
      (set! filter-amount a)
      (send drb set-filter-amount (expt a 2))
      (unless dont-refresh (refresh-plot)))

    (define (get-plot-snip)
      (if (not graph-render-tree)
          #f
          (let ((rt graph-render-tree))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((y-axis (list-ref axis-choices y-axis-index)))
              (when (list? y-axis) (set! y-axis (second y-axis)))
              (parameterize ([plot-y-label (if show-as-percentage? "pct %" "time (seconds)")]
                             [plot-x-ticks (send y-axis get-axis-ticks)]
                             [plot-x-label (send y-axis get-axis-label)])
                            (plot-snip rt))))))

    (define (refresh-plot)
      (let ((axis (list-ref axis-choices y-axis-index)))
        (when (list? axis) (set! axis (second axis)))
        (let ((bw (* bucket-width (send axis get-histogram-bucket-slot))))
          (set! graph-render-tree (send drb get-histogram-renderer
                                        bw show-as-percentage? include-zeroes?)))
        (let ((snip (get-plot-snip)))
          (send graph-pb set-snip snip))))

    (define/public (save-visual-layout)
      (when current-sport
        (hash-set! axis-choice-by-sport current-sport 
                  (cons bucket-width y-axis-index)))
      (al-put-pref pref-tag (list show-grid? show-as-percentage? include-zeroes? 
                                  axis-choice-by-sport filter-amount)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send graph-pb export-image-to-file file))))

    (define generation -1)

    (define/public (set-session session)
      ;; maybe save previous sport settings
      (when current-sport
        (hash-set! axis-choice-by-sport current-sport 
                  (cons bucket-width y-axis-index)))
      
      (set! generation (+ 1 generation))

      (let ((lap-swimming? 
             (let ((sport (session-sport session))
                   (sub-sport (session-sub-sport session)))
               (and (equal? sport 5) (equal? sub-sport 17)))))

        (set! axis-choices (if lap-swimming? (swim-axis-choices) (default-axis-choices)))
        (set! drb (if lap-swimming? swim-drb default-drb))
        ;; the set-x-axis call is dummy and used only to make the data render
        ;; builder happy.
        (send drb set-x-axis (if lap-swimming? axis-swim-time axis-timer-time))
        (install-axis-choices axis-choices)

        (set! current-sport
              (cons (session-sport session) (session-sub-sport session)))

        (let ((xy-index (hash-ref axis-choice-by-sport current-sport
                                  (lambda () (cons 1 0)))))
          (set! bucket-width (car xy-index))
          (set! y-axis-index (cdr xy-index)))

        (when (equal? bucket-width 0)
          (set! bucket-width 1))

        (let ([axis (list-ref axis-choices y-axis-index)])
          (if (list? axis)
              (begin
                (send drb set-y-axis (second axis))
                (send drb set-secondary-y-axis (third axis)))
              (begin
                (send drb set-y-axis axis)
                (send drb set-secondary-y-axis #f))))
        (send drb set-session session)
        (send bucket-width-field set-value (number->string bucket-width))
        (send y-axis-choice set-selection y-axis-index)

        (if lap-swimming?
            (begin
              (send filter-amount-choice set-selection 0)
              (send drb set-filter-amount 0)
              (send filter-amount-choice enable #f))
            (begin
              (send filter-amount-choice set-selection filter-amount)
              (on-filter-amount filter-amount #f)
              (send filter-amount-choice enable #t)))

        (refresh-plot)))

    (define/public (get-generation) generation)

    ))
