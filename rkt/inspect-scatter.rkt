#lang racket/gui
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

(require pict
         plot
         "activity-util.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt"
         "plot-axis-def.rkt"
         "plot-builder.rkt"
         "snip-canvas.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide scatter-plot-panel%)

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
   axis-power
   axis-power-zone
   axis-torque
   axis-left-right-balance
   axis-torque-effectiveness
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-pedal-smoothness
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

(define scatter-plot-panel%
  (class object%
    (init parent)
    (super-new)

    (define pref-tag 'activity-log:scatter-plot)

    (define x-axis-index 0)
    (define y-axis-index 0)
    (define show-grid? #f)
    (define filter-amount 0)
    (define delay-amount #f)

    (define default-drb (new plot-builder%))
    (define swim-drb (new swim-plot-builder%))

    (define drb default-drb)
    (define axis-choices '())

    (define x-axis-choice #f)
    (define y-axis-choice #f)
    (define delay-amount-field #f)
    (define show-grid-check-box #f)
    (define filter-amount-choice #f)

    ;; The selection for the X-Y axis is stored per sport in a hash table, to
    ;; be restored when a similar sport is selected.  This hash table is also
    ;; stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    ;; Restore the preferences now. 
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 4))
        (set! show-grid? (first pref))
        (set! axis-choice-by-sport (hash-copy (second pref)))
        (set! filter-amount (third pref))
        (set! delay-amount (fourth pref))))

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

      (set! filter-amount-choice
            (new choice% [parent cp]
                 [label "Filter Amount: "]
                 [choices '("None" "Small" "Medium" "Large" "Huge")]
                 [callback (lambda (c e)
                             (on-filter-amount (send c get-selection)))]))

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
        (send drb set-x-axis (list-ref axis-choices x-axis-index))
        (refresh-plot)))

    (define (on-y-axis-changed new-axis-index)
      (unless (equal? y-axis-index new-axis-index)
        (set! y-axis-index new-axis-index)
        (send drb set-y-axis (list-ref axis-choices y-axis-index))
        (refresh-plot)))

    (define (on-delay-amount new-delay [dont-refresh #f])
      (when (eq? new-delay 'empty)
        (set! new-delay #f))
      (unless (equal? delay-amount new-delay)
        (set! delay-amount new-delay)
        (send drb set-delay-amount delay-amount)
        (unless dont-refresh (refresh-plot))))

    (define (get-plot-snip)
      (if (not graph-render-tree)
          #f
          (let ((rt (list graph-render-tree)))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((x-axis (list-ref axis-choices x-axis-index))
                  (y-axis (list-ref axis-choices y-axis-index)))
              (parameterize ([plot-x-ticks (send x-axis get-axis-ticks)]
                             [plot-x-label (send x-axis get-axis-label)]
                             [plot-y-ticks (send y-axis get-axis-ticks)]
                             [plot-y-label (send y-axis get-axis-label)])
                            (plot-snip rt))))))

    (define (refresh-plot)
      (set! graph-render-tree (send drb get-plot-renderer #t))
      (let ((snip (get-plot-snip)))
        (send graph-pb set-snip snip)))

    (define (on-filter-amount a [dont-refresh #f])
      (set! filter-amount a)
      (send drb set-filter-amount (expt a 2))
      (unless dont-refresh (refresh-plot)))

    (define/public (save-visual-layout)
      (when current-sport
        (hash-set! axis-choice-by-sport current-sport 
                  (cons x-axis-index y-axis-index)))
      (al-put-pref pref-tag (list show-grid? axis-choice-by-sport filter-amount delay-amount)))

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
                  (cons x-axis-index y-axis-index)))
      
      (set! generation (+ 1 generation))

      (let ((lap-swimming? 
             (let ((sport (session-sport session))
                   (sub-sport (session-sub-sport session)))
               (and (equal? sport 5) (equal? sub-sport 17)))))
        (set! axis-choices (if lap-swimming? (swim-axis-choices) (default-axis-choices)))
        (set! drb (if lap-swimming? swim-drb default-drb))
        (install-axis-choices axis-choices)

        (set! current-sport
              (cons (session-sport session) (session-sub-sport session)))

        (let ((xy-index (hash-ref axis-choice-by-sport current-sport
                                  (lambda () (cons 0 0)))))
          (set! x-axis-index (car xy-index))
          (set! y-axis-index (cdr xy-index)))

        (send drb set-x-axis (list-ref axis-choices x-axis-index))
        (send drb set-y-axis (list-ref axis-choices y-axis-index))
        (send drb set-session session)
        (send x-axis-choice set-selection x-axis-index)
        (send y-axis-choice set-selection y-axis-index)

        (if lap-swimming?
            (begin
              (send filter-amount-choice set-selection 0)
              (send drb set-filter-amount 0)
              (send filter-amount-choice enable #f)
              (send delay-amount-field enable #f))
            (begin
              (send filter-amount-choice set-selection filter-amount)
              (on-filter-amount filter-amount #t)
              (let ((amt delay-amount))
                (set! delay-amount #f)
                (on-delay-amount amt #t))
              (send filter-amount-choice enable #t)
              (send delay-amount-field enable #t)))
        
        (refresh-plot)))

    (define/public (get-generation) generation)

    ))
