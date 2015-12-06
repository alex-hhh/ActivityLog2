#lang racket/gui
;; inspect-best-avg.rkt -- best-avg plot view for a session.  This is not
;; supported for swimming activites.
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
         racket/draw
         "activity-util.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt"
         "plot-axis-def.rkt"
         "plot-builder.rkt"
         "snip-canvas.rkt"
         "spline-interpolation.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide best-avg-plot-panel%)

;; Axis choices for all non lap swimming sports.  note that some axis choices
;; don't make sense, so they are not listed here, to keep the list smaller.
(define (get-best-avg-axis-choices)
  (list
   axis-speed
   axis-pace
   axis-speed-zone
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
   axis-torque-effectiveness
   axis-left-torque-effectiveness
   axis-right-torque-effectiveness
   axis-pedal-smoothness
   axis-left-pedal-smoothness
   axis-right-pedal-smoothness
   axis-left-power-phase-angle
   axis-left-peak-power-phase-angle
   axis-right-power-phase-angle
   axis-right-peak-power-phase-angle
   ))

(define best-avg-plot-panel%
  (class object%
    (init parent)
    (super-new)

    (define pref-tag 'activity-log:best-avg-plot)

    ;; The selection for the BEST-AVG axis is stored per sport in a hash table, to
    ;; be restored when a similar sport is selected.  This hash table is also
    ;; stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    (define best-avg-axis-choices '())
    (define best-avg-axis-index 0)

    (define show-grid? #f)
    (define zero-base? #f)

    ;; Gui elements
    (define best-avg-axis-choice #f)
    (define show-grid-check-box #f)

    ;; Graph data
    (define best-avg-data '())
    (define session #f)

    (define best-avg-cache (make-hash))

    (define best-avg-render-tree #f)

    ;; Restore the preferences now.
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 3))
        (set! axis-choice-by-sport (hash-copy (first pref)))
        (set! show-grid? (second pref))
        (set! zero-base? (third pref))))

    (define panel (new (class vertical-panel%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image))
                         (define/public (interactive-export-data formatted?)
                           (on-interactive-export-data formatted?)))
                       [parent parent]
                       [border 5]
                       [spacing 5]
                       [alignment '(center top)]))

    (let ((cp (new horizontal-panel%
                   [parent panel] [spacing 10] [border 0]
                   [alignment '(center center)]
                   [stretchable-height #f])))
      (set! best-avg-axis-choice
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "Best Avg: "]
                 [callback (lambda (c e)
                             (on-best-avg-axis-changed (send c get-selection)))]))
      (set! show-grid-check-box
            (new check-box% [parent cp]
                 [value show-grid?]
                 [label "Show Grid"]
                 [callback (lambda (c e)
                             (set! show-grid? (send c get-value))
                             (set! best-avg-render-tree #f)
                             (refresh-plot))]))

      (set! show-grid-check-box
            (new check-box% [parent cp]
                 [value zero-base?]
                 [label "Zero Base"]
                 [callback (lambda (c e)
                             (set! zero-base? (send c get-value))
                             (set! best-avg-render-tree #f)
                             (refresh-plot))]))
      )
    
    ;; Pasteboard to display the actual BEST-AVG plot
    (define graph-pb (new snip-canvas% [parent panel]))

    ;; Update the axis selection checkboxes with AXIS-CHOICE-LIST
    (define (install-axis-choices axis-choice-list)
      (send best-avg-axis-choice clear)
      (for ((a (in-list axis-choice-list)))
        (let ((n (send a get-axis-label)))
          (send best-avg-axis-choice append n))))

    (define (on-best-avg-axis-changed new-axis-index)
      (unless (equal? best-avg-axis-index new-axis-index)
        (set! best-avg-axis-index new-axis-index)
        (invalidate-best-avg-data)
        (refresh-plot)))

    (define (invalidate-best-avg-data)
      (set! best-avg-render-tree #f)
      (set! best-avg-data #f))

    (define (get-plot-snip)
      (if (not best-avg-render-tree)
          #f
          (let ((rt (list best-avg-render-tree)))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((best-avg-axis (list-ref best-avg-axis-choices best-avg-axis-index)))
              (parameterize ([plot-x-ticks (best-avg-ticks)]
                             [plot-x-label "Duration"]
                             [plot-x-transform log-transform]
                             [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                             [plot-y-label (send best-avg-axis get-axis-label)])
                (plot-snip rt))))))

    (define (refresh-plot)
      (cond ((eq? best-avg-render-tree 'no-data)
             (send graph-pb set-background-message "No data for graph")
             (send graph-pb set-snip #f))
            ((eq? best-avg-render-tree #f)
             (send graph-pb set-background-message "Working...")
             (send graph-pb set-snip #f)
             (thread
              (lambda ()
                (unless best-avg-data (rebuild-best-avg-data))
                (if best-avg-data
                    (let ((plot-data (for/list ([e best-avg-data] #:when (vector-ref e 1))
                                       (match-define (vector d m s) e)
                                       (vector d m))))
                      (if (< (length plot-data) 3) ; we need at least 3 points to interpolate!
                          (set! best-avg-render-tree 'no-data)
                          (let ((spline (mk-spline-fn plot-data))
                                (kwd '())
                                (val '()))
                            (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
                            (when zero-base? (add-arg '#:y-min 0))
                            (add-arg '#:width 3)
                            (add-arg '#:color (get-axis-plot-color (list-ref best-avg-axis-choices best-avg-axis-index)))
                            (set! best-avg-render-tree
                                  (keyword-apply function kwd val spline
                                                 (vector-ref (first plot-data) 0)
                                                 (vector-ref (last plot-data) 0)
                                                 '())))))
                    (set! best-avg-render-tree 'no-data))
                (queue-callback refresh-plot))))
            (#t
             (let ((snip (get-plot-snip)))
               (send graph-pb set-background-message "...")
               (send graph-pb set-snip snip)))))
      
    (define (rebuild-best-avg-data)
      (set! best-avg-data #f)
      (when session
        (define series-axis (list-ref best-avg-axis-choices best-avg-axis-index))
        (let ((cached-data (hash-ref best-avg-cache series-axis (lambda () #f))))
          (when cached-data
            (set! best-avg-data cached-data)))
        (unless best-avg-data
          (define inverted? (send series-axis inverted-best-avg?))
          ;; HACK: inverted? plots are negatively affected by the stop
          ;; detection code, which puts 0 points in the data series.  For
          ;; inverted graphs, this results in those sections always being
          ;; selected and a 0 min/km pace is really fast :-).
          (define base-axis
            (if inverted? axis-elapsed-time-no-stop-detection axis-elapsed-time))
          (define data-series
            (let-values ([(data lap-markers min-x max-x min-y max-y)
                          (extract-data session base-axis series-axis 0)])
              data))
          (set! best-avg-data (if (and data-series (not (null? data-series)))
                             (make-best-avg data-series inverted?)
                             #f))
          (hash-set! best-avg-cache series-axis best-avg-data))))

    (define/public (save-visual-layout)
      (when current-sport
        (hash-set! axis-choice-by-sport current-sport best-avg-axis-index))
      (al-put-pref pref-tag (list axis-choice-by-sport show-grid? zero-base?)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send graph-pb export-image-to-file file))))

    (define/public (on-interactive-export-data formatted?)
      (let ((file (put-file "Select file to export to" #f #f #f "csv" '()
                            '(("CSV Files" "*.csv") ("Any" "*.*")))))
        (when file
          (call-with-output-file file
            (lambda (out) (export-data-as-csv out formatted?))
            #:mode 'text #:exists 'truncate))))

    (define (export-data-as-csv out formatted?)
      (write-string "Start Timestamp, Duration, Value" out)
      (newline out)
      (when best-avg-data
        (for ([e best-avg-data])
          (match-define (vector d m s) e)
          (write-string (format "~a, ~a, ~a~%" s d m) out))))

    (define generation -1)

    (define/public (set-session s)

      ;; maybe save previous sport settings
      (when current-sport
        (hash-set! axis-choice-by-sport
                   current-sport best-avg-axis-index))

      (set! current-sport (cons (session-sport s) (session-sub-sport s)))
      (set! generation (+ 1 generation))
      (set! session s)

      (set! best-avg-axis-choices (get-best-avg-axis-choices))
      (install-axis-choices best-avg-axis-choices)

      (let ((xy-index (hash-ref axis-choice-by-sport current-sport
                                (lambda () 0))))
        (set! best-avg-axis-index xy-index))

      (send best-avg-axis-choice set-selection best-avg-axis-index)

      (invalidate-best-avg-data)
      (set! best-avg-cache (make-hash))
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
