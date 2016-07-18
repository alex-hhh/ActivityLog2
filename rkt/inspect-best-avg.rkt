#lang racket/base
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

(require plot
         racket/class
         racket/gui/base
         racket/list
         racket/match
         "activity-util.rkt"
         "al-prefs.rkt"
         "plot-axis-def.rkt"
         "data-frame.rkt"
         "plot-hack.rkt"
         "snip-canvas.rkt"
         "spline-interpolation.rkt"
         "workers.rkt")

(provide best-avg-plot-panel%)

;; Filter AXIS-LIST to remove any axis definition that don't have a data
;; series in DF, a data-frame%
(define (filter-axis-list df axis-list)
  (define al
    (for/list ([axis axis-list]
               #:when
               (if (list? axis)
                   (let ()
                     (match-define (list name a1 a2) axis)
                     (send df contains?
                           (send a1 get-series-name)
                           (send a2 get-series-name)))
                   (send df contains? (send axis get-series-name))))
      axis))
  (sort al string<?
        #:key (lambda (a) (if (list? a) (first a) (send a get-axis-title)))))

;; Find an axis that works in SERIES-NAME and return its position in
;; AXIS-LIST.  Return #f is not found
(define (find-axis series-name axis-list)
  (for/first ([(axis index) (in-indexed axis-list)]
              #:when
              (let ((sn (if (list? axis)
                            (car axis)
                            (send axis get-axis-label))))
                (equal? series-name sn)))
    index))

;; Axis choices for all non lap swimming sports.  note that some axis choices
;; don't make sense, so they are not listed here, to keep the list smaller.
(define default-axis-choices
  (list
   axis-speed
   axis-pace
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

(define best-avg-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:best-avg-plot)

    ;; Variables that control the look of the plot
    (define axis-choices '())
    (define selected-axis 0)
    (define selected-aux-axis 0)
    (define show-grid? #f)
    (define zero-base? #f)

    ;; The selection for the BEST-AVG axis is stored per sport in a hash
    ;; table, to be restored when a similar sport is selected.  This hash
    ;; table is also stored as a preference to persist accross sessions.
    (define axis-by-sport (make-hash))

    ;; Restore the preferences now.
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 3))
        (set! axis-by-sport (hash-copy (first pref)))
        (set! show-grid? (second pref))
        (set! zero-base? (third pref))))

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
      
    (define show-grid-check-box
      (new check-box% [parent control-panel]
           [value show-grid?] [label "Show Grid"]
           [callback (lambda (c e) (on-show-grid (send c get-value)))]))

    (define zero-base-check-box
      (new check-box% [parent control-panel]
           [value zero-base?] [label "Zero Base"]
           [callback (lambda (c e) (on-zero-base (send c get-value)))]))

    ;; Pasteboard to display the actual BEST-AVG plot
    (define plot-pb (new snip-canvas% [parent panel]))
        
    ;; Graph data
    (define data-frame #f)
    (define best-avg-data '())
    (define best-avg-aux-data '())
    (define inhibit-refresh #f)
    (define plot-rt #f)
    ;; keep best avg values around, as they are expensive to calculate
    (define data-cache (make-hash))

    (define (current-sport)
      (if data-frame (send data-frame get-property 'sport) #f))
    
    (define (get-series-axis)
      (list-ref axis-choices selected-axis))
    
    (define (get-aux-axis)
      (if (zero? selected-aux-axis)
          #f
          (list-ref axis-choices (sub1 selected-aux-axis))))
    
    ;; Update the axis selection checkboxes with AXIS-LIST
    (define (install-axis-choices axis-list)
      (send axis-choice-box clear)
      (send aux-axis-choice-box clear)
      (send aux-axis-choice-box append "None")
      (for ([a axis-list])
        (let ((n (send a get-axis-label)))
          (send axis-choice-box append n)
          (send aux-axis-choice-box append n))))

    (define (on-axis-changed new-index)
      (unless (equal? selected-axis new-index)
        (set! selected-axis new-index)
        (refresh-plot)))

    (define (on-aux-axis-changed new-index)
      (unless (equal? selected-aux-axis new-index)
        (set! selected-aux-axis new-index)
        (refresh-plot)))

    (define (on-show-grid flag)
      (unless (equal? show-grid? flag)
        (set! show-grid? flag)
        (refresh-plot)))

    (define (on-zero-base flag)
      (unless (equal? zero-base? flag)
        (set! zero-base? flag)
        (refresh-plot)))
    
    (define (put-plot-snip)
      (when plot-rt
        (let ((rt (list plot-rt)))
          (when show-grid?
            (set! rt (cons (tick-grid) rt)))
          (let ((best-avg-axis (get-series-axis))
                (aux-axis (get-aux-axis)))
            ;; aux data might not exist, if an incorrect/invalid aux-axis is
            ;; selected
            (if best-avg-aux-data
                (let ((ivs (mk-inverse best-avg-aux-data best-avg-data zero-base?)))
                  (parameterize ([plot-x-ticks (best-avg-ticks)]
                                 [plot-x-label "Duration"]
                                 [plot-x-transform log-transform]
                                 [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                                 [plot-y-label (send best-avg-axis get-axis-label)]
                                 [plot-y-far-ticks (transform-ticks (send aux-axis get-axis-ticks) ivs)]
                                 [plot-y-far-label (send aux-axis get-axis-label)])
                    (plot-snip/hack plot-pb rt)))
                (parameterize ([plot-x-ticks (best-avg-ticks)]
                               [plot-x-label "Duration"]
                               [plot-x-transform log-transform]
                               [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                               [plot-y-label (send best-avg-axis get-axis-label)])
                  (plot-snip/hack plot-pb rt)))))))

    (define (refresh-plot)
      (set! plot-rt #f)
      (send plot-pb set-background-message "Working...")
      (send plot-pb set-snip #f)
      (unless inhibit-refresh
        ;; Capture all needed data, as we will work in a different thread.
        (let ((df data-frame)
              (cache data-cache)
              (axis (get-series-axis))
              (aux-axis (get-aux-axis))
              (zerob? zero-base?))
          (queue-task
           "inspect-best-avg%/refresh-plot"
           (lambda ()
             (define data
               (or (hash-ref cache axis #f)
                   (let ((inverted? (send axis inverted-best-avg?))
                         (series (send axis get-series-name)))
                     (and (send df contains? series)
                          (df-best-avg df series #:inverted? inverted?)))))
             (hash-set! cache axis data)
             ;; rebuild auxiliary data here
             (define aux-data
               (and data aux-axis
                    (let ((series (send aux-axis get-series-name)))
                      (and (send df contains? series)
                           (df-best-avg-aux df series data)))))
             (define rt
               (and data
                    (make-best-avg-renderer
                     data aux-data
                     #:color1 (send axis get-line-color)
                     #:color2 (and aux-axis (send aux-axis get-line-color))
                     #:zero-base? zerob?)))
             (queue-callback
              (lambda ()
                (cond (rt
                       (set! best-avg-data data)
                       (set! best-avg-aux-data aux-data)
                       (set! plot-rt rt)
                       (put-plot-snip))
                      (#t
                       (send plot-pb set-background-message "No data for plot"))))))))))

    (define (save-params-for-sport)
      (when (current-sport)
        (when (current-sport)
        (let ((axis (get-series-axis)))
          (let ((name (if (list? axis) (first axis) (send axis get-axis-label))))
            (hash-set! axis-by-sport (current-sport) name))))))

    (define (restore-params-for-sport)
      (let ((name (hash-ref axis-by-sport (current-sport) #f)))
        (when name
          (let ((index (find-axis name axis-choices)))
            (set! selected-axis (or index 0))
            (send axis-choice-box set-selection selected-axis)
            (set! selected-aux-axis 0)
            (send aux-axis-choice-box set-selection selected-aux-axis)))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (al-put-pref pref-tag (list axis-by-sport show-grid? zero-base?)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send plot-pb export-image-to-file file))))

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

    (define/public (set-session s df)
      (set! inhibit-refresh #t)
      ;; maybe save previous sport settings
      (save-params-for-sport)
      (set! generation (+ 1 generation))
      (set! data-frame df)
      (set! axis-choices (filter-axis-list data-frame default-axis-choices))
      (install-axis-choices axis-choices)
      (set! data-cache (make-hash))
      (restore-params-for-sport)
      (set! inhibit-refresh #f)
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
