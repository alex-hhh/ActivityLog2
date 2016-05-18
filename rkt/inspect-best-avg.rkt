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
         "spline-interpolation.rkt")

(provide best-avg-plot-panel%)

(define (filter-axis-choices data-frame axis-choices)
  (for/list ([axis axis-choices]
             #:when
             (if (list? axis)
                 (let ()
                   (match-define (list name a1 a2) axis)
                   (send data-frame contains?
                         (send a1 get-series-name)
                         (send a2 get-series-name)))
                 (send data-frame contains? (send axis get-series-name))))
    axis))

(define (find-axis axis-choices axis-name)
  (for/first ([axis axis-choices]
              [index (in-range (length axis-choices))]
              #:when
              (if (list? axis)
                  (let ()
                    (define name (car axis))
                    (equal? name axis-name))
                  (equal? axis-name (send axis get-axis-label))))
    index))


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
  (class object%
    (init parent)
    (super-new)

    (define pref-tag 'activity-log:best-avg-plot)

    ;; The selection for the BEST-AVG axis is stored per sport in a hash table, to
    ;; be restored when a similar sport is selected.  This hash table is also
    ;; stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    (define axis-choices '())
    (define selected-axis 0)
    (define selected-aux-axis 0)

    (define show-grid? #f)
    (define zero-base? #f)

    ;; Gui elements
    (define axis-choice-box #f)
    (define aux-axis-choice-box #f)
    (define show-grid-check-box #f)

    ;; Graph data
    (define best-avg-data '())
    (define best-avg-aux-data '())
    (define data-frame #f)

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
      (set! axis-choice-box
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "Best Avg: "]
                 [callback (lambda (c e)
                             (on-best-avg-axis-changed (send c get-selection)))]))

      (set! aux-axis-choice-box
            (new choice% [parent cp]
                 [choices '()]
                 [min-width 300]
                 [label "Auxiliary: "]
                 [callback (lambda (c e)
                             (on-best-avg-aux-axis-changed (send c get-selection)))]))
      
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

    (define (get-series-axis)
      (list-ref axis-choices selected-axis))
    
    (define (get-aux-axis)
      (if (zero? selected-aux-axis)
          #f
          (list-ref axis-choices (sub1 selected-aux-axis))))
    
    ;; Pasteboard to display the actual BEST-AVG plot
    (define graph-pb (new snip-canvas% [parent panel]))

    ;; Update the axis selection checkboxes with AXIS-CHOICE-LIST
    (define (install-axis-choices axis-choice-list)
      (send axis-choice-box clear)
      (send aux-axis-choice-box clear)
      (send aux-axis-choice-box append "None")
      (for ([a axis-choice-list])
        (let ((n (send a get-axis-label)))
          (send axis-choice-box append n)
          (send aux-axis-choice-box append n))))

    (define (on-best-avg-axis-changed new-axis-index)
      (unless (equal? selected-axis new-axis-index)
        (set! selected-axis new-axis-index)
        (invalidate-best-avg-data)
        (refresh-plot)))

    (define (on-best-avg-aux-axis-changed new-axis-index)
      (unless (equal? selected-aux-axis new-axis-index)
        (set! selected-aux-axis new-axis-index)
        (invalidate-best-avg-data)
        (refresh-plot)))
    
    (define (invalidate-best-avg-data)
      (set! best-avg-render-tree #f)
      (set! best-avg-data #f)
      (set! best-avg-aux-data #f))

    (define (put-plot-snip canvas)
      (if (not best-avg-render-tree)
          #f
          (let ((rt (list best-avg-render-tree)))
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
                      (plot-snip/hack canvas rt)))
                  (parameterize ([plot-x-ticks (best-avg-ticks)]
                                 [plot-x-label "Duration"]
                                 [plot-x-transform log-transform]
                                 [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                                 [plot-y-label (send best-avg-axis get-axis-label)])
                    (plot-snip/hack canvas rt)))))))

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
                (set! best-avg-render-tree
                      (make-best-avg-renderer
                       best-avg-data
                       best-avg-aux-data
                       #:color1 (send (get-series-axis) get-line-color)
                       #:color2 (if (get-aux-axis) (send (get-aux-axis) get-line-color) #f)
                       #:zero-base? zero-base?))
                (unless best-avg-render-tree
                  (set! best-avg-render-tree 'no-data))
                (queue-callback refresh-plot))))
            (#t
             (send graph-pb set-background-message "...")
             (put-plot-snip graph-pb))))
      
    (define (rebuild-best-avg-data)
      (set! best-avg-data #f)
      (set! best-avg-aux-data #f)
      (when data-frame
        (define series-axis (get-series-axis))
        (let ((cached-data (hash-ref best-avg-cache series-axis (lambda () #f))))
          (when cached-data
            (set! best-avg-data cached-data)))
        (define inverted? (send series-axis inverted-best-avg?))
        (unless best-avg-data
          (define series-name (send series-axis get-series-name))
          (when (send data-frame contains? series-name)
            (set! best-avg-data (df-best-avg data-frame series-name #:inverted? inverted?))
            (hash-set! best-avg-cache series-axis best-avg-data)))
        ;; Rebuild auxiliary data here
        (define aux-axis (get-aux-axis))
        (when (and best-avg-data aux-axis)
          (define series-name (send aux-axis get-series-name))
          (when (send data-frame contains? series-name)
            (set! best-avg-aux-data (df-best-avg-aux data-frame series-name best-avg-data))))))

    (define/public (save-visual-layout)
      (when current-sport
        (let ((axis (list-ref axis-choices selected-axis)))
          (let ((name (if (list? axis) (first axis) (send axis get-axis-label))))
            (hash-set! axis-choice-by-sport current-sport name))))
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

    (define/public (set-session s df)

      ;; maybe save previous sport settings
      (when current-sport
        (let ((axis (list-ref axis-choices selected-axis)))
          (let ((name (if (list? axis) (first axis) (send axis get-axis-label))))
            (hash-set! axis-choice-by-sport current-sport name))))

      (set! current-sport (cons (session-sport s) (session-sub-sport s)))
      (set! generation (+ 1 generation))
      (set! data-frame df)

      (set! axis-choices (filter-axis-choices data-frame (get-best-avg-axis-choices)))
      (install-axis-choices axis-choices)

      (let ((name (hash-ref axis-choice-by-sport current-sport #f)))
        (when name
          (let ((index (find-axis axis-choices name)))
            (set! selected-axis (or index 0)))))

      (send axis-choice-box set-selection selected-axis)
      (send aux-axis-choice-box set-selection 0)
      (set! selected-aux-axis 0)

      (invalidate-best-avg-data)
      (set! best-avg-cache (make-hash))
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
