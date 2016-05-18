#lang racket/base
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
         "widgets.rkt")

(provide histogram-plot-panel%)

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
   axis-vratio
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
    (define outlier-trim 0)

    (define axis-choices '())

    (define y-axis-choice #f)
    (define show-grid-check-box #f)
    (define bucket-width-field #f)
    (define show-as-percentage-check-box #f)
    (define include-zeroes-check-box #f)
    (define outlier-trim-field #f)

    (define inhibit-refresh #f)
    (define lap-swimming? #f)           ; when #t, we are inspecting a lap swimming activity

    ;; The selection for the Y axis and bucket width is stored per sport in a
    ;; hash table, to be restored when a similar sport is selected.  This hash
    ;; table is also stored as a preference to persist accross sessions.
    (define current-sport #f)
    (define axis-choice-by-sport (make-hash))

    ;; Some parameters are saved based on a per axis basis
    (define params-by-axis (make-hash))

    ;; Restore the preferences now. 
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (> (length pref) 0) (eq? (car pref) 'gen1))
        (match-define (list tag abs pba grid? as-pct?) pref)
        (set! axis-choice-by-sport (hash-copy abs))
        (set! params-by-axis (hash-copy pba))
        (set! show-grid? grid?)
        (set! show-as-percentage? as-pct?)))

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

      (set! show-grid-check-box
            (new check-box% [parent cp]
                 [value show-grid?]
                 [label "Show Grid"]
                 [callback (lambda (c e)
                             (set! show-grid? (send c get-value))
                             (refresh-plot))]))
      
      (set! show-as-percentage-check-box
            (new check-box% [parent cp]
                 [value show-as-percentage?]
                 [label "Show as Percentage"]
                 [callback (lambda (c e)
                             (set! show-as-percentage? (send c get-value))
                             (refresh-plot))]))

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

      (set! include-zeroes-check-box
            (new check-box% [parent cp]
                 [value include-zeroes?]
                 [label "Include Zeroes"]
                 [callback (lambda (c e)
                             (set! include-zeroes? (send c get-value))
                             (refresh-plot))]))

      (set! outlier-trim-field
            (new number-input-field%
                 [parent cp] 
                 [label "Outlier Trim (%) "]
                 [cue-text "0 .. 100%"]
                 [min-value 0]
                 [max-value 100]
                 [stretchable-width #f]
                 ;; [min-width 100]
                 [valid-value-cb 
                  (lambda (v) (unless (eq? v 'empty) (on-outlier-trim (/ v 100))))]))
      )

    (define graph-render-tree #f)

    ;; Pasteboard to display the actual plot
    (define graph-pb (new snip-canvas% [parent panel]))

    (define (get-axis-label index)
      (let ((axis (list-ref axis-choices index)))
        (if (list? axis)
            (car axis)
            (send axis get-axis-label))))

    ;; Update the axis selection checkboxes with AXIS-CHOICE-LIST
    (define (install-axis-choices axis-choice-list)
      (send y-axis-choice clear)
      (for ([a axis-choice-list])
        (let ((n (if (list? a)
                     (car a)
                     (send a get-axis-label))))
          (send y-axis-choice append n))))

    (define (on-y-axis-changed new-axis-index)
      (unless (equal? y-axis-index new-axis-index)
        (set! inhibit-refresh #t)
        (save-params-for-axis current-sport y-axis-index)
        (restore-params-for-axis current-sport new-axis-index)
        (set! y-axis-index new-axis-index)
        (set! inhibit-refresh #f)
        (refresh-plot)))

    (define (on-bucket-width new-width)
      (unless (equal? bucket-width new-width)
        (set! bucket-width new-width)
        (refresh-plot)))

    (define (on-outlier-trim a)
      (set! outlier-trim a)
      (refresh-plot))

    (define (save-params-for-axis sport index)
      (define axis-name (get-axis-label index))
      (define key (cons sport axis-name))
      (hash-set! params-by-axis key
                 (list bucket-width include-zeroes? outlier-trim)))

    (define (restore-params-for-axis sport index)
      (define axis-name (get-axis-label index))
      (define key (cons sport axis-name))
      (define val (hash-ref params-by-axis key (lambda () #f)))
      (when val
        (match-define (list bw incz trim) val)
        (send bucket-width-field set-numeric-value bw)
        (on-bucket-width bw)
        (send include-zeroes-check-box set-value incz)
        (set! include-zeroes? incz)
        (send outlier-trim-field set-numeric-value (* trim 100))
        (on-outlier-trim trim)))

    (define (put-plot-snip canvas)
      (if (not graph-render-tree)
          (begin
            (send canvas set-background-message "No data for graph")
            (send canvas set-snip #f))
          (let ((rt graph-render-tree))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((y-axis (list-ref axis-choices y-axis-index)))
              (when (list? y-axis) (set! y-axis (second y-axis)))
              (parameterize ([plot-y-label (if show-as-percentage? "pct %"
                                               (if lap-swimming? "# of lengths" "time (seconds)"))]
                             [plot-x-ticks (send y-axis get-axis-ticks)]
                             [plot-x-label (send y-axis get-axis-label)])
                            (plot-snip/hack canvas rt))))))

    (define (refresh-plot)

      ;; HACK: some get-line-color methods return 'smart, we should fix this
      (define (get-color axis)
        (let ((color (send axis get-line-color)))
          (if (or (not color) (eq? color 'smart))
              '(0 148 255)
              color)))
      
      (unless inhibit-refresh
        (set! graph-render-tree #f)
        (let* ((axis (list-ref axis-choices y-axis-index))
               (axis1 (if (list? axis) (second axis) axis))
               (axis2 (if (list? axis) (third axis) #f))
               (sname1 (send axis1 get-series-name))
               (sname2 (if axis2 (send axis2 get-series-name) #f))
               (bw (* bucket-width (send axis1 get-histogram-bucket-slot)))
               (h1 (df-histogram data-frame sname1
                                 #:bucket-width bw
                                 #:include-zeroes? include-zeroes?
                                 #:as-percentage? show-as-percentage?
                                 #:trim-outliers outlier-trim))
               (h2 (if sname2
                       (df-histogram data-frame sname2
                                     #:bucket-width bw
                                     #:include-zeroes? include-zeroes?
                                     #:as-percentage? show-as-percentage?
                                     #:trim-outliers outlier-trim)
                       #f)))
          (set! graph-render-tree
                (if (and axis2 h1 h2)
                    (make-histogram-renderer/dual h1 (send axis1 get-series-label)
                                                  h2 (send axis2 get-series-label)
                                                  #:color1 (get-color axis1)
                                                  #:color2 (get-color axis2))
                    (if h1
                        (list (make-histogram-renderer h1 #:color (get-color axis1)))
                        #f)))
          (put-plot-snip graph-pb))))

    (define (save-params-for-sport)
      (when current-sport
        (save-params-for-axis current-sport y-axis-index)
        (let ((name (get-axis-label y-axis-index)))
          (hash-set! axis-choice-by-sport current-sport name))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (al-put-pref pref-tag
                   (list 'gen1 axis-choice-by-sport params-by-axis show-grid? show-as-percentage?)))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send graph-pb export-image-to-file file))))

    (define generation -1)
    (define data-frame #f)

    (define/public (set-session session df)
      (set! inhibit-refresh #t)
      
      ;; maybe save previous sport settings
      (save-params-for-sport)
      (set! generation (+ 1 generation))
      (set! data-frame df)
      (set! lap-swimming? (send data-frame get-property 'is-lap-swim?)) 
      (set! axis-choices
            (filter-axis-choices
             data-frame
             (if lap-swimming? (swim-axis-choices) (default-axis-choices))))
      (install-axis-choices axis-choices)
      (set! current-sport (send data-frame get-property 'sport))

      (let ((name (hash-ref axis-choice-by-sport current-sport 0)))
        (let ((index (find-axis axis-choices name)))
          (set! y-axis-index (or index 0))))

      (send y-axis-choice set-selection y-axis-index)
      (restore-params-for-axis current-sport y-axis-index)
      (set! inhibit-refresh #f)
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
