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

(require racket/class
         racket/gui/base
         racket/function
         racket/math
         racket/string
         racket/match
         plot
         "data-frame.rkt"
         "snip-canvas.rkt"
         "plot-hack.rkt"
         "plot-axis-def.rkt"
         "sport-charms.rkt"
         "widgets.rkt"
         "workers.rkt"
         "session-df.rkt"
         "al-prefs.rkt"
         "fmt-util.rkt")

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

;; Convert a string in the format "mm:ss" into a number of seconds
(define (str->seconds data)
  (let ((t (string-trim data)))
    (cond ((= (string-length t) 0) 'empty)
          ((regexp-match "^([0-9]+):([0-9]+)$" t) =>
           (lambda (m)
             (let ((minutes (string->number (list-ref m 1)))
                   (seconds (string->number (list-ref m 2))))
               (if (and (< minutes 60) (< seconds 60))
                   (+ (* minutes 60) seconds)
                   #f))))
          (#t #f))))

;; Convert a pace value (mm:ss/km) into meters per second
(define (run-pace-string->mps str)
  (let ((seconds (str->seconds str)))
    (if seconds (/ 1000.0 seconds) #f)))

;; Convert a pace value (mm:ss/100m) into meters per second
(define (swim-pace-string->mps str)
  (let ((seconds (str->seconds str)))
    (if seconds (/ 100.0 seconds) #f)))

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

(define zone-colors
  (vector (make-object color% #xad #xd8 #xe6) ; z0, light blue
          (make-object color% #x00 #xbf #xff) ; z1, deep sky blue
          (make-object color% #x22 #x8b #x22) ; z2, forrest green
          (make-object color% #xff #x7f #x50) ; z3, coral
          (make-object color% #xcd #x5c #x5c) ; z4, indian red
          (make-object color% #xdc #x14 #x3c) ; z5, crimson
          (make-object color% #x8b #x00 #x00) ; z6, dark red
          (make-object color% #x99 #x32 #xcc) ; z7, dark orchid
          (make-object color% #x00 #x00 #x8b) ; z8, dark blue
          (make-object color% #xff #x8c #x00) ; z9, dark orange
          (make-object color% #xda #xa5 #x20) ; z10, golden rod
          ))

(define (make-sport-zone-renderers zones yval-fn)
  (define zone-fns (for/list ([z zones] #:when (>= z 0))
                    (curry yval-fn z)))
  (for/list ([low zone-fns]
             [high (cdr zone-fns)]
             [idx (in-range (length zone-fns))])
    (function-interval low high
                       #:color (vector-ref zone-colors idx)
                       #:alpha 0.1)))

(define quadrant-plot-panel%
  (class object% (init parent) (super-new)
    (define pref-tag 'activity-log:quadrant-plot)

    ;; Variables that control the look of the plot
    (define show-grid? #f)
    (define show-zones? #f)
    (define threshold-speed #f)
    (define threshold-power #f)
    (define threshold-cadence #f)
    (define params-by-sport (make-hash))

    ;; Restore the preferences now. 
    (let ((pref (al-get-pref pref-tag (lambda () #f))))
      (when (and pref (> (length pref) 0) (eq? (car pref) 'gen1))
        (match-define (list tag pbs sg? sz?) pref)
        (set! params-by-sport (hash-copy pbs))
        (set! show-grid? sg?)
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
      (new validating-input-field% [parent control-panel] 
           [label "Threshold Pace: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [cue-text "min/km"]
           [validate-fn (lambda (v)
                          (let ((t (string-trim v)))
                            (or (= (string-length t) 0)
                                (run-pace-string->mps t))))]
           [convert-fn (lambda (v)
                         (let ((t (string-trim v)))
                           (if (= (string-length t) 0)
                               'empty
                               (run-pace-string->mps t))))]
           [valid-value-cb (lambda (v) (on-threshold-speed v))]))

    (define swim-pace-field
      (new validating-input-field% [parent control-panel] 
           [label "Threshold Pace: "] [style '(single deleted)]
           [min-width 100] [stretchable-width #f]
           [cue-text "min/100m"]
           [validate-fn (lambda (v)
                          (let ((t (string-trim v)))
                            (or (= (string-length t) 0)
                                (swim-pace-string->mps t))))]
           [convert-fn (lambda (v)
                         (let ((t (string-trim v)))
                           (if (= (string-length t) 0)
                               'empty
                               (swim-pace-string->mps t))))]
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
    
    (define show-grid-check-box
      (new check-box% [parent control-panel] [value show-grid?]
           [label "Show Grid"]
           [callback (lambda (c e) (on-show-grid (send c get-value)))]))

    (define (on-show-grid flag)
      (unless (equal? show-grid? flag)
        (set! show-grid? flag)
        (put-plot-snip)))

    (define (on-show-zones flag)
      (unless (equal? show-zones? flag)
        (set! show-zones? flag)
        (put-plot-snip)))

    (define (on-threshold-cadence cadence)
      (unless (equal? threshold-cadence cadence)
        (set! threshold-cadence cadence)
        (put-plot-snip)))

    (define (on-threshold-speed speed)
      (unless (equal? threshold-speed speed)
        (set! threshold-speed (if (number? speed) speed #f))
        (if (number? threshold-speed)
            (set! threshold-fn (curry yval-fn threshold-speed))
            (set! threshold-fn #f))
        (put-plot-snip)))

    (define (on-threshold-power power)
      (unless (equal? threshold-power power)
        (set! threshold-power (if (number? power) power #f))
        (if (number? threshold-power)
            (set! threshold-fn (curry yval-fn threshold-power))
            (set! threshold-fn #f))
        (put-plot-snip)))

    ;; Pasteboard to display the actual plot
    (define plot-pb (new snip-canvas% [parent panel]))

    ;; Data from the session we inspect
    (define generation -1)
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
    ;; Function that plots the line for threshold speed or power
    (define threshold-fn #f)
    ;; Filter function used to extract the data for the plot.  For bike, it
    ;; will filter our unrealistic torque values.
    (define filter-fn valid-only)
    (define plot-rt #f)                 ; plot render tree
    (define zone-rt #f)                 ; sport zone render tree
    (define inhibit-refresh #f)


    (define (put-plot-snip)
      (when (and plot-rt (not inhibit-refresh))
        (let ((rt (list plot-rt)))
          (when show-grid?
            (set! rt (cons (tick-grid) rt)))
          (when (and show-zones? zone-rt)
            (set! rt (cons zone-rt rt)))
          (when threshold-fn
            (set! rt (cons (function threshold-fn) rt)))
          (when (and threshold-fn threshold-cadence)
            (set! rt (cons (vrule threshold-cadence)
                           (cons (hrule (threshold-fn threshold-cadence))
                                 rt))))
          (parameterize ([plot-x-ticks (send x-axis get-axis-ticks)]
                         [plot-x-label (send x-axis get-axis-label)]
                         [plot-y-ticks (send y-axis get-axis-ticks)]
                         [plot-y-label (send y-axis get-axis-label)])
            (match-define (vector x-min x-max y-min y-max) data-bounds)
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
              (df data-frame))
          (queue-task
           "quadrant-plot-panel%/refresh-plot"
           (lambda ()
             (define ds
               (and x y
                    (let ((xnam (send x get-series-name))
                          (ynam (send y get-series-name)))
                      (and  (send df contains? xnam ynam)
                            (send df select* xnam ynam #:filter filter-fn)))))
             (define bounds (and ds (find-bounds ds)))
             (define grouped
               (and ds
                    (group-samples ds
                                   (send x get-fractional-digits)
                                   (send y get-fractional-digits))))
             (define rt
               (and grouped
                    (make-scatter-group-renderer
                     grouped
                     #:color (send y-axis get-line-color))))
             (queue-callback
              (lambda ()
                (set! plot-rt rt)
                (set! data-series ds)
                (set! data-bounds bounds)
                (unless plot-rt
                  (send plot-pb set-background-message "No data to plot..."))
                (put-plot-snip))))))))

    (define (save-params-for-sport)
      (when data-frame
        (let ((sport (send data-frame get-property 'sport))
              (data (list threshold-speed threshold-power threshold-cadence)))
          (hash-set! params-by-sport sport data))))

    (define (restore-params-for-sport)
      (when data-frame
        (let* ((sport (send data-frame get-property 'sport))
               (data (hash-ref params-by-sport sport (lambda () (list #f #f #f)))))
          (match-define (list tspeed tpower tcad) data)
          (if tspeed
              (cond ((equal? (vector-ref sport 0) 1) ; running
                     (send run-pace-field set-value (pace->string tspeed)))
                    ((equal? (vector-ref sport 0) 5) ; swimming
                     (send swim-pace-field set-value (swim-pace->string tspeed))))
              (begin
                (send run-pace-field set-value "")
                (send swim-pace-field set-value "")))
          (if tpower
              (send power-field set-numeric-value tpower)
              (send power-field set-value ""))
          (if tcad
              (send cadence-field set-numeric-value tcad)
              (send cadence-field set-value ""))
          (set! threshold-speed #f)
          (set! threshold-power #f)
          (set! threshold-cadence #f)
          (let ((old inhibit-refresh))
            (set! inhibit-refresh #t)
            (on-threshold-speed tspeed)
            (on-threshold-power tpower)
            (on-threshold-cadence tcad)
            (set! inhibit-refresh old)))))

    (define/public (save-visual-layout)
      (save-params-for-sport)
      (let ((data (list 'gen1 params-by-sport show-grid? show-zones?)))
        (al-put-pref pref-tag data)))

    (define/public (set-session session df)
      (save-params-for-sport)
      (set! inhibit-refresh #t)
      (set! generation (+ 1 generation))
      (set! data-frame df)
      (set! data-series #f)
      (define current-sport (send data-frame get-property 'sport))
      (define session-id (send data-frame get-property 'session-id)) 
      (cond
        ((and (equal? (vector-ref current-sport 0) 1) ; running
              (send data-frame contains? "spd" "cad"))
         (set! x-axis axis-cadence)
         (set! y-axis axis-stride)
         (set! zones (get-session-sport-zones session-id 2))
         (set! yval-fn cadence->stride)
         (set! filter-fn valid-only)
         (send control-panel change-children
               (lambda (old) (list run-pace-field cadence-field
                                   show-zones-check-box show-grid-check-box))))
        ((and (equal? (vector-ref current-sport 0) 5) ; swim
              (send data-frame contains? "spd" "cad"))
         (set! x-axis axis-swim-avg-cadence)
         (set! y-axis axis-swim-stroke-length)
         ;; Add the torque series if not present
         (set! zones (get-session-sport-zones session-id 3))
         (set! yval-fn cadence->stride)
         (set! filter-fn valid-only)
         (send control-panel change-children
               (lambda (old) (list swim-pace-field cadence-field
                                   show-zones-check-box show-grid-check-box))))
        ((and (equal? (vector-ref current-sport 0) 2) ; bike
              (send data-frame contains? "pwr" "cad"))
         (set! x-axis axis-cadence)
         (set! y-axis axis-torque)
         ;; Add the torque series if not present
         (unless (send data-frame contains? "torque")
           (add-torque-series df))
         (set! zones (get-session-sport-zones session-id 3))
         (set! yval-fn cadence->torque)
         (set! filter-fn filter-torque)
         (send control-panel change-children
               (lambda (old) (list power-field cadence-field
                                   show-zones-check-box show-grid-check-box))))
        (#t
         (set! zones #f)
         (set! x-axis #f)
         (set! y-axis #f)
         (set! yval-fn #f)
         (send control-panel change-children (lambda (old) '()))))
      (restore-params-for-sport)
      (when zones
        (set! zone-rt (make-sport-zone-renderers zones yval-fn)))
      (set! inhibit-refresh #f)
      (refresh-plot))

    (define/public (get-generation) generation)

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (send plot-pb export-image-to-file file))))
    
    ))
