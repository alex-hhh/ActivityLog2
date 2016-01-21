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

;; Transform TIKS (a ticks struct) so that it really prints values transormed
;; by tr-fun.  This is part of the hack to add a secondary axis to a plot.  It
;; is used to print secondary axis values at the primary axis ticks.
(define (transform-ticks tiks tr-fun)
  (let ([layout (ticks-layout tiks)]
        [format (ticks-format tiks)])
    (ticks
     layout
     (lambda (start end tics)
       (format (tr-fun start) 
               (tr-fun end) 
               (for/list ([t tics])
                 (pre-tick (tr-fun (pre-tick-value t))
                           (pre-tick-major? t))))))))

;; Return a function that will plot the BEST-AVG data using spline
;; interpolation
(define (best-avg->plot-fn best-avg)
  (let ((data (for/list ([e best-avg] #:when (vector-ref e 1))
                (match-define (vector d m s) e)
                (vector d m))))
    ;; need at least 3 points for spline interpolation
    (if (> (length data) 3)
        (mk-spline-fn data)
        #f)))

(define (extract-data-series session base-axis data-axis)
  (let-values ([(data lap-markers min-x max-x min-y max-y)
                (extract-data session base-axis data-axis)])
    (if (and data (not (null? data))) data #f)))

(define (transform v smin smax tmin tmax)
  (let ((p (/ (- v smin) (- smax smin))))
    (+ tmin (* p (- tmax tmin)))))

(define (inv-transform v smin smax tmin tmax)
  (let ((p (/ (- v tmin) (- tmax tmin))))
    (+ smin (* p (- smax smin)))))

;; Return the set of transformation parameters so that BEST-AVG-AUX values map
;; onto BEST-AVG plot (for example a 0-100 cadence range can be mapped to a
;; 0-500 watt power graph.  The returned values can be passed to `transform'
;; and `inv-transform'.
(define (get-transform-params best-avg-aux best-avg [zero-base? #t])
  (define tmin (if zero-base? 0 #f))
  (define tmax #f)
  (for ([b best-avg])
    (match-define (vector _1 value _2) b)
    (when value
      (set! tmin (if tmin (min tmin value) value))
      (set! tmax (if tmax (max tmax value) value))))
  (define smin (if zero-base? 0 #f))
  (define smax #f)
  (for ([b best-avg-aux])
    (match-define (vector _1 value _2) b)
    (when value
      (set! smin (if smin (min smin value) value))
      (set! smax (if smax (max smax value) value))))
  (values smin smax tmin tmax))

(define (mk-inverse best-avg-aux best-avg zero-base?)
  (let-values ([(smin smax tmin tmax) (get-transform-params best-avg-aux best-avg zero-base?)])
    (lambda (v)
      (inv-transform v smin smax tmin tmax))))

;; Normalize (transform) the values in BEST-AVG-AUX so that they can be
;; displayed on the BEST-AVG plot.
(define (normalize best-avg-aux best-avg [zero-base? #t])
  (define tmin (if zero-base? 0 #f))
  (define tmax #f)
  (for ([b best-avg])
    (match-define (vector _1 value _2) b)
    (when value
      (set! tmin (if tmin (min tmin value) value))
      (set! tmax (if tmax (max tmax value) value))))
  (define smin (if zero-base? 0 #f))
  (define smax #f)
  (for ([b best-avg-aux])
    (match-define (vector _1 value _2) b)
    (when value
      (set! smin (if smin (min smin value) value))
      (set! smax (if smax (max smax value) value))))
  (define (tr v) (transform v smin smax tmin tmax))
  (for/list ([data best-avg-aux])
    (match-define (vector duration value position) data)
    (if value
        (vector duration (tr value) position)
        data)))

(define (make-best-avg-plot-render-tree best-avg-data best-avg-axis aux-data aux-axis zero-base?)
  (define data-fn (best-avg->plot-fn best-avg-data))
  (define data-color (get-axis-plot-color best-avg-axis))

  (if (not data-fn)
      #f
      (let ((foo 0))
        (define min-x #f)
        (define max-x #f)
        (for ([b best-avg-data] #:when (vector-ref b 1))
          (unless min-x (set! min-x (vector-ref b 0)))
          (set! max-x (vector-ref b 0)))
        
        (define aux-fn (if aux-data (best-avg->plot-fn (normalize aux-data best-avg-data zero-base?)) #f))
        (define aux-color (if aux-axis (get-axis-plot-color aux-axis) #f))

        (define data-rt
          (let ((kwd '()) (val '()))
            (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
            (when zero-base? (add-arg '#:y-min 0))
            (add-arg '#:width 3)
            (add-arg '#:color data-color)
            (keyword-apply function kwd val data-fn min-x max-x '())))
        (define aux-rt
          (if (not aux-fn)
              #f
              (let ((kwd '()) (val '()))
                (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
                (when zero-base? (add-arg '#:y-min 0))
                (add-arg '#:width 3)
                (add-arg '#:style 'long-dash)
                (add-arg '#:color aux-color)
                (keyword-apply function kwd val aux-fn min-x max-x '()))))

        (if aux-rt (list data-rt aux-rt) data-rt))))

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

    (define (get-plot-snip)
      (if (not best-avg-render-tree)
          #f
          (let ((rt (list best-avg-render-tree)))
            (when show-grid?
              (set! rt (cons (tick-grid) rt)))
            (let ((best-avg-axis (get-series-axis))
                  (aux-axis (get-aux-axis)))
              (if aux-axis
                  (let ((ivs (mk-inverse best-avg-aux-data best-avg-data zero-base?)))
                    (parameterize ([plot-x-ticks (best-avg-ticks)]
                                   [plot-x-label "Duration"]
                                   [plot-x-transform log-transform]
                                   [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                                   [plot-y-label (send best-avg-axis get-axis-label)]
                                   [plot-y-far-ticks (transform-ticks (send aux-axis get-axis-ticks) ivs)]
                                   [plot-y-far-label (send aux-axis get-axis-label)])
                      (plot-snip rt)))
                  (parameterize ([plot-x-ticks (best-avg-ticks)]
                                 [plot-x-label "Duration"]
                                 [plot-x-transform log-transform]
                                 [plot-y-ticks (send best-avg-axis get-axis-ticks)]
                                 [plot-y-label (send best-avg-axis get-axis-label)])
                    (plot-snip rt)))))))

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
                      (make-best-avg-plot-render-tree
                       best-avg-data (get-series-axis)
                       best-avg-aux-data (get-aux-axis)
                       zero-base?))
                (unless best-avg-render-tree
                  (set! best-avg-render-tree 'no-data))
                (queue-callback refresh-plot))))
            (#t
             (let ((snip (get-plot-snip)))
               (send graph-pb set-background-message "...")
               (send graph-pb set-snip snip)))))
      
    (define (rebuild-best-avg-data)
      (set! best-avg-data #f)
      (when session
        (define series-axis (get-series-axis))
        (let ((cached-data (hash-ref best-avg-cache series-axis (lambda () #f))))
          (when cached-data
            (set! best-avg-data cached-data)))
        (define inverted? (send series-axis inverted-best-avg?))
        ;; HACK: inverted? plots are negatively affected by the stop detection
        ;; code, which puts 0 points in the data series.  For inverted graphs,
        ;; this results in those sections always being selected and a 0 min/km
        ;; pace is really fast :-).
        (define base-axis
          (if inverted? axis-elapsed-time-no-stop-detection axis-elapsed-time))
        (unless best-avg-data
          (define data-series (extract-data-series session base-axis series-axis))
          (set! best-avg-data (if data-series (make-best-avg data-series inverted?) #f))
          (hash-set! best-avg-cache series-axis best-avg-data))
        ;; Rebuild auxiliary data here
        (define aux-axis (get-aux-axis))
        (when (and best-avg-data aux-axis)
          (define aux-series (extract-data-series session base-axis aux-axis))
          (set! best-avg-aux-data (if aux-series (make-best-avg-aux aux-series best-avg-data) #f)))))

    (define/public (save-visual-layout)
      (when current-sport
        (hash-set! axis-choice-by-sport current-sport selected-axis))
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
                   current-sport selected-axis))

      (set! current-sport (cons (session-sport s) (session-sub-sport s)))
      (set! generation (+ 1 generation))
      (set! session s)

      (set! axis-choices (get-best-avg-axis-choices))
      (install-axis-choices axis-choices)

      (let ((xy-index (hash-ref axis-choice-by-sport current-sport
                                (lambda () 0))))
        (set! selected-axis xy-index))

      (send axis-choice-box set-selection selected-axis)
      (send aux-axis-choice-box set-selection 0)
      (set! selected-aux-axis 0)

      (invalidate-best-avg-data)
      (set! best-avg-cache (make-hash))
      (refresh-plot))

    (define/public (get-generation) generation)

    ))
