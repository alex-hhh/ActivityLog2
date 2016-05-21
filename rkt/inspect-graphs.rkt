#lang racket/base
;; inspect-graphs.rkt -- graphs for various data series for a session
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
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/match
         math/statistics
         racket/math
         racket/sequence
         racket/vector
         "activity-util.rkt"
         "al-prefs.rkt"
         "al-widgets.rkt"
         "fmt-util.rkt"
         "plot-axis-def.rkt"
         "sport-charms.rkt"
         "utilities.rkt"
         "data-frame.rkt"
         "widgets.rkt"
         "dbglog.rkt"
         "al-profiler.rkt"
         "session-df.rkt"
         "workers.rkt")

(provide graph-panel%)
(provide elevation-graph%)

(define identity (lambda (x) x))

(define *header-font*
  (send the-font-list find-or-create-font 15 'default 'normal 'normal))

(define graph-title-font
  (send the-font-list find-or-create-font 9 'default 'normal 'normal))


;;.............................................................. helpers ....

(define (is-lap-swimming? data-frame)
  (send data-frame get-property 'is-lap-swim?))


;;.......................................................... chart-view% ....

(define message-font
  (send the-font-list find-or-create-font 36 'default 'normal 'normal))

(define (draw-centered-message dc msg font)
  (let-values (([cw ch] (send dc get-size))
               ([w h x y] (send dc get-text-extent msg font #t)))
    (send dc set-font font)
    (send dc set-text-foreground "gray")
    (let ((ox (- (/ cw 2) (/ w 2)))
          (oy (- (/ ch 2) (/ h 2))))
      (send dc draw-text msg ox oy))))

(define graph-view%
  (class object%
    (init parent)
    (init-field text tag [min-height 250])

    (super-new)

    (define data-frame #f)

    (define data-series #f)
    (define data-y-range #f)

    (define factored-data #f)
    (define factor-fn #f)
    (define factor-colors #f)

    (define data-series2 #f)            ; secondary data series

    ;; whether this graph is active or not.  Inactive graphs will not be
    ;; displayed at all.
    (define active? #t)

    (define show-graph? #t)         ; graph view can be toggled on/off
    (define show-avg? #f)           ; display the average line
    (define show-grid? #f)          ; display a grid
    (define zoom-to-lap? #f)        ; zoom current lap via a stretch-transform
    (define filter-amount 0)        ; amount of filtering to use

    (define x-axis #f)
    (define y-axis #f)
    (define y-axis2 #f)                 ; secondary Y axis

    (define selected-lap #f)        ; lap# that should be highlighted

    ;; The render tree to be passed to plot.  This is produced by
    ;; `prepare-render-tree' once we have an session
    (define graph-render-tree #f)
    (define lap-render-tree #f)

    ;; The graph itself is drawn once and stored as a bitmap.  It will be
    ;; redrawn only when graph params change or the width/height of the canvas
    ;; does.
    (define cached-graph-bitmap #f)
    (define cached-bitmap-dirty? #f)

    (define y-axis-by-sport (make-hash)) ; saved as a preference

    (let ((pref (al-get-pref tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 3))
        (set! active? (first pref))
        (set! show-graph? (second pref))
        (set! y-axis-by-sport (hash-copy (third pref)))))

    ;; The panel that contains the entire graph view
    (define panel (new (class vertical-panel%
                         (init)
                         (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
                       [parent parent]
                       [style '(border)]
                       [border 1]
                       [spacing 0]
                       [stretchable-height show-graph?]
                       [alignment '(center top)]))

    (define/public (get-panel) panel)

    ;; Panel containing the title and other controls for this graph
    (define title-panel (new horizontal-pane% [parent panel]
                             [border 0]
                             [spacing 5]
                             [vert-margin 0]
                             [stretchable-height #f]
                             [alignment '(center center)]))

    ;; The toggle button with icon and message go into an unnamed panel
    (let ((hp (new horizontal-pane% [parent title-panel]
                   [border 0]
                   [spacing 2]
                   [vert-margin 0]
                   [stretchable-height #f]
                   [alignment '(left center)]))
          (cb (lambda (button event)
                (set! show-graph? (not show-graph?))
                (if show-graph?
                    (begin
                      (send panel add-child graph-canvas)
                      (send button set-label "Hide"))
                    (begin
                      (send panel delete-child graph-canvas)
                      (send button set-label "Show")))
                ;; Make the panel non stretchable if the graph is not visible.
                ;; This will ensure it occupies the minimum space needed.
                (send panel stretchable-height show-graph?)
                (send panel reflow-container))))
      (new button% [parent hp] [label (if show-graph? "Hide" "Show")]
           [vert-margin 0] [callback cb])
      (new message% [parent hp] [label text] [font graph-title-font]))

    ;; Panel containing a popup box selecting different things to display
    ;; (this is setup by derived classes via `setup-y-axis-items`
    (define optional-items-panel (new horizontal-pane% [parent title-panel]
                                      [spacing 5]
                                      [stretchable-height #f]
                                      [alignment '(right center)]))

    (define y-axis-choice #f)

    (define/public (setup-y-axis-items y-axis-choices)
      ;; First remove all previous children from the panel
      (for-each
       (lambda (c) (send optional-items-panel delete-child c))
       (send optional-items-panel get-children))

      (when y-axis-choices
	(set! y-axis-choice
              (new choice% [parent optional-items-panel]
                   [label "Display: "]
                   [choices y-axis-choices]
                   [callback (lambda (c e)
                               (let ((index (send c get-selection))
                                     (sport (if data-frame (send data-frame get-property 'sport) #f)))
                                 (when sport
                                   (hash-set! y-axis-by-sport sport index))
                                 (on-y-axis-selected index)))]))
        (send y-axis-choice set-selection 0)
        (on-y-axis-selected 0)))

    (define (get-cached-graph-bitmap width height)

      (define (full-render-tree)
        (let ((render-tree (list graph-render-tree)))
          (when lap-render-tree
            (set! render-tree (cons lap-render-tree render-tree)))
          (when show-grid?
            (set! render-tree (cons (tick-grid) render-tree)))
          (when show-avg?
            (let ((avg (get-average-renderer)))
              (when avg (set! render-tree (cons avg render-tree)))))
          (reverse render-tree)))

      (define (get-x-transform)
        (if (and selected-lap zoom-to-lap?)
            (let ((range (get-lap-extents data-series data-frame selected-lap)))
              (if range
                  (stretch-transform (car range) (cdr range) 30)
                  id-transform))
            id-transform))

      (define (get-x-axis-ticks)
        (let ((ticks (send x-axis get-axis-ticks)))
          (if (and selected-lap zoom-to-lap?)
              (let ((range (get-lap-extents data-series data-frame selected-lap)))
                (if range
                    (ticks-add ticks (list (car range) (cdr range)))
                    ticks))
              ticks)))

      (let* ((bmp (if (and cached-graph-bitmap
                           (= (send cached-graph-bitmap get-width) width)
                           (= (send cached-graph-bitmap get-height) height))
                      ;; We can reuse the bitmap if it is the same size
                      cached-graph-bitmap
                      (make-object bitmap% width height #f #f (get-display-backing-scale)))))
        (parameterize ([plot-x-transform (get-x-transform)]
                       [plot-x-ticks (get-x-axis-ticks)]
                       [plot-x-label (send x-axis get-axis-label)]
                       [plot-y-ticks (send y-axis get-axis-ticks)]
                       [plot-y-label (send y-axis get-axis-label)])
          (plot/dc (full-render-tree) (send bmp make-dc) 0 0 width height))
        bmp))

    (define (make-cached-graph-bitmap width height)
      (queue-task
       "graph-view%/make-cached-graph-bitmap"
       (lambda ()
         (let ((bmp (get-cached-graph-bitmap width height)))
           (queue-callback
            (lambda ()
              (set! cached-graph-bitmap bmp)
              (set! cached-bitmap-dirty? #f)
              (send graph-canvas refresh)))))))

    (define (on-canvas-paint canvas dc)

      (define (maybe-draw-working-message)
        (when cached-graph-bitmap
          ;; if a bitmap is available, use it untill data is ready.
          (send dc draw-bitmap cached-graph-bitmap 0 0))
        (draw-centered-message dc "Working..." message-font))

      (cond ((or (eq? x-axis #f) (eq? y-axis #f))
             (draw-centered-message dc "Graph not configured" message-font))
            ((eq? graph-render-tree #f)
             (draw-centered-message dc "No data for graph" message-font))
            ((eq? graph-render-tree 'working)
             (maybe-draw-working-message))
            (#t
             (let-values (([w h] (send canvas get-virtual-size)))
               (if (or cached-bitmap-dirty?
                         (not cached-graph-bitmap)
                         (not (= (send cached-graph-bitmap get-width) w))
                         (not (= (send cached-graph-bitmap get-height) h)))
                   (begin
                     (make-cached-graph-bitmap w h)
                     (maybe-draw-working-message))
                   (send dc draw-bitmap cached-graph-bitmap 0 0))))))

    (define graph-canvas
      (new canvas% [parent panel]
           [min-height min-height]
           [style (if show-graph? '() '(deleted))]
           [paint-callback
            (lambda (canvas dc)
              ;; The canvas will be left in an invalid state if exceptions are
              ;; thrown while painting it.  We catch the exceptions and just
              ;; discard them, but it would be nice to clean-up and re-raise
              ;; the exception.
              (with-handlers
                (((lambda (e) #t)
                  (lambda (e) (display (format "Exception in canvas/paint-callback: ~a~%" e)))))
                (on-canvas-paint canvas dc)))]))

    (define/public (suspend-flush) (send graph-canvas suspend-flush))
    (define/public (resume-flush) (send graph-canvas resume-flush))

    (define (prepare-render-tree)
      (if (and data-frame x-axis y-axis)
          ;; Capture all variables, we are about to do work in another thread
          ;; and these could change underneath us.
          (let ((data-frame data-frame)
                (x-axis x-axis)
                (y-axis y-axis)
                (y-axis2 y-axis2)
                (filter-amount filter-amount))
            (set! graph-render-tree 'working)
            (queue-task
             "graph-view%/prepare-render-tree"
             (lambda ()
               (define ds
                 (or data-series
                     (let ([ds (extract-data data-frame x-axis y-axis filter-amount)])
                       (if (is-lap-swimming? data-frame)
                           (add-verticals ds)
                           ds))))
               (define fdata
                 (or factored-data
                     (if factor-fn
                         (group-samples/factor ds factor-fn #:key (lambda (v) (vector-ref v 1)))
                         #f)))
               (define ds2
                 (or data-series2
                     (if y-axis2
                         (let ([ds (extract-data data-frame x-axis y-axis2 filter-amount)])
                           (if (is-lap-swimming? data-frame)
                               (add-verticals ds)
                               ds))
                         #f)))
               (define yr
                 (or data-y-range
                     (let* ((st1 (if ds (ds-stats ds) #f))
                            (st2 (if ds2 (ds-stats ds2) #f))
                            (yr1 (if st1 (get-plot-y-range st1 y-axis) #f))
                            (yr2 (if st2 (get-plot-y-range st2 y-axis2) #f)))
                       (cond ((and yr1 yr2)
                              (combine-y-range yr1 yr2))
                             (yr1)
                             (yr2)
                             (#t #f)))))
               (define rt
                 (cond
                   ((and ds (is-lap-swimming? data-frame)
                         (eq? (send y-axis get-line-color) 'smart))
                    (make-plot-renderer/swim-stroke
                     ds
                     (send data-frame select "swim_stroke")))
                   (fdata (make-plot-renderer/factors fdata yr factor-colors))
                   ((and ds ds2)
                    (list
                     (make-plot-renderer ds yr
                                         #:color (send y-axis get-line-color)
                                         #:width 1
                                         #:alpha 0.9
                                         #:label (send y-axis get-series-label))
                     (make-plot-renderer ds2 yr
                                         #:color (send y-axis2 get-line-color)
                                         #:width 1
                                         #:alpha 0.9
                                         #:label (send y-axis2 get-series-label))))
                   (ds
                    (make-plot-renderer ds yr
                                        #:color (send y-axis get-line-color)))
                   (ds2
                    (make-plot-renderer ds2 yr
                                        #:color (send y-axis2 get-line-color)))
                   (#t #f)))
               (queue-callback
                (lambda ()
                  (set! data-series ds)
                  (set! data-y-range yr)
                  (set! data-series2 ds2)
                  (set! factored-data fdata)
                  (set! graph-render-tree rt)
                  (set! cached-bitmap-dirty? #t)
                  (highlight-lap selected-lap)
                  (send graph-canvas refresh))))))
          (set! graph-render-tree #f)))

    (define/public (get-average-renderer)
      #f)

    (define/public (on-y-axis-selected index)
      #f)

    ;; Return #t if this graph can display some data for DATA-FRAME (e.g. a
    ;; cadence graph is only valid if there is cadence series in the data
    ;; frame).  This needs to be overriden.
    (define/public (is-valid-for? data-frame) #f)

    (define/public (set-factors ffn fcolors)
      (set! factor-fn ffn)
      (set! factor-colors fcolors)
      (set! factored-data #f)
      (send graph-canvas refresh))

    (define/public (save-visual-layout)
      (al-put-pref
       tag
       (list active? show-graph? y-axis-by-sport)))

    (define/public (set-data-frame df)
      (suspend-flush)
      (set! cached-bitmap-dirty? #t)
      (set! cached-graph-bitmap #f)     ; dont use previous one
      (set! data-frame df)
      (set! data-series #f)
      (set! data-y-range #f)
      (set! factored-data #f)
      (set! factor-fn #f)
      (set! factor-colors #f)
      (set! data-series2 #f)
      (when (and y-axis-choice data-frame)
        (let* ((sport (send data-frame get-property 'sport))
               (y-axis-index (hash-ref y-axis-by-sport sport 0)))
          (send y-axis-choice set-selection y-axis-index)
          (on-y-axis-selected y-axis-index)))
      (set! selected-lap #f)
      (prepare-render-tree)
      (resume-flush))

    (define/public (zoom-to-lap zoom)
      (set! zoom-to-lap? zoom)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh))

    (define/public (show-grid show)
      (set! show-grid? show)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh))

    (define/public (set-filter-amount a)
      (set! filter-amount a)
      (set! data-series #f)
      (set! data-series2 #f)
      (set! factored-data #f)
      (prepare-render-tree))

    (define/public (show-average-line show)
      (set! show-avg? show)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh))

    (define/public (set-x-axis new-x-axis)
      (set! x-axis new-x-axis)
      (set! data-series #f)
      (set! data-series2 #f)
      (set! factored-data #f)
      (prepare-render-tree))

    (define/public (set-y-axis new-y-axis (new-y-axis2 #f))
      (set! y-axis new-y-axis)
      (set! y-axis2 new-y-axis2)
      (set! data-series #f)
      (set! data-y-range #f)
      (set! factored-data #f)
      (set! factor-fn #f)
      (set! factor-colors #f)
      (set! data-series2 #f)
      (prepare-render-tree))

    (define/public (highlight-lap lap-num)

      (define (get-color)
        (let ((c (send y-axis get-line-color)))
          (if (eq? c 'smart) "gray" c)))
      
      (if (and lap-num (>= lap-num 0) data-series data-frame data-y-range)
          (let ((lap-extents (get-lap-extents data-series data-frame lap-num)))
            (set! lap-render-tree
                  (make-box-renderer (car lap-extents) (cdr lap-extents)
                                     (car data-y-range) (cdr data-y-range)
                                     (get-color))))
          (begin
            (set! lap-render-tree #f)))
      (set! selected-lap lap-num)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh))

    (define/public (get-data-frame) data-frame)

    (define/public (export-image-to-file file-name)
      (let-values (([cwidth cheight] (send graph-canvas get-size)))
        (let ((bmp (send graph-canvas make-bitmap cwidth cheight)))
          (on-canvas-paint graph-canvas (new bitmap-dc% [bitmap bmp]))
          (send bmp save-file file-name 'png))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f #f "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (export-image-to-file file))))

    (define/public (get-name) text)
    (define/public (is-active?) active?)
    (define/public (set-active flag) (set! active? flag))

    ))


;;......................................................... speed-graph% ....

(define speed-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:speed-graph]
               [text "Speed"])
    (inherit setup-y-axis-items set-y-axis get-data-frame)

    (define zones #f)
    (define selected-y-axis 0)
    (define avg-speed #f)

    (define (get-avg-speed)
      (unless avg-speed
        (let ((st (df-statistics (get-data-frame) "spd")))
          (set! avg-speed (statistics-mean st))))
      avg-speed)

    (define y-axis-items
      `(("Speed" ,axis-speed ,m/s->speed ,speed->string)
	("Pace" ,axis-pace ,m/s->pace ,pace->string)
        ("Zone" ,axis-speed-zone ,(lambda (x) (val->zone x zones)) ,(lambda (x y) (format-48 "~1,1F" x)))))

    (define/override (get-average-renderer)
      (let ((avg (get-avg-speed)))
	(if avg
            (let* ((item (list-ref y-axis-items selected-y-axis))
                   (speed-converter (third item))
                   (speed-formatter (fourth item))
                   (avg-val (speed-converter avg))
                   (label (string-append "Avg " (speed-formatter avg #t))))
              (function (lambda (x) avg-val) #:label label))
            #f)))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-speed #f)
      (set! zones #f)
      (when data-frame
        (define sid (send data-frame get-property 'session-id))
        (set! zones (get-session-sport-zones sid 2)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (for/or ([series '("speed" "pace" "speed-zone")])
        (send data-frame contains? series)))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)
    
    ))


;;..................................................... elevation-graph% ....

(define elevation-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:elevation-graph]
               [text "Elevation"])
    (inherit setup-y-axis-items set-y-axis)

    (define selected-y-axis 0)

    (define y-axis-items
      `(("Elevation (original)" ,axis-elevation)
        ("Elevation (corrected)" ,axis-corrected-elevation)
        ("Grade" ,axis-grade)))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "alt" "calt"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;.................................................... heart-rate-graph% ....


(define heart-rate-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:hr-graph]
               [text "Heart Rate"])

    (inherit setup-y-axis-items set-y-axis get-data-frame)

    (define selected-y-axis 0)
    (define zones #f)
    (define avg-hr #f)

    (define (get-avg-hr)
      (unless avg-hr
        (let ((st (df-statistics (get-data-frame) "hr")))
          (set! avg-hr (statistics-mean st))))
      avg-hr)

    (define y-axis-items
      `(("BPM" ,axis-hr-bpm ,identity ,heart-rate->string/bpm)
	("% of Max" ,axis-hr-pct ,(lambda (v) (val->pct-of-max v zones))
         ,(lambda (v) (heart-rate->string/pct v zones)))
	("Zone" ,axis-hr-zone ,(lambda (v) (val->zone v zones))
         ,(lambda (v) (heart-rate->string/zone v zones)))))

    (define/override (get-average-renderer)
      (let ((avg (get-avg-hr)))
	(if avg
            (let* ((item (list-ref y-axis-items selected-y-axis))
                   (bpm-converter (third item))
                   (bpm-formatter (fourth item))
                   (avg-val (bpm-converter avg))
                   (label (string-append "Avg " (bpm-formatter avg))))
              (function (lambda (x) avg-val) #:label label))
            #f)))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-hr #f)
      (set! zones #f)
      (when data-frame
        (define sid (send data-frame get-property 'session-id))
        (set! avg-hr #f)
        (set! zones (get-session-sport-zones sid 1)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "hr" "hr-pct" "hr-zone"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)
    
    ))


;;....................................................... cadence-graph% ....

(define (cadence-factor/run cad)
  (define cad1 (* 2 cad))
  (cond ((> cad1 185) 'purple)
        ((> cad1 174) 'blue)
        ((> cad1 163) 'green)
        ((> cad1 151) 'orange)
        (#t 'red)))

(define (cadence-factor/bike cad)
  (cond ((> cad 95) 'purple)
        ((> cad 85) 'blue)
        ((> cad 75) 'green)
        ((> cad 65) 'orange)
        (#t 'red)))

(define cadence-colors
  (list
   (list 'red '(220 20 60))
   (list 'orange '(255 127 80))
   (list 'green '(34 139 34))
   (list 'blue '(30 144 255))
   (list 'purple '(139 0 139))))

(define cadence-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:cadence-graph]
               [text "Cadence"])
    (inherit set-y-axis get-data-frame setup-y-axis-items set-factors)

    (define sport #f)
    (define selected-y-axis 0)
    (define avg-cadence #f)
    (define avg-stride #f)

    (define (get-avg-cadence)
      (unless avg-cadence
        (let ((stats (df-statistics (get-data-frame) "cad")))
          (set! avg-cadence (statistics-mean stats))))
      avg-cadence)

    (define (get-avg-stride)
      (unless avg-stride
        (let ((stats (df-statistics (get-data-frame) "stride")))
          (set! avg-stride (statistics-mean stats))))
      avg-stride)

    (define y-axis-items
      `(("Cadence" ,axis-cadence ,get-avg-cadence ,(lambda (v) (cadence->string v sport #t)))
	("Stride" ,axis-stride ,get-avg-stride ,(lambda (c) (stride->string c #t)))))

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn)))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
            #f))))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index))))
      ;; NOTE: we have to do this always!
      (when (equal? index 0)            ; cadence
        (cond ((equal? sport 1)         ; running
               (set-factors cadence-factor/run cadence-colors))
              ((equal? sport 2)         ; cycling
               (set-factors cadence-factor/bike cadence-colors)))))

    (define/override (set-data-frame data-frame)
      (set! avg-cadence #f)
      (set! avg-stride #f)
      (when data-frame
        (let ((sp (send data-frame get-property 'sport)))
          (set! sport (vector-ref sp 0))))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "cad" "stride"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;................................................... vosc-vratio-graph% ....

(define (vosc-factor vosc)
  (cond ((> vosc 118) 'red)
        ((> vosc 111) 'orange)
        ((> vosc 84) 'green)
        ((> vosc 67) 'blue)
        (#t 'purple)))

(define (vratio-factor vratio)
  (cond ((> vratio 10.1) 'red)
        ((> vratio 8.7) 'orange)
        ((> vratio 7.5) 'green)
        ((> vratio 6.1) 'blue)
        (#t 'purple)))

(define vosc-vratio-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:vosc-vratio-graph]
               [text "Vertical Oscillation"])

    (inherit set-y-axis get-data-frame setup-y-axis-items set-factors)
    (define sport #f)
    (define selected-y-axis 0)
    (define avg-vosc #f)
    (define avg-vratio #f)

    (define (get-avg-vosc)
      (unless avg-vosc
        (let ((stats (df-statistics (get-data-frame) "vosc")))
          (set! avg-vosc (statistics-mean stats))))
      avg-vosc)

    (define (get-avg-vratio)
      (unless avg-vratio
        (let ((stats (df-statistics (get-data-frame) "vratio")))
          (set! avg-vratio (statistics-mean stats))))
      avg-vratio)

    (define y-axis-items
      `(("VOSC" ,axis-vertical-oscillation ,get-avg-vosc ,(lambda (v) (vosc->string v #t)))
	("VRATIO" ,axis-vratio ,get-avg-vratio ,(lambda (c) (vratio->string c #t)))))

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn)))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
              #f))))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index))))
      ;; NOTE: we have to do this always
      (when (equal? sport 1)            ; running
        (cond ((equal? index 0)         ; VOSC
               (set-factors vosc-factor cadence-colors))
              ((equal? index 1)
               (set-factors vratio-factor cadence-colors)))))

    (define/override (set-data-frame data-frame)
      (set! avg-vosc #f)
      (set! avg-vratio #f)
      (if data-frame
          (let ((sp (send data-frame get-property 'sport)))
            (set! sport (vector-ref sp 0)))
          (set! sport #f))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "vosc" "vratio"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;........................................................... gct-graph% ....

(define (gct-factor gct)
  (cond ((> gct 305) 'red)
        ((> gct 273) 'orange)
        ((> gct 241) 'green)
        ((> gct 208) 'blue)
        (#t 'purple)))

(define gct-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:gct-graph]
               [text "Ground Contact Time"])

    (inherit set-y-axis get-data-frame setup-y-axis-items set-factors)

    (define sport #f)
    (define selected-y-axis 0)
    (define avg-gct #f)
    (define avg-gct-pct #f)

    (define (get-avg-gct)
      (unless avg-gct
        (let ((stats (df-statistics (get-data-frame) "gct")))
          (set! avg-gct (statistics-mean stats))))
      avg-gct)

    (define (get-avg-gct-pct)
      (unless avg-gct-pct
        (let ((stats (df-statistics (get-data-frame) "pgct")))
          (set! avg-gct-pct (statistics-mean stats))))
      avg-gct-pct)

    (define y-axis-items
      `(("as time (ms)" ,axis-stance-time ,get-avg-gct ,(lambda (v) (stance-time->string v #t)))
	("as percent" ,axis-stance-time-percent ,get-avg-gct-pct ,(lambda (v) (stance-time-pct->string v #t)))))

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn)))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
              #f))))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index))))
      ;; NOTE: we have to do this always
      (when (equal? sport 1)            ; running
        (cond ((equal? index 0)         ; GCT
               (set-factors gct-factor cadence-colors)))))

    (define/override (set-data-frame data-frame)
      (set! avg-gct #f)
      (set! avg-gct-pct #f)
      (when data-frame
        (let ((sp (send data-frame get-property 'sport)))
          (set! sport (vector-ref sp 0))))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "gct" "pgct"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)


    ))


;;..................................................... power-graph% ....

(define power-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:power-graph]
               [text "Power"])
    (inherit set-y-axis get-data-frame setup-y-axis-items)

    (define selected-y-axis 0)
    (define zones #f)
    (define avg-power #f)

    (define (get-avg-power)
      (unless avg-power
        (let ((st (df-statistics (get-data-frame) "pwr")))
          (set! avg-power (statistics-mean st))))
      avg-power)

    (define y-axis-items
      `(("Watts" ,axis-power ,identity ,number->string)
	("Zone" ,axis-power-zone ,(lambda (v) (val->zone v zones))
         ,(lambda (v) (format-48 "~1,1F" (val->zone v zones))))))

    (define/override (get-average-renderer)
      (let ((avg (get-avg-power)))
	(if avg
            (let* ((item (list-ref y-axis-items selected-y-axis))
                   (pwr-converter (third item))
                   (pwr-formatter (fourth item))
                   (avg-val (pwr-converter avg))
                   (label (string-append "Avg " (pwr-formatter avg))))
              (function (lambda (x) avg-val) #:label label))
            #f)))

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-power #f)
      (set! zones #f)
      (when data-frame
        (define sid (send data-frame get-property 'session-id))
        (set! zones (get-session-sport-zones sid 3)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "pwr" "pwr-zone"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)
    ))


;;..................................................... left-right-balance-graph% ....

(define (lrbal-factors lrbal)
  (cond ((> lrbal 52.2) 'red)
        ((> lrbal 50.8) 'orange)
        ((> lrbal 49.2) 'green)
        ((> lrbal 47.8) 'orange)
        (#t 'red)))

(define lrbal-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:lrbal-graph]
               [text "Left-Right Balance"])

    (inherit set-y-axis set-factors get-data-frame setup-y-axis-items)

    (define avg-lrbal #f)

    (define (get-avg-lrbal)
      (unless avg-lrbal
        (let ((st (df-statistics (get-data-frame) "lrbal")))
          (set! avg-lrbal (statistics-mean st))))
      avg-lrbal)

    (define/override (get-average-renderer)
      (let ((avg (get-avg-lrbal)))
          (if avg
              (let ((label (format-48 "Avg ~1,1F%" avg)))
                (function (lambda (x) avg) #:label label))
              #f)))

    (set-y-axis axis-left-right-balance)

    (define/override (set-data-frame data-frame)
      (set! avg-lrbal #f)
      (super set-data-frame data-frame)
      ;; Needs to be after we set the data frame!
      (set-factors lrbal-factors cadence-colors))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains? "lrbal"))

    ))


;;.......................................................... teff-graph% ....

(define teff-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:teff-graph]
               [text "Torque Effectiveness"])

    (send this set-y-axis
          axis-left-torque-effectiveness
          axis-right-torque-effectiveness)

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "lteff" "rteff"))

    ))


;;......................................................... psmth-graph% ....

(define psmth-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:psmth-graph]
               [text "Pedal Smoothness"])

    (send this set-y-axis
          axis-left-pedal-smoothness
          axis-right-pedal-smoothness)

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "lpsmth" "rpsmth"))

    ))


;;........................................................... pco-graph% ....

(define pco-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:pco-graph]
               [text "Platform Centre Offset"])

    (send this set-y-axis
          axis-left-platform-centre-offset
          axis-right-platform-centre-offset)

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any? "lpco" "rpco"))

    ))


;;.................................................... Power Phase Graph ....

(define power-phase-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:power-phase-graph]
               [text "Power Phase"])

    (inherit set-y-axis get-data-frame  setup-y-axis-items)

    (define (setup-pp-start)
      (set-y-axis axis-left-power-phase-start
                  axis-right-power-phase-start))
    (define (setup-pp-end)
      (set-y-axis axis-left-power-phase-end
                  axis-right-power-phase-end))
    (define (setup-pp-angle)
      (set-y-axis axis-left-power-phase-angle
                  axis-right-power-phase-angle))
    (define (setup-ppp-start)
      (set-y-axis axis-left-peak-power-phase-start
                  axis-right-peak-power-phase-start))
    (define (setup-ppp-end)
      (set-y-axis axis-left-peak-power-phase-end
                  axis-right-peak-power-phase-end))
    (define (setup-ppp-angle)
      (set-y-axis axis-left-peak-power-phase-angle
                  axis-right-peak-power-phase-angle))

    (define y-axis-items
      `(("PP Start" ,setup-pp-start)
        ("PP End" ,setup-pp-end)
        ("PP Angle" ,setup-pp-angle)
        ("Peak PP Start" ,setup-ppp-start)
        ("Peak PP End" ,setup-ppp-end)
        ("Peak PP Angle" ,setup-ppp-angle)))

    (define selected-y-axis #f)

    (define/override (on-y-axis-selected index)
      (unless (equal? selected-y-axis index)
        (set! selected-y-axis index)
        (let ((fn (list-ref (list-ref y-axis-items index) 1)))
          (fn))))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains/any?
            "lpps" "lppe" "lppa" "rpps" "rppe" "rppa"
            "lppps" "lpppe" "lpppa" "rppps" "rpppe" "rpppa"))

    (setup-y-axis-items (map car y-axis-items))
    (on-y-axis-selected 0)

    ))


;;..................................................... swim-pace-graph% ....

(define swim-pace-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-pace-graph]
               [text "Swim Pace"])

    (inherit set-y-axis get-data-frame)
    (set-y-axis axis-swim-pace)

    (define avg-speed #f)

    (define (get-avg-speed)
      (unless avg-speed
        (let ((st (df-statistics (get-data-frame) "spd")))
          (set! avg-speed (statistics-mean st))))
      avg-speed)

    (define/override (get-average-renderer)
      (let ((avg (get-avg-speed)))
        (if (and avg (> avg 0))
            (let ((pace (m/s->swim-pace avg)))
              (function (lambda (x) pace)
                        #:label (string-append "Avg " (swim-pace->string avg #t))))
         #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-speed #f)
      (super set-data-frame data-frame))

    ))


;;.................................................... swim-swolf-graph% ....

(define swim-swolf-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-swolf-graph]
               [text "SWOLF"])

    (inherit set-y-axis get-data-frame)

    (set-y-axis axis-swim-swolf)

    (define avg-swolf #f)

    (define (get-avg-swolf)
      (unless avg-swolf
        (let ((st (df-statistics (get-data-frame) "swolf")))
          (set! avg-swolf (statistics-mean st))))
      avg-swolf)

    (define/override (get-average-renderer)
      (let ((avg (get-avg-swolf)))
	(if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (format-48 "Avg ~1,1F" avg))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-swolf #f)
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains? "swolf"))

    ))


;;............................................. swim-stroke-count-graph% ....

(define swim-stroke-count-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-stroke-count-graph]
               [text "Stroke Count"])

    (inherit set-y-axis get-data-frame)
    (set-y-axis axis-swim-stroke-count)

    (define avg-stroke-count #f)

    (define (get-avg-stroke-count)
      (unless avg-stroke-count
        (let ((st (df-statistics (get-data-frame) "strokes")))
          (set! avg-stroke-count (statistics-mean st))))
      avg-stroke-count)

    (define/override (get-average-renderer)
      (let ((avg (get-avg-stroke-count)))
	(if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (format-48 "Avg ~1,1F" avg))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-stroke-count #f)
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains? "strokes"))

    ))


;;.................................................. swim-cadence-graph% ....

(define swim-cadence-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-cadence-graph]
               [text "Swim Cadence"])

    (inherit set-y-axis get-data-frame)
    (set-y-axis axis-swim-avg-cadence)

    (define avg-cadence #f)

    (define (get-avg-cadence)
      (unless avg-cadence
        (let ((st (df-statistics (get-data-frame) "cad")))
          (set! avg-cadence (statistics-mean st))))
      avg-cadence)

    (define/override (get-average-renderer)
      (let ((avg (get-avg-cadence)))
	(if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (format-48 "Avg ~1,1F" avg))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-cadence #f)
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (send data-frame contains? "cad"))

    ))


;;................................................. visible-graphs-edit% ....

;; A dialog box used to edit the visible graphs in the graph panel
(define visible-graphs-edit%
  (class al-edit-dialog%
    (init)
    (super-new [title "Edit graphs to display"] [icon 'app])

    (define (setup graphs)

      (define parent-pane (send this get-client-pane))

      (define (make-check-box graph)
        (new check-box%
             [parent parent-pane]
             [label (send graph get-name)]
             [value (send graph is-active?)]
             [style '(deleted)]))

      (send parent-pane change-children
            (lambda (old) (map make-check-box graphs))))

    (define/public (run-dialog parent graphs)
      (setup graphs)
      (if (send this do-edit parent)
          (let ((selection '()))
            (for ((c (in-list (send (send this get-client-pane) get-children))))
              (when (is-a? c check-box%)
                (set! selection (cons (send c get-value) selection))))
            (for ((sel (in-list (reverse selection)))
                  (graph (in-list graphs)))
              (send graph set-active sel))
            #t)
          #f))

    ))



;......................................................... graph-panel% ....

;; Choices for the X axis in the graphs for all non-swimming activities
(define default-x-axis-choices
  `(("Time" . ,axis-timer-time)
    ("Elapsed Time" . ,axis-elapsed-time)
    ("Distance" . ,axis-distance)))

;; Choices for the X axis for swimming activitites
(define swim-x-axis-choices
  `(("Distance" . ,axis-swim-distance)
    ("Time" . ,axis-swim-time)))

(define graph-panel%
  (class object%
    (init parent)
    (super-new)

    (define the-pref-tag 'activity-log:graph-panel)

    ;; The session for which we display the graph
    (define the-session #f)

    ;; These are settings for all the graphs
    (define show-avg? #f)           ; display the average line
    (define show-grid? #f)          ; display a grid
    (define zoom-to-lap? #f)        ; zoom current lap via a stretch-transform
    (define filter-amount 0)        ; amount of filtering to use in graphs

    ;; Map the preferred x-axis (as an index) by sport, this is saved as a
    ;; user preference
    (define x-axis-by-sport (make-hash))

    ;; The axis-choices for the graphs, either default-x-axis-choices or
    ;; swim-x-axis-choices, depending on the session's sport
    (define x-axis-choices '())

    (define graphs '())      ; the list of graphs we are currently  displaying

    ;; Restore the preferences now.
    (let ((pref (al-get-pref the-pref-tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 5))
        (set! show-avg? (first pref))
        (set! show-grid? (second pref))
        (set! zoom-to-lap? (third pref))
        (set! filter-amount (fourth pref))
        (set! x-axis-by-sport (hash-copy (fifth pref)))))

    (define (show-grid show)
      (set! show-grid? show)
      (for-each (lambda (g) (send g show-grid show)) graphs))

    (define (zoom-to-lap zoom)
      (set! zoom-to-lap? zoom)
      (for-each (lambda (g) (send g zoom-to-lap zoom)) graphs))

    (define (show-average-line show)
      (set! show-avg? show)
      (for-each (lambda (g) (send g show-average-line show)) graphs))

    (define (highlight-lap n)
      (for-each (lambda (g) (send g highlight-lap n)) graphs))

    (define (set-x-axis index)
      (let ((x-axis (cdr (list-ref x-axis-choices index))))
        (when the-session
          (hash-set! x-axis-by-sport (session-sport the-session) index))
        (for-each (lambda (g) (send g set-x-axis x-axis)) graphs)))

    (define (set-filter-amount a)
      (set! filter-amount a)
      (for-each (lambda (g) (send g set-filter-amount a)) graphs))

    (define panel (new horizontal-pane%
                       [parent parent]
                       [border 0]
                       [spacing 1]
                       [alignment '(center top)]))

    (define lap-view-panel (new vertical-pane%
                                [parent panel]
                                [border 0]
                                [spacing 1]
                                [min-width 220]
                                [stretchable-width #f]
                                [alignment '(left top)]))

    (new message% [parent lap-view-panel] [label "Laps"] [font *header-font*])

    (define lap-view (new mini-lap-view%
                          [parent lap-view-panel]
                          [tag 'activity-log:charts-mini-lap-view]
                          [callback (lambda (n lap)
                                      (let ((lap-num (assq1 'lap-num lap)))
                                        (when lap-num
                                          (highlight-lap (- lap-num 1)))))]))

    (define charts-panel (new vertical-pane%
                              [parent panel]
                              [border 0]
                              [spacing 1]
                              [alignment '(left top)]))

    (define x-axis-choice #f)

    (define control-panel (new horizontal-pane%
                               [parent charts-panel]
                               [spacing 10]
                               [border 0]
                               ;; [style '(border)]
                               [stretchable-height #f]
                               [alignment '(left center)]))

    (new message% [parent control-panel] [label "Graphs"] [font *header-font*])

    (new check-box% [parent control-panel]
         [value show-avg?]
         [label "Show Average"]
         [callback (lambda (b e) (show-average-line (send b get-value)))])

    (new check-box% [parent control-panel]
         [value zoom-to-lap?]
         [label "Zoom to Lap"]
         [callback (lambda (b e) (zoom-to-lap (send b get-value)))])

    (new check-box% [parent control-panel]
         [label "Show Grid"]
         [value show-grid?]
         [callback (lambda (b e) (show-grid (send b get-value)))])

    (set! x-axis-choice
          (new choice% [parent control-panel]
               [label "X Axis Shows: "]
               [choices '()]
               [min-width 100]
               [stretchable-width #f]
               [callback (lambda (control event)
                           (let ((index (send control get-selection)))
                             (set-x-axis index)))]))

    (define filter-amount-choice
      (new choice% [parent control-panel]
           [label "Filter Amount: "]
           [choices '("None" "Small" "Medium" "Large" "Huge")]
           [callback (lambda (c e)
                       (set-filter-amount (send c get-selection)))]))

    (define setup-button
      (new button% [parent control-panel]
           [label "Setup"]
           [callback (lambda (b e) (on-setup))]))

    (send filter-amount-choice set-selection filter-amount)

    (define graphs-panel (new vertical-panel%
                              [parent charts-panel]
                              [border 0]
                              [spacing 1]
                              [style '(vscroll)]
                              [alignment '(left top)]))

    (define default-graphs
      (list
       (new speed-graph% [parent graphs-panel])
       (new elevation-graph% [parent graphs-panel])
       (new heart-rate-graph% [parent graphs-panel])
       (new cadence-graph% [parent graphs-panel])
       (new vosc-vratio-graph% [parent graphs-panel])
       (new gct-graph% [parent graphs-panel])
       (new power-graph% [parent graphs-panel])
       (new lrbal-graph% [parent graphs-panel])
       (new teff-graph% [parent graphs-panel])
       (new psmth-graph% [parent graphs-panel])
       (new pco-graph% [parent graphs-panel])
       (new power-phase-graph% [parent graphs-panel])
       ))

    (define swim-graphs
      (list
       (new swim-pace-graph% [parent graphs-panel])
       (new swim-swolf-graph% [parent graphs-panel])
       (new swim-stroke-count-graph% [parent graphs-panel])
       (new swim-cadence-graph% [parent graphs-panel])
       ))

    (define (on-setup)
      (when the-session
        (let ((e (new visible-graphs-edit%)))
          (when (send e run-dialog
                      (send panel get-top-level-window)
                      (if (is-lap-swimming? data-frame)
                          swim-graphs default-graphs))
            (setup-graphs-for-current-session)))))


    (define/public (save-visual-layout)
      (send lap-view save-visual-layout)
      (for-each (lambda (g) (send g save-visual-layout)) default-graphs)
      (for-each (lambda (g) (send g save-visual-layout)) swim-graphs)
      (al-put-pref
       the-pref-tag
       (list show-avg? show-grid? zoom-to-lap? filter-amount x-axis-by-sport)))


    (define (setup-graphs-for-current-session)

      ;; Return the available graphs for SESSION.  For non-lap swimming
      ;; activities, we only use the graphs for which we have data.
      (define (get-graphs-for-session session)
        (if (is-lap-swimming? data-frame)
            (filter (lambda (g) (send g is-active?)) swim-graphs)
            (filter (lambda (g)
                      (and (send g is-active?)
                           (send g is-valid-for? data-frame)))
                    default-graphs)))

      (set! graphs (get-graphs-for-session the-session))
      (let* ((sel (send x-axis-choice get-selection))
             (x-axis (cdr (list-ref x-axis-choices sel))))
        (for-each (lambda (g)
                    (send g suspend-flush)
                    (send g set-x-axis x-axis)
                    (send g show-grid show-grid?)
                    (send g zoom-to-lap zoom-to-lap?)
                    (send g show-average-line show-avg?)
                    (send g set-filter-amount filter-amount)
                    (send g set-data-frame data-frame)
                    (send g resume-flush))
                  graphs))
      (send graphs-panel change-children
            (lambda (old) (map (lambda (g) (send g get-panel)) graphs))))

    (define generation -1)
    (define data-frame #f)

    (define/public (set-session session df)
      ;; Clear the sessions from all graphs, this will allow it to be garbage
      ;; collected (as we won't set the session on all graphs all the time,
      ;; the previous session might stick around longer than intended.

      (for-each (lambda (g) (send g set-data-frame #f)) default-graphs)
      (for-each (lambda (g) (send g set-data-frame #f)) swim-graphs)

      (set! the-session session)
      (set! data-frame df)
      (set! generation (+ 1 generation))
      (send lap-view set-session session)
      (let ((lap-swimming? (is-lap-swimming? data-frame)))

        (set! x-axis-choices
              (if lap-swimming? swim-x-axis-choices default-x-axis-choices))
        (send filter-amount-choice set-selection (if lap-swimming? 0 filter-amount))
        (send filter-amount-choice enable (not lap-swimming?))
        (send x-axis-choice clear)
        (for-each (lambda (x) (send x-axis-choice append (car x))) x-axis-choices)

        (let* ((sport-x-axis (hash-ref x-axis-by-sport (session-sport session) 0)))
          ;; We have an ambiguity here, as the open water swim has 3 choices,
          ;; but lap swimming only two.  Our X-AXIS-BY-SPORT hash only
          ;; considers the sport (not the sub-sport) when saving the
          ;; selection...
          (if (>= sport-x-axis (length x-axis-choices))
              (send x-axis-choice set-selection 0)
              (send x-axis-choice set-selection sport-x-axis))
          (setup-graphs-for-current-session))))

    (define/public (get-generation) generation)

    ))
