#lang racket/gui
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
         (rename-in srfi/48 (format format-48))
         "activity-util.rkt"
         "al-prefs.rkt"
         "al-widgets.rkt"
         "fmt-util.rkt"
         "plot-axis-def.rkt"
         "plot-builder.rkt"
         "sport-charms.rkt"
         "utilities.rkt"
         "widgets.rkt")

(provide graph-panel%)
(provide elevation-graph%)

(define *header-font*
  (send the-font-list find-or-create-font 15 'default 'normal 'normal))

(define graph-title-font
  (send the-font-list find-or-create-font 9 'default 'normal 'bold))


;;.............................................................. helpers ....

(define (is-lap-swimming? session)
  (let ((sport (session-sport session))
        (sub-sport (session-sub-sport session)))
    (and (eqv? sport 5) (eqv? sub-sport 17))))

;; Return true if SESSION is an open water activtiy (currently OW swim or
;; sailing)
(define (is-ow-activity? session)
  (let ((sport (session-sport session))
        (sub-sport (session-sub-sport session)))
    (or (and (eqv? sport 5) (eqv? sub-sport 18))
        (and (eqv? sport 0) (eqv? sub-sport 257)))))



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
    (init-field text tag plot-builder [min-height 250])

    (super-new)

    (define the-session #f)         ; session for which we display the graph

    ;; whether this graph is active or not.  Inactive graphs will not be
    ;; displayed at all.
    (define active? #t)

    (define show-graph? #t)         ; graph view can be toggled on/off
    (define show-avg? #f)           ; display the average line
    (define show-grid? #f)          ; display a grid
    (define zoom-to-lap? #f)        ; zoom current lap via a stretch-transform
    (define filter-amount 0)        ; amount of filtering to use

    (define x-axis-info #f)
    (define y-axis-info #f)

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
                                     (sport (if the-session (session-sport the-session) #f)))
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
            (let ((range (send plot-builder get-lap-start-end selected-lap)))
              (if range
                  (stretch-transform (car range) (cdr range) 30)
                  id-transform))
            id-transform))

      (define (get-x-axis-ticks)
        (let ((ticks (send x-axis-info get-axis-ticks)))
          (if (and selected-lap zoom-to-lap?)
              (let ((range (send plot-builder get-lap-start-end selected-lap)))
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
		       [plot-x-label (send x-axis-info get-axis-label)]
		       [plot-y-ticks (send y-axis-info get-axis-ticks)]
		       [plot-y-label (send y-axis-info get-axis-label)])
          (plot/dc (full-render-tree) (send bmp make-dc) 0 0 width height))
        bmp))

    (define (make-cached-graph-bitmap width height)
      (thread
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

      (cond ((or (eq? x-axis-info #f) (eq? y-axis-info #f))
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

    (define (prepare-render-tree)
      (if (and the-session x-axis-info y-axis-info)
	  (begin
            (set! graph-render-tree 'working)
            (thread
             (lambda ()
               (let ((rt (send plot-builder get-plot-renderer)))
                 (queue-callback
                  (lambda ()
                    (set! graph-render-tree rt)
                    (set! cached-bitmap-dirty? #t)
                    (send graph-canvas refresh)))))))
          (set! graph-render-tree #f)))

    (define/public (get-average-renderer)
      #f)

    (define/public (on-y-axis-selected index)
      #f)

    ;; Return #t if this graph can display some data for SESSION (e.g. a
    ;; cadence graph is only valid if a session has cadence data).  This needs
    ;; to be overriden.
    (define/public (is-valid-for-session? session)
      #f)

    (define/public (save-visual-layout)
      (al-put-pref 
       tag
       (list active? show-graph? y-axis-by-sport)))

    (define/public (set-session session)
      (set! the-session session)
      (when (and y-axis-choice session)
        (let* ((sport (session-sport session))
               (y-axis-index (hash-ref y-axis-by-sport sport 0)))
          (send y-axis-choice set-selection y-axis-index)
          (on-y-axis-selected y-axis-index)))
      (set! selected-lap #f)
      (send plot-builder set-session session)
      (prepare-render-tree)
      (highlight-lap selected-lap))

    (define/public (zoom-to-lap zoom)
      (set! zoom-to-lap? zoom)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh-now))

    (define/public (show-grid show)
      (set! show-grid? show)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh-now))

    (define/public (set-filter-amount a)
      (set! filter-amount a)
      (send plot-builder set-filter-amount (expt a 2))
      (prepare-render-tree)
      (highlight-lap selected-lap))

    (define/public (show-average-line show)
      (set! show-avg? show)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh-now))

    (define/public (set-x-axis new-x-axis-info)
      (set! x-axis-info new-x-axis-info)
      (send plot-builder set-x-axis new-x-axis-info)
      (prepare-render-tree)
      (highlight-lap selected-lap))

    (define/public (set-y-axis new-y-axis-info)
      (set! y-axis-info new-y-axis-info)
      (send plot-builder set-y-axis new-y-axis-info)
      (prepare-render-tree)
      (highlight-lap selected-lap))

    (define/public (highlight-lap lap-num)
      (if (and lap-num (>= lap-num 0))
          (set! lap-render-tree (send plot-builder get-plot-renderer-for-lap lap-num))
          (set! lap-render-tree #f))
      (set! selected-lap lap-num)
      (set! cached-bitmap-dirty? #t)
      (send graph-canvas refresh-now))

    (define/public (get-session) the-session)

    (define/public (has-data?) (not (not graph-render-tree)))

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
	       [text "Timing"]
               [plot-builder (new plot-builder%)])

    (inherit setup-y-axis-items)
    (inherit set-y-axis)
    (inherit get-session)

    (define zones #f)

    (define selected-y-axis 0)

    (define y-axis-items
      `(("Speed"  ,axis-speed ,m/s->speed ,speed->string)
	("Pace"  ,axis-pace ,m/s->pace ,pace->string)
        ("Zone" ,axis-speed-zone ,(lambda (x) (val->zone x zones)) ,(lambda (x y) (number->string x)))))

    (define/override (get-average-renderer)
      (let ((avg (session-avg-speed (get-session))))
	(if avg
	    (let* ((item (list-ref y-axis-items selected-y-axis))
		   (speed-converter (third item))
		   (speed-formatter (fourth item))
		   (avg-val (speed-converter avg))
		   (label (string-append "Avg " (speed-formatter avg #t))))
	      (function (lambda (x) avg-val) #:label label))
	    #f)))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session session)
      (super set-session #f)
      (when session
        (define sid (assq1 'database-id session))
        (setup-y-axis-items (map car y-axis-items))
        (set-y-axis (second (list-ref y-axis-items 0)))
        (set! selected-y-axis 0)
        (set! zones (get-session-sport-zones sid 2))
        (super set-session session)))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-speed session))))

    ))


;;..................................................... elevation-graph% ....

(define elevation-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:elevation-graph]
	       [text "Elevation"]
               [plot-builder (new plot-builder%)])

    (inherit setup-y-axis-items)
    (inherit set-y-axis)
    (inherit get-session)

    (define selected-y-axis 0)

    (define y-axis-items
      `(("Elevation (original)" ,axis-elevation)
        ("Elevation (corrected)" ,axis-corrected-elevation)
        ("Grade" ,axis-grade)))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (and (not (not (session-total-ascent session)))
           ;; Open water activties have elevation data recorded, but we don't
           ;; display it
           (not (is-ow-activity? session))))

    ))


;;.................................................... heart-rate-graph% ....


(define heart-rate-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:hr-graph]
	       [text "Heart Rate"]
               [plot-builder (new plot-builder%)])

    (inherit setup-y-axis-items)
    (inherit set-y-axis)
    (inherit get-session)

    (define selected-y-axis 0)

    (define zones #f)

    (define y-axis-items
      `(("BPM" ,axis-hr-bpm ,identity ,heart-rate->string/bpm)
	("% of Max" ,axis-hr-pct ,(lambda (v) (val->pct-of-max v zones))
         ,(lambda (v) (heart-rate->string/pct v zones)))
	("Zone" ,axis-hr-zone ,(lambda (v) (val->zone v zones)) 
         ,(lambda (v) (heart-rate->string/zone v zones)))))

    (define/override (get-average-renderer)
      (let ((avg (session-avg-hr (get-session))))
	(if avg
	    (let* ((item (list-ref y-axis-items selected-y-axis))
		   (bpm-converter (third item))
		   (bpm-formatter (fourth item))
		   (avg-val (bpm-converter avg))
		   (label (string-append "Avg " (bpm-formatter avg))))
	      (function (lambda (x) avg-val) #:label label))
	    #f)))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session session)
      (super set-session #f)
      (when session
        (define sid (assq1 'database-id session))
        (setup-y-axis-items (map car y-axis-items))
        (set-y-axis (second (list-ref y-axis-items 0)))
        (set! selected-y-axis 0)
        (set! zones (get-session-sport-zones sid 1))
        (super set-session session)))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-hr session))))
    
    ))


;;....................................................... cadence-graph% ....

(define cadence-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:cadence-graph]
	       [text "Cadence"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define sport #f)

    (define y-axis-items
      `(("Cadence" ,axis-cadence ,session-avg-cadence ,(lambda (v) (cadence->string v sport #t)))
	("Stride" ,axis-stride ,session-avg-stride ,(lambda (c) (stride->string c #t)))))

    (define selected-y-axis 0)

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
	    #f))))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (when new-session
        (setup-y-axis-items (map car y-axis-items))
        (set-y-axis (second (list-ref y-axis-items 0)))
        (set! selected-y-axis 0)
        (set! sport (session-sport new-session))
        (super set-session new-session)))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-cadence session))))

    ))


;;.......................................... vertical-oscillation-graph% ....

(define vertical-oscillation-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:vertical-oscillation-graph]
	       [text "Vertical Oscillation"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define/override (get-average-renderer)
      (let ((avg (session-avg-vertical-oscillation (get-session))))
          (if avg
              (let ((label (string-append "Avg " (number->string avg))))
                (function (lambda (x) avg) #:label label))
	    #f)))

    (define/override (set-session new-session)
      (super set-session #f)
      (set-y-axis axis-vertical-oscillation)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-vertical-oscillation session))))

    ))


;;..................................................... stance-time-graph% ....

(define stance-time-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:stance-time-graph]
	       [text "Ground Contact Time"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define y-axis-items
      `(("as time (ms)" ,axis-stance-time ,session-avg-stance-time ,number->string)
	("as percent" ,axis-stance-time-percent ,session-avg-stance-time-percent ,number->string)))

    (define selected-y-axis 0)

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
	    #f))))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-stance-time session))))
      
    ))


;;..................................................... power-graph% ....

(define power-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:power-graph]
	       [text "Power"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define selected-y-axis 0)
    (define zones #f)
    (define y-axis-items
      `(("Watts" ,axis-power ,identity ,number->string)
	("Zone" ,axis-power-zone ,(lambda (v) (val->zone v zones))
         ,(lambda (v) (format-48 "~1,1F" (val->zone v zones))))))

    (define/override (get-average-renderer)
      (let ((avg (session-avg-power (get-session))))
	(if avg
	    (let* ((item (list-ref y-axis-items selected-y-axis))
		   (pwr-converter (third item))
		   (pwr-formatter (fourth item))
		   (avg-val (pwr-converter avg))
		   (label (string-append "Avg " (pwr-formatter avg))))
	      (function (lambda (x) avg-val) #:label label))
	    #f)))
    
    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session session)
      (super set-session #f)
      (when session
        (define sid (assq1 'database-id session))
        (setup-y-axis-items (map car y-axis-items))
        (set-y-axis (second (list-ref y-axis-items 0)))
        (set! selected-y-axis 0)
        (set! zones (get-session-sport-zones sid 3))
        (super set-session session)))

    (define/override (is-valid-for-session? session)
      (not (not (session-avg-power session))))

    ))


;;..................................................... left-right-balance-graph% ....

(define left-right-balance-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:left-right-balance-graph]
	       [text "Left-Right Balance"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define/override (get-average-renderer)
      (let ((avg (session-left-right-balance (get-session))))
          (if avg
              (let ((label (string-append "Avg " (number->string avg))))
                (function (lambda (x) avg) #:label label))
	    #f)))

    (define/override (set-session new-session)
      (super set-session #f)
      (set-y-axis axis-left-right-balance)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (session-left-right-balance session))))
    
    ))


;;............................................... torque-effectiveness-graph% ....

(define torque-effectiveness-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:left-torque-effectiveness-graph]
	       [text "Torque Effectiveness"]
               [plot-builder (new plot-builder%)])

    (define y-axis-items
      `(("Both Pedals" ,axis-torque-effectiveness ,session-avg-torque-effectiveness ,number->string)
        ("Left Pedal" ,axis-left-torque-effectiveness ,session-avg-left-torque-effectiveness ,number->string)
	("Right Pedal" ,axis-right-torque-effectiveness ,session-avg-right-torque-effectiveness ,number->string)))

    (define selected-y-axis 0)

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
              #f))))
    
    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (or (session-avg-left-torque-effectiveness session)
                    (session-avg-right-torque-effectiveness session)))))
    
    ))


;;............................................... pedal-smoothness-graph% ....

(define pedal-smoothness-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:left-pedal-smoothness-graph]
	       [text "Pedal Smoothness"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define y-axis-items
      `(("Both Pedals" ,axis-pedal-smoothness ,session-avg-pedal-smoothness ,number->string)
        ("Left Pedal" ,axis-left-pedal-smoothness ,session-avg-left-pedal-smoothness ,number->string)
	("Right Pedal" ,axis-right-pedal-smoothness ,session-avg-right-pedal-smoothness ,number->string)))

    (define selected-y-axis 0)

     (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
	    #f))))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (or (session-avg-right-pedal-smoothness session)
                    (session-avg-left-pedal-smoothness session)))))

    ))


;;......................................... platform centre offset graph ....

(define platform-centre-offset-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:platform-centre-offset-graph]
	       [text "Platform Centre Offset"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define y-axis-items
      `(("Left Pedal" ,axis-left-platform-centre-offset ,session-avg-left-pco ,number->string)
        ("Right Pedal" ,axis-right-platform-centre-offset ,session-avg-right-pco ,number->string)))

    (define selected-y-axis 0)

     (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
	    #f))))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      (not (not (or (session-avg-right-pco session)
                    (session-avg-left-pco session)))))

    ))


;;.................................................... Power Phase Graph ....

(define power-phase-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:power-phase-graph]
	       [text "Power Phase"]
               [plot-builder (new plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)
    (inherit setup-y-axis-items)

    (define y-axis-items
      `(("Left PP Start" ,axis-left-power-phase-start ,session-avg-left-pp-start ,number->string)
        ("Left PP End" ,axis-left-power-phase-end ,session-avg-left-pp-end ,number->string)
        ("Left PP Angle" ,axis-left-power-phase-angle ,(lambda (session)
                                                         (let ((start (session-avg-left-pp-start session))
                                                               (end (session-avg-left-pp-end session)))
                                                           (if (and start end)
                                                               (let ((angle (- end start)))
                                                                 (if (< angle 0) (+ angle 360) angle))
                                                               #f)))
         ,number->string)
        ("Left Peak PP Start" ,axis-left-peak-power-phase-start ,session-avg-left-ppp-start ,number->string)
        ("Left Peak PP End" ,axis-left-peak-power-phase-end ,session-avg-left-ppp-end ,number->string)
        ("Left Peak PP Angle" ,axis-left-peak-power-phase-angle ,(lambda (session)
                                                         (let ((start (session-avg-left-ppp-start session))
                                                               (end (session-avg-left-ppp-end session)))
                                                           (if (and start end)
                                                               (let ((angle (- end start)))
                                                                 (if (< angle 0) (+ angle 360) angle))
                                                               #f)))
         ,number->string)
        ("Right PP Start" ,axis-right-power-phase-start ,session-avg-right-pp-start ,number->string)
        ("Right PP End" ,axis-right-power-phase-end ,session-avg-right-pp-end ,number->string)
        ("Right PP Angle" ,axis-right-power-phase-angle ,(lambda (session)
                                                         (let ((start (session-avg-right-pp-start session))
                                                               (end (session-avg-right-pp-end session)))
                                                           (if (and start end)
                                                               (let ((angle (- end start)))
                                                                 (if (< angle 0) (+ angle 360) angle))
                                                               #f)))
         ,number->string)
        ("Right Peak PP Start" ,axis-right-peak-power-phase-start ,session-avg-right-ppp-start ,number->string)
        ("Right Peak PP End" ,axis-right-peak-power-phase-end ,session-avg-right-ppp-end ,number->string)
        ("Right Peak PP Angle" ,axis-right-peak-power-phase-angle ,(lambda (session)
                                                         (let ((start (session-avg-right-ppp-start session))
                                                               (end (session-avg-right-ppp-end session)))
                                                           (if (and start end)
                                                               (let ((angle (- end start)))
                                                                 (if (< angle 0) (+ angle 360) angle))
                                                               #f)))
         ,number->string)
        ))

    (define selected-y-axis 0)

     (define/override (get-average-renderer)
      (let* ((item (list-ref y-axis-items selected-y-axis))
             (avg-fn (third item))
             (fmt-fn (fourth item)))
        (let ((avg (avg-fn (get-session))))
          (if avg
              (let* ((label (string-append "Avg " (fmt-fn avg))))
                (function (lambda (x) avg) #:label label))
	    #f))))

    (define/override (on-y-axis-selected index)
      (set! selected-y-axis index)
      (set-y-axis (second (list-ref y-axis-items index))))

    (define/override (set-session new-session)
      (super set-session #f)
      (setup-y-axis-items (map car y-axis-items))
      (set-y-axis (second (list-ref y-axis-items 0)))
      (set! selected-y-axis 0)
      (super set-session new-session))

    (define/override (is-valid-for-session? session)
      ;; Should we check all values?
      (not (not (or (session-avg-right-pp-start session)
                    (session-avg-left-pp-start session)))))

    ))


;;..................................................... swim-pace-graph% ....

(define swim-pace-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-pace-graph]
               [text "Swim Pace"]
               [plot-builder (new swim-plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)

    (define/override (get-average-renderer)
      (let ((avg (session-avg-speed (get-session))))
	(if (and avg (> avg 0))
            (let ((avg-pace (/ 1 (/ avg 100.0))))
              (function (lambda (x) avg-pace) 
                        #:label (string-append "Avg " (swim-pace->string avg #t))))
	    #f)))

    (define/override (set-session new-session)
      (super set-session #f)
      (set-y-axis axis-swim-pace)
      (super set-session new-session))
    
    ))


;;.................................................... swim-swolf-graph% ....

(define swim-swolf-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-swolf-graph]
               [text "SWOLF"]
               [plot-builder (new swim-plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)

    (set-y-axis axis-swim-swolf)

    (define/override (get-average-renderer)
      (let ((avg (session-avg-swolf (get-session))))
	(if (and avg (> avg 0))
            (function (lambda (x) avg) 
                      #:label (string-append "Avg " (n->string avg)))
	    #f)))
    ))


;;............................................. swim-stroke-count-graph% ....

(define swim-stroke-count-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-stroke-count-graph]
               [text "Stroke Count"]
               [plot-builder (new swim-plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)

    (set-y-axis axis-swim-stroke-count)

;    (define/override (get-average-renderer)
;      (let ((avg (session-avg-swolf (get-session))))
;	(if (and avg (> avg 0))
;            (function (lambda (x) avg) 
;                      #:label (string-append "Avg " (n->string avg)))
;	    #f)))
    ))


;;.................................................. swim-cadence-graph% ....

(define swim-cadence-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-cadence-graph]
               [text "Swim Cadence"]
               [plot-builder (new swim-plot-builder%)])

    (inherit set-y-axis)
    (inherit get-session)

    (set-y-axis axis-swim-avg-cadence)

    (define/override (get-average-renderer)
      (let ((avg (session-avg-cadence (get-session))))
	(if (and avg (> avg 0))
            (function (lambda (x) avg) 
                      #:label (string-append "Avg " (n->string avg)))
	    #f)))
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
       (new vertical-oscillation-graph% [parent graphs-panel])
       (new stance-time-graph% [parent graphs-panel])
       (new power-graph% [parent graphs-panel])
       (new left-right-balance-graph% [parent graphs-panel])
       (new torque-effectiveness-graph% [parent graphs-panel])
       (new pedal-smoothness-graph% [parent graphs-panel])
       (new platform-centre-offset-graph% [parent graphs-panel])
       (new power-phase-graph% [parent graphs-panel])
       ))
    
    (define swim-graphs
      (list
       (new swim-pace-graph% [parent graphs-panel])
       (new swim-swolf-graph% [parent graphs-panel])
       (new swim-stroke-count-graph% [parent graphs-panel])
       (new swim-cadence-graph% [parent graphs-panel])))

    (define (on-setup)
      (when the-session
        (let ((e (new visible-graphs-edit%)))
          (when (send e run-dialog
                      (send panel get-top-level-window)
                      (if (is-lap-swimming? the-session)
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
        (if (is-lap-swimming? session)
            (filter (lambda (g) (send g is-active?)) swim-graphs)
            (filter (lambda (g)
                      (and (send g is-active?)
                           (send g is-valid-for-session? session)))
                    default-graphs)))
      
      (set! graphs (get-graphs-for-session the-session))
      (let* ((sel (send x-axis-choice get-selection))
             (x-axis (cdr (list-ref x-axis-choices sel))))
        (for-each (lambda (g)
                    (send g set-x-axis x-axis)
                    (send g show-grid show-grid?)
                    (send g zoom-to-lap zoom-to-lap?)
                    (send g show-average-line show-avg?)
                    (send g set-session the-session)
                    (send g set-filter-amount filter-amount))
                  graphs))
      (send graphs-panel change-children 
            (lambda (old) (map (lambda (g) (send g get-panel)) graphs))))

    (define generation -1)

    (define/public (set-session session)
      ;; Clear the sessions from all graphs, this will allow it to be garbage
      ;; collected (as we won't set the session on all graphs all the time,
      ;; the previous session might stick around longer than intended.

      (for-each (lambda (g) (send g set-session #f)) default-graphs)
      (for-each (lambda (g) (send g set-session #f)) swim-graphs)
      
      (set! the-session session)
      (set! generation (+ 1 generation))
      (send lap-view set-session session)
      (let ((lap-swimming? (is-lap-swimming? session)))

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
