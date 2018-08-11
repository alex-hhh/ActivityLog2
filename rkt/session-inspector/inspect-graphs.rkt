#lang racket/base
;; inspect-graphs.rkt -- graphs for various data series for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require plot/no-gui
         racket/class
         racket/contract
         racket/gui/base
         (rename-in srfi/48 (format format-48))
         racket/list
         racket/match
         math/statistics
         racket/math
         plot/utils
         racket/dict
         "../fit-file/activity-util.rkt"
         "../al-widgets.rkt"
         "../fmt-util.rkt"
         "../series-meta.rkt"
         "../sport-charms.rkt"
         "../data-frame/df.rkt"
         "../data-frame/bsearch.rkt"
         "../data-frame/statistics.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "../session-df.rkt"
         "../plot-hack.rkt"
         "../plot-util.rkt")

(provide graph-panel%)
(provide elevation-graph%)

(define *header-font*
  (send the-font-list find-or-create-font 15 'default 'normal 'normal))

(define graph-title-font
  (send the-font-list find-or-create-font 9 'default 'normal 'normal))


;;.............................................................. helpers ....

(define (is-lap-swimming? data-frame)
  (df-get-property data-frame 'is-lap-swim?))


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

;; Return the X values in DATA-FRAME for START and END timestamps.  Returns
;; two values, start-x and end-x
(define (ivl-extents data-series start end)
  (define (by-timestamp v) (vector-ref v 2))
  (let ((start-idx (and start (bsearch data-series start #:key by-timestamp)))
        (end-idx (and end (bsearch data-series end #:key by-timestamp)))
        (max-idx (vector-length data-series)))
    (unless start-idx (set! start-idx 0))
    (unless (< start-idx max-idx)
      (set! start-idx (sub1 max-idx)))
    (unless (and end-idx (< end-idx max-idx))
      (set! end-idx (sub1 max-idx)))
    (values
     (vector-ref (vector-ref data-series start-idx) 0)
     (vector-ref (vector-ref data-series end-idx) 0))))

(define (find-change-point p0 p1 f0 f1 epsilon factor-fn key-fn)
  (match-define (vector x0 y0 z0 ...) p0) ; previous
  (match-define (vector x1 y1 z1 ...) p1) ; current
  (if (< (abs (- y1 y0)) epsilon)
      p1
      (let* ((pmid (apply vector (/ (+ x0 x1) 2) (/ (+ y0 y1) 2) z1))
             (fmid (factor-fn (key-fn pmid))))
        (if (equal? f0 fmid)
            (find-change-point pmid p1 f0 f1 epsilon factor-fn key-fn)
            (find-change-point p0 pmid f0 f1 epsilon factor-fn key-fn)))))

;; Split DATA-SERIES into continuous segments having the same factor
;; (according to FACTOR-FN).  For example, if the data series contains heart
;; rate and the FACTOR-FN returns zones, the function will split the data into
;; segments that have the same heart rate zone.
(define (split-by-factor data-series factor-fn
                         #:key (key-fn values)
                         #:epsilon (epsilon 0.1))
  (define result '())
  (define current-factor #f)
  (define seq '())

  (let loop ((index 0))
    (when (< index (vector-length data-series))
      (let* ([item (vector-ref data-series index)]
             [factor (factor-fn (key-fn item))])
        (unless current-factor
          (set! current-factor factor))
        (if (equal? factor current-factor)
            (begin
              (set! seq (cons item seq))    ; factor has not changed
              (loop (add1 index)))
            (let ((mp (find-change-point (car seq) item current-factor factor
                                         epsilon factor-fn key-fn)))
              (set! seq (cons mp seq))
              (set! result (cons (cons current-factor (reverse seq)) result))
              (set! seq (list mp))
              (set! current-factor (factor-fn (key-fn mp)))
              (if (equal? factor current-factor)
                  (begin
                    (set! seq (cons item seq))
                    (loop (add1 index)))
                  (loop index)))))))
  ;; Add last one
  (when current-factor
    (set! result (cons (cons current-factor (reverse seq)) result)))
  (reverse result))

;; Return a list of plot renderers for data created by `split-by-factor`.
(define (make-plot-renderer-for-splits fdata yr factor-colors)
  (for/list ((item fdata))
    (match-define (cons factor data) item)
    (define color (cdr (assoc factor factor-colors)))
    (make-plot-renderer data yr #:color color #:width 4.0)))

;; A function that generates integer tokens (effectively a counter).  It uses
;; a channel and a separate thread so that tokens can be correctly generated
;; from multiple threads.  Tokens are used in PS and PD structures to ensure
;; data consistency.
(define next-token
  (let ((c (make-channel)))
    (thread (lambda ()
              (let loop ((n 0))
                (channel-put c n)
                (loop (add1 n)))))
    (lambda () (channel-get c))))

;; Guard function for the PS structure.  Serves two purposes: generates a new
;; token each time a new PS structure is created and validates all the values
;; in the structure slots via a contract.
(define/contract (ps-guard
                  token
                  df
                  x-axis
                  y-axis
                  y-axis2
                  avg?
                  zoom?
                  color?
                  filter
                  ivl
                  struct-name)
  (-> nonnegative-integer?
      (or/c #f data-frame?)
      (or/c #f (is-a?/c series-metadata%))
      (or/c #f (is-a?/c series-metadata%))
      (or/c #f (is-a?/c series-metadata%))
      boolean?
      boolean?
      boolean?
      nonnegative-integer?
      (or/c #f (cons/c number? number?))
      any/c
      any)
  (values
   (next-token)
   df
   x-axis
   y-axis
   y-axis2
   avg?
   zoom?
   color?
   filter
   ivl))

;; Plot state, defines data and input parameters for a plot: data frame, what
;; axis do we plot, etc
(struct ps
  (;; Automatically set by the ps guard to a (next-token), regardless of what
   ;; you pass in here :-)
   token
   ;; The data-frame from which we extract data
   df
   ;; series-meta for X-axis, this will be time, elapsed time or distance
   x-axis
   ;; series-meta for Y-axis
   y-axis
   ;; series-meta for secondary Y-axis (for a dual axis plot), #f if there is
   ;; no secondary axis.
   y-axis2
   ;; show an average line on the plot
   avg?
   ;; zoom the selected lap on the plot
   zoom?
   ;; color the plot by factors (e.g. zones)
   color?
   ;; filter amount for the data on the plot, 0 means no filtering
   filter
   ;; interval to highlight.  This can be #f for no highlighted interval, or a
   ;; cons of start and end timestamps.
   ivl)
  #:guard ps-guard
  #:transparent)

;; The "empty" plot state, defined for convenience
(define empty-ps (ps 0 #f #f #f #f #f #f #f 0 #f))

;; Guard function for the PD structure.  Can be used to add contracts to its
;; fields (disabled for now, for performance reasons).
(define (pd-guard
         token
         sdata
         sdata2
         fdata
         y-range
         plot-rt
         hlivl
         struct-name)
  ;; (-> nonnegative-integer?
  ;;     (or/c #f ts-data/c)
  ;;     (or/c #f ts-data/c)
  ;;     (or/c #f (listof (cons/c any/c ts-data/c)))
  ;;     (or/c #f y-range/c)
  ;;     (or/c #f (treeof renderer2d?))
  ;;     (or/c #f (list number? number? (is-a?/c color%)))
  ;;     any/c
  ;;     any)
  (values
   token                             ; NOTE: don't generate a new token here!
   sdata
   sdata2
   fdata
   y-range
   plot-rt
   hlivl))

;; Plot Data, contains the data that is ready to plot, based on a
;; corresponding PS structure
(struct pd
  (;; Token is copied from a PS structure and is used to identify what PS
   ;; instance this data corresponds to.
   token
   ;; Plot data extracted from the data-frame for the first Y axis, see
   ;; `extract-data`
   sdata
   ;; Plot data extracted from the data-frame for the second Y axis, if any,
   ;; see `extract-data`
   sdata2
   ;; Factored data, if the data requires factoring (color? is #t).  Note that
   ;; we might have a FDATA here even if color? is #f in the corresponding PS
   ;; structure.  This is done to avoid re-factoring the data if the user
   ;; clicks on-off on the color repeatedly.
   fdata
   ;; The min and max Y values for the plot
   y-range
   ;; The plot renderer tree for the data
   plot-rt
   ;; The highlighted interval for the data.
   hlivl)
  #:guard pd-guard
  #:transparent)

;; An empty plot data structure, for convenience.
(define empty-pd (pd 0 #f #f #f #f #f #f))

;; Find the y values corresponding to the X value in the plot data.  This is
;; used to display the Y values on hover.  It returns six values: x, y1, y2
;; and labels for x y1 and y2 using the corresponding series formatter.  y2
;; and the label for y2 will be #f if there is no secondary axis defined for
;; the plot.
;;
;; NOTE: the y values displayed are the actual values at position X.  There
;; might be a discrepancy between what is displayed and what the plot shows,
;; as there is some filtering and line simplification going on for the plots.
(define (find-y-values plot-state x)
  (define df (ps-df plot-state))
  (define x-axis (ps-x-axis plot-state))
  (define y-axis (ps-y-axis plot-state))
  (define y-axis2 (ps-y-axis2 plot-state))
  (define xseries (send x-axis series-name))
  (define yseries1 (send y-axis series-name))
  (define yseries2 (if y-axis2 (send y-axis2 series-name) #f))
  (define xfmt (send x-axis value-formatter))
  (define yfmt (send y-axis value-formatter))
  (define y1 (or (df-lookup df xseries yseries1 x)
                 (send y-axis missing-value)))
  (define y2 (if yseries2
                 (or (df-lookup df xseries yseries2 x)
                     (send y-axis2 missing-value))
                 #f))
  ;; NOTE: we might land on a #f value in the original data series.  Normally,
  ;; we should look up a neighbor...
  (values x y1 y2 (xfmt x) (if y1 (yfmt y1) "") (if y2 (yfmt y2) "")))

;; Find the swim stroke name at the X value on the plot.  Returns #f if the
;; PLOT-STATE is not for a swim activity or the y-axis is not supposed to
;; color plots by swim stroke.  This means the function can be safely called
;; for any data frame + x-axis + y-axis combination and will only return a
;; swim stroke if appropriate.
(define (find-swim-stroke plot-state x)
  (define df (ps-df plot-state))
  (define xseries (send (ps-x-axis plot-state) series-name))
  (and
   (send (ps-y-axis plot-state) plot-color-by-swim-stroke?)
   (df-contains? df xseries "swim_stroke")
   (let ((stroke (df-lookup df xseries "swim_stroke" x)))
     (and stroke (get-swim-stroke-name stroke)))))

;; Produce a renderer tree from the data in the PD and PS structures.  We
;; assume the PD structure is up-to date w.r.t PD structure.
(define/contract (plot-data-renderer-tree pd ps)
  (-> pd? ps? (or/c #f (treeof renderer2d?)))
  (let ((df (ps-df ps))
        (y (ps-y-axis ps))
        (y2 (ps-y-axis2 ps))
        (sdata (pd-sdata pd))
        (sdata2 (pd-sdata2 pd))
        (fdata (pd-fdata pd))
        (y-range (pd-y-range pd)))
    (cond ((and df (is-lap-swimming? df)
                (send y plot-color-by-swim-stroke?)
                (df-contains? df "swim_stroke"))
           (make-plot-renderer/swim-stroke
            sdata (df-select df "swim_stroke")))
          ((and (ps-color? ps) fdata)
           (make-plot-renderer-for-splits fdata y-range (send y factor-colors)))
          ((and sdata sdata2)
           (list
            (make-plot-renderer sdata y-range
                                #:color (send y plot-color)
                                #:width 1
                                #:alpha 0.9
                                #:label (send y plot-label))
            (make-plot-renderer sdata2 y-range
                                #:color (send y2 plot-color)
                                #:width 1
                                #:alpha 0.9
                                #:label (send y2 plot-label))))
          (sdata
           (make-plot-renderer sdata y-range
                               #:color (send y plot-color)))
          (sdata2
           (error "plot-data-renderer-tree -- unexpected combination"))
          (#t #f))))

;; Produce a highlight interval data structure from the PD and PS structures.
;; We assume the PD structure is up-to date w.r.t PD structure.
(define/contract (plot-highlight-interval pd ps)
  (-> pd? ps? (or/c #f (list/c number? number? any/c)))
  (let ((df (ps-df ps))
        (y (ps-y-axis ps))
        (ivl (ps-ivl ps))
        (sdata (pd-sdata pd)))
    (if (and (cons? ivl) df y sdata)
        (let-values (((start end) (ivl-extents sdata (car ivl) (cdr ivl))))
          (let ((c (send y plot-color)))
            (list start end (make-object color% (send c red) (send c green) (send c blue) 0.2))))
        #f)))

;; Produce a new PD structure given an old PD and PS structure and a new PS
;; structure.  This function determines what has changed between OLD-PS and
;; NEW-PS and re-computes only what is needed, the remaining data is taken
;; from OLD-PD.
(define/contract (update-plot-data old-pd old-ps new-ps)
  (-> pd? ps? ps? pd?)
  (unless (equal? (ps-token old-ps) (pd-token old-pd))
    (error "update-plot-data -- token mismatch"))
  (cond
    ((or (eq? old-ps new-ps)
         (equal? (ps-token old-ps) (ps-token new-ps)))
     old-pd)
    (#t
     ;; need a new sdata if the df, x-axis, y-axis or filter amount have
     ;; changed, or there is no sdata in old-pd
     (define need-sdata?
       (and (ps-df new-ps)
            (ps-x-axis new-ps)
            (ps-y-axis new-ps)
            (or (not (pd-sdata old-pd))
                (not (and (eq? (ps-df old-ps) (ps-df new-ps))
                          (eq? (ps-x-axis old-ps) (ps-x-axis new-ps))
                          (eq? (ps-y-axis old-ps) (ps-y-axis new-ps))
                          (equal? (ps-filter old-ps) (ps-filter new-ps)))))))
     ;; need a new sdata2 if df, x-axis, y-axis2 or filter amount have
     ;; changed, or there is no sdata2 in old-pd.
     (define need-sdata2?
       (and (ps-df new-ps)
            (ps-x-axis new-ps)
            (ps-y-axis2 new-ps)
            (or (not (pd-sdata2 old-pd))
                (not (and (eq? (ps-df old-ps) (ps-df new-ps))
                          (eq? (ps-x-axis old-ps) (ps-x-axis new-ps))
                          (eq? (ps-y-axis2 old-ps) (ps-y-axis2 new-ps))
                          (equal? (ps-filter old-ps) (ps-filter new-ps)))))))
     ;; need new fdata if we update sdata or color? has changed or there is no
     ;; fdata in old-pd
     (define need-fdata?
       (and (ps-color? new-ps)
            (or need-sdata?
                (not (pd-fdata old-pd)))))

     ;; Calculate new SDATA if NEED-SDATA? is #t, or re-use the one from
     ;; OLD-PD
     (define sdata
       (if need-sdata?
           (let* ((df (ps-df new-ps))
                  (x (ps-x-axis new-ps))
                  (y (ps-y-axis new-ps))
                  (filter (ps-filter new-ps))
                  (lap-swimming? (is-lap-swimming? df)))
             (if (df-contains? df (send x series-name) (send y series-name))
                 (let ([ds (extract-data df x y filter (not lap-swimming?))])
                   (if lap-swimming? (add-verticals ds) ds))
                 #f))
           (pd-sdata old-pd)))
     ;; Calculate new SDATA2 if NEED-SDATA2? is #t, or re-use the one from
     ;; OLD-PD (but only if we actually have an y-axis2)
     (define sdata2
       (if need-sdata2?
           (let* ((df (ps-df new-ps))
                  (x (ps-x-axis new-ps))
                  (y (ps-y-axis2 new-ps))
                  (filter (ps-filter new-ps))
                  (lap-swimming? (is-lap-swimming? df)))
             (if (df-contains? df (send x series-name) (send y series-name))
                 (let ([ds (extract-data df x y filter (not lap-swimming?))])
                   (if lap-swimming? (add-verticals ds) ds))
                 #f))
           (if (ps-y-axis2 new-ps)
               (pd-sdata2 old-pd) ; don't reuse sdata2 unless there is an y-axis2
               #f)))
     ;; Calculate a new FDATA if NEED-FDATA? is #t, or re-use the one from
     ;; OLD-PD.  Note that we keep the old FDATA around even if color? is now
     ;; #f.
     (define fdata
       (if need-fdata?
           (let ((df (ps-df new-ps))
                 (y (ps-y-axis new-ps)))
             (let* ((sport (df-get-property df 'sport))
                    (sid (df-get-property df 'session-id))
                    (factor-fn (send y factor-fn sport sid))
                    (factor-colors (send y factor-colors))
                    (epsilon (expt 10 (- (send y fractional-digits)))))
               (if (and factor-fn sdata)
                   (split-by-factor sdata factor-fn
                                    #:key (lambda (v) (vector-ref v 1))
                                    #:epsilon epsilon)
                   #f)))
           (pd-fdata old-pd)))
     ;; Calculate new Y-RANGE, if needed, or reuse the one from OLD-PD.
     (define y-range
       (if (or need-sdata? need-sdata2?)
           (let* ((y (ps-y-axis new-ps))
                  (y2 (ps-y-axis2 new-ps))
                  (st1 (if sdata (ds-stats sdata) #f))
                  (st2 (if sdata2 (ds-stats sdata2) #f))
                  (yr1 (if st1 (get-plot-y-range st1 y) #f))
                  (yr2 (if st2 (get-plot-y-range st2 y2) #f)))
             (cond ((and yr1 yr2)
                    (combine-y-range yr1 yr2))
                   (yr1)
                   (yr2)
                   (#t #f)))
           (pd-y-range old-pd)))

     ;; Temporary PD structure, used to pass them on to the renderer tree
     ;; functions.
     (define tmp-pd (pd (ps-token new-ps) sdata sdata2 fdata y-range #f #f))

     ;; Calculate a new plot renderer tree if needed, or reuse the one from
     ;; OLD-PD
     (define plot-rt
       (if (and (ps-df new-ps) (ps-x-axis new-ps) (ps-y-axis new-ps))
           (if (or need-sdata? need-sdata2? need-fdata?
                   (not (equal? (ps-color? old-ps) (ps-color? new-ps))))
               (plot-data-renderer-tree tmp-pd new-ps)
               (pd-plot-rt old-pd))
           #f))
     ;; Calculate a new highlight interval renderer tree if needed, or reuse
     ;; the one from OLD-PD
     (define hlivl
       (if (and (ps-df new-ps) (ps-x-axis new-ps) (ps-y-axis new-ps))
           (if (or need-sdata? need-sdata2? need-fdata?
                   (and (ps-ivl new-ps)
                        (not (equal? (ps-ivl old-ps) (ps-ivl new-ps)))))
               (plot-highlight-interval tmp-pd new-ps)
               (pd-hlivl old-pd))
           #f))

     ;; Put the renderer tree in the structure and return the result.
     (struct-copy pd tmp-pd (plot-rt plot-rt) (hlivl hlivl)))))

(define graph-view%
  (class object%
    (init parent)
    (init-field text tag
                [min-height 250]
                [hover-callback (lambda (x) (void))]
                [style '()])
    (super-new)

    (define previous-plot-state empty-ps)
    (define plot-state empty-ps)
    (define plot-data empty-pd)

    ;; True if the plot is shown, false if hidden, controlled by the Show/Hide
    ;; button.
    (define show-graph? #t)

    ;; When #t, the plot is not refreshed when parameters change.  This is
    ;; used to prevent multiple refresh calls when there are multiple changes
    ;; in one go.
    (define flush-suspended? #f)

    ;; Token corresponding to the PD instance that was used to generate the
    ;; CACHED-BITMAP.  If cached-bitmap-token is not the same as (ps-token
    ;; plot-state), the cached-bitmap is outdated.
    (define cached-bitmap-token #f)

    (define y-axis-by-sport (make-hash)) ; saved as a preference

    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define export-file-name #f)

    (let ((pref (get-pref tag (lambda () #f))))
      (when (and pref (eqv? (length pref) 3))
        (set! show-graph? (second pref))
        (set! y-axis-by-sport (hash-copy (third pref)))))

    ;; The panel that contains the entire graph view
    (define panel (new (class vertical-panel%
                         (init) (super-new)
                         (define/public (interactive-export-image)
                           (on-interactive-export-image)))
                       [parent parent]
                       [style (remove-duplicates (append '(border) style))]
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
                      (send button set-label "Hide")
                      (refresh))
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
      (send optional-items-panel change-children (lambda (old) '()))

      (when y-axis-choices
	(set! y-axis-choice
              (new choice% [parent optional-items-panel]
                   [label "Display: "]
                   [choices y-axis-choices]
                   [callback (lambda (c e)
                               (let* ((df (ps-df plot-state))
                                      (index (send c get-selection))
                                      (sport (and df (df-get-property df 'sport))))
                                 (when sport
                                   (hash-set! y-axis-by-sport sport index))
                                 (on-y-axis-selected index)))]))
        (send y-axis-choice set-selection 0)
        (on-y-axis-selected 0)))

    (define (put-plot output-fn pd ps)
      ;; (-> (-> (treeof renderer2d?) any/c) pd? ps? any/c)

      (define (full-render-tree)
        (let ((render-tree (list (pd-plot-rt pd))))
          (set! render-tree (cons (tick-grid) render-tree))
          (when (ps-avg? ps)
            (let ((avg (get-average-renderer)))
              (when avg (set! render-tree (cons avg render-tree)))))
          (reverse render-tree)))

      (define (get-x-transform)
        (let ((ivl (ps-ivl ps))
              (sdata (pd-sdata pd)))
          (if (and (cons? ivl) (ps-zoom? ps))
              (let-values (((start end) (ivl-extents sdata (car ivl) (cdr ivl))))
                (stretch-transform start end 30))
              id-transform)))

      (define x-axis (ps-x-axis ps))
      (define y-axis (ps-y-axis ps))

      (define (get-x-axis-ticks)
        (let ((ticks (send x-axis plot-ticks))
              (ivl (ps-ivl ps))
              (sdata (pd-sdata pd)))
          (if (and (cons? ivl) (ps-zoom? ps))
              (let-values (((start end) (ivl-extents sdata (car ivl) (cdr ivl))))
                (ticks-add ticks (list start end)))
              ticks)))

      (parameterize ([plot-x-transform (get-x-transform)]
                     [plot-x-ticks (get-x-axis-ticks)]
                     [plot-x-label (send x-axis axis-label)]
                     [plot-y-ticks (send y-axis plot-ticks)]
                     [plot-y-label (send y-axis axis-label)])
        (output-fn (full-render-tree))))

    (define (put-plot/canvas canvas pd ps)
      (put-plot (lambda (rt) (plot-snip/hack canvas rt)) pd ps))

    (define (put-plot/file file-name width height pd ps)
      ;; (-> path-string? exact-positive-integer? exact-positive-integer? pd? ps? any/c)
      (put-plot
       (lambda (renderer-tree)
         (plot-file renderer-tree file-name #:width width #:height height))
       pd ps))

    (define graph-canvas
      (new snip-canvas% [parent panel]
           [min-height min-height]
           [style (if show-graph? '() '(deleted))]))

    (define/public (suspend-flush)
      (send graph-canvas suspend-flush)
      (set! flush-suspended? #t))
    (define/public (resume-flush)
      (send graph-canvas resume-flush)
      (set! flush-suspended? #f))

    (define the-plot-snip #f)

    (define/public (draw-marker-at x)
      (when (and the-plot-snip show-graph?)
        (let ((rt '()))
          (define (add-renderer r) (set! rt (cons r rt)))
          ;; Add the highlight overlay back in...
          (when (pd-hlivl plot-data)
            (match-define (list xmin xmax color) (pd-hlivl plot-data))
            (add-renderer (pu-vrange xmin xmax color)))
          (when x
            (define-values (_ y1 y2 xlab ylab1 ylab2) (find-y-values plot-state x))
            (cond ((and y1 y2)
                   (let ((label (string-append ylab1 "/" ylab2 " @ " xlab)))
                     (add-renderer (pu-label x (max y1 y2) label))))
                  (y1
                   (let ((label (string-append ylab1 " @ " xlab))
                         (swim-stroke (find-swim-stroke plot-state x)))
                     (add-renderer (pu-label x y1 label swim-stroke))))
                  (y2
                   (let ((label (string-append ylab2 " @ " xlab)))
                     (add-renderer (pu-label x y2 label)))))
            (add-renderer (pu-vrule x)))
          (set-overlay-renderers the-plot-snip rt))))

    (define (plot-hover-callback snip event x y)
      (if (good-hover? x y event)
          (hover-callback x)
          (hover-callback #f)))

    (define (refresh-plot)
      (let ((pstate plot-state)
            (ppstate previous-plot-state)
            (pdata plot-data))
        (queue-task
         "graph-view%/refresh-plot"
         (lambda ()
           (let ((npdata (update-plot-data pdata ppstate pstate)))
             (queue-callback
              (lambda ()
                (if (= (pd-token npdata) (ps-token plot-state))
                    (begin
                      (if (pd-plot-rt npdata)
                          (begin
                            (set! the-plot-snip (put-plot/canvas graph-canvas npdata pstate))
                            (set-mouse-event-callback the-plot-snip plot-hover-callback)
                            (when (pd-hlivl npdata)
                              (match-define (list xmin xmax color) (pd-hlivl npdata))
                              (set-overlay-renderers the-plot-snip (list (pu-vrange xmin xmax color)))))
                          (begin
                            (send graph-canvas set-snip #f)
                            (send graph-canvas set-background-message "No data for plot...")))
                      (set! previous-plot-state pstate)
                      (set! plot-state pstate)
                      (set! plot-data npdata)
                      (set! cached-bitmap-token (pd-token npdata))
                      (send graph-canvas refresh))
                    (void)))))))))

    (define (refresh-cached-bitmap)
      (let ((pstate plot-state)
            (pdata plot-data))
        (queue-task
         "graph-view%/refresh-cached-bitmap"
         (lambda ()
           (when (pd-plot-rt pdata)
             (queue-callback
              (lambda ()
                (if (= (pd-token pdata) cached-bitmap-token)
                    (if (pd-plot-rt pdata)
                        (begin
                          (set! the-plot-snip (put-plot/canvas graph-canvas pdata pstate))
                          (set-mouse-event-callback the-plot-snip plot-hover-callback)
                          (when (pd-hlivl pdata)
                            (match-define (list xmin xmax color) (pd-hlivl pdata))
                            (set-overlay-renderers the-plot-snip (list (pu-vrange xmin xmax color)))))
                        (begin
                          (send graph-canvas set-snip #f)
                          (send graph-canvas set-background-message "No data for plot...")))
                    (void)))))))))

    (define (refresh)
      (when (and (not flush-suspended?) show-graph?)
        (if (equal? (pd-token plot-data) (ps-token plot-state))
            (refresh-cached-bitmap)
            (refresh-plot))))

    (define/public (get-average-renderer)
      #f)

    (define/public (on-y-axis-selected index)
      #f)

    ;; Return #t if this graph can display some data for DATA-FRAME (e.g. a
    ;; cadence graph is only valid if there is cadence series in the data
    ;; frame).  This needs to be overridden.
    (define/public (is-valid-for? data-frame) #f)

    (define/public (save-visual-layout)
      (put-pref
       tag
       (list #t show-graph? y-axis-by-sport)))

    (define/public (set-data-frame df)
      (suspend-flush)
      (set! plot-state (struct-copy ps plot-state [df df] [ivl #f]))
      (set! export-file-name #f)
      (when (and y-axis-choice df)
        (let* ((sport (df-get-property df 'sport))
               (y-axis-index (hash-ref y-axis-by-sport sport 0)))
          (send y-axis-choice set-selection y-axis-index)
          (on-y-axis-selected y-axis-index)))
      (resume-flush)
      ;; When a new data frame is set, remove the old plot immediately, as it
      ;; is not relevant anymore.
      (send graph-canvas set-snip #f)
      (send graph-canvas set-background-message "Working...")
      (refresh))

    (define/public (zoom-to-lap zoom)
      (set! plot-state (struct-copy ps plot-state [zoom? zoom]))
      (refresh))

    (define/public (color-by-zone flag)
      (set! plot-state (struct-copy ps plot-state [color? flag]))
      (refresh))

    (define/public (set-filter-amount a)
      (set! plot-state (struct-copy ps plot-state [filter a]))
      (refresh))

    (define/public (show-average-line show)
      (set! plot-state (struct-copy ps plot-state [avg? show]))
      (refresh))

    (define/public (set-x-axis new-x-axis)
      (set! plot-state (struct-copy ps plot-state [x-axis new-x-axis]))
      (refresh))

    (define/public (set-y-axis new-y-axis (new-y-axis2 #f))
      (set! plot-state (struct-copy ps plot-state [y-axis new-y-axis] [y-axis2 new-y-axis2]))
      (set! export-file-name #f)
      (refresh))

    (define/public (highlight-interval start-timestamp end-timestamp)
      (set! plot-state (struct-copy ps plot-state [ivl (cons start-timestamp end-timestamp)]))
      ;; need full refresh if zoom to lap is set, as the actual plotted data will change.
      (if (ps-zoom? plot-state)
          (refresh)
          (begin
            (set! plot-data (update-plot-data plot-data previous-plot-state plot-state))
            (set! previous-plot-state plot-state)
            (when the-plot-snip
              (if (pd-hlivl plot-data)
                  (match-let (((list xmin xmax color) (pd-hlivl plot-data)))
                    (set-overlay-renderers the-plot-snip (list (pu-vrange xmin xmax color))))
                  (set-overlay-renderers #f))))))

    (define/public (get-data-frame) (ps-df plot-state))

    (define/public (export-image-to-file file-name)
      (put-plot/file file-name 800 300 plot-data plot-state))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define (get-default-export-file-name)
      (or export-file-name
          (let* ((df (ps-df plot-state))
                 (y (ps-y-axis plot-state))
                 (y2 (ps-y-axis2 plot-state))
                 (sid (df-get-property df 'session-id))
                 (s1 (and y (send y series-name)))
                 (s2 (and y2 (send y2 series-name))))
            (cond ((and sid s1 s2)
                   (format "graph-~a-~a-~a.png" sid s1 s2))
                  ((and sid s1)
                   (format "graph-~a-~a.png" sid s1))
                  (#t
                   "graph.png")))))

    (define/public (on-interactive-export-image)
      (let ((file (put-file "Select file to export to" #f #f
                            (get-default-export-file-name) "png" '()
                            '(("PNG Files" "*.png") ("Any" "*.*")))))
        (when file
          (set! export-file-name file)
          (export-image-to-file file))))

    (define/public (get-name) text)
    (define/public (get-tag) tag)

    ))


;;......................................................... speed-graph% ....

(define speed-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:speed-graph]
               [text "Speed "])
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
      `(("Speed" ,axis-speed ,convert-m/s->speed ,speed->string)
	("Pace" ,axis-pace ,convert-m/s->pace ,pace->string)
        ("Zone" ,axis-speed-zone ,(lambda (x) (val->zone x zones)) ,(lambda (x y) (format-48 "~1,1F" x)))
        ("GAP" ,axis-gap ,convert-m/s->pace ,pace->string)))

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
        (define sid (df-get-property data-frame 'session-id))
        (set! zones (get-session-sport-zones sid 2)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (for/or ([series '("speed" "pace" "speed-zone")])
        (df-contains? data-frame series)))

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
               [text "Elevation "])
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
      (df-contains/any? data-frame "alt" "calt"))

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
               [text "Heart Rate "])

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
      `(("BPM" ,axis-hr-bpm ,values ,heart-rate->string/bpm)
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
        (define sid (df-get-property data-frame 'session-id))
        (set! avg-hr #f)
        (set! zones (get-session-sport-zones sid 1)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "hr" "hr-pct" "hr-zone"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;....................................................... cadence-graph% ....

(define cadence-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:cadence-graph]
               [text "Cadence "])
    (inherit set-y-axis get-data-frame setup-y-axis-items)

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
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-cadence #f)
      (set! avg-stride #f)
      (when data-frame
        (let ((sp (df-get-property data-frame 'sport)))
          (set! sport (vector-ref sp 0))))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "cad" "stride"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;................................................... vosc-vratio-graph% ....

(define vosc-vratio-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:vosc-vratio-graph]
               [text "Vertical Oscillation "])

    (inherit set-y-axis get-data-frame setup-y-axis-items)
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
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-vosc #f)
      (set! avg-vratio #f)
      (if data-frame
          (let ((sp (df-get-property data-frame 'sport)))
            (set! sport (vector-ref sp 0)))
          (set! sport #f))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "vosc" "vratio"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)

    ))


;;........................................................... gct-graph% ....

(define gct-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:gct-graph]
               [text "Ground Contact Time "])

    (inherit set-y-axis get-data-frame setup-y-axis-items)

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
        (set-y-axis (second (list-ref y-axis-items index)))))

    (define/override (set-data-frame data-frame)
      (set! avg-gct #f)
      (set! avg-gct-pct #f)
      (when data-frame
        (let ((sp (df-get-property data-frame 'sport)))
          (set! sport (vector-ref sp 0))))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "gct" "pgct"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)


    ))


;.......................................................... wbal-graph% ....

(define wbal-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:wbal-graph]
               [text "W' Bal "])

    (send this set-y-axis axis-wbal)

    (define/override (is-valid-for? data-frame)
      (df-contains? data-frame "wbal"))

    ))


;;..................................................... power-graph% ....

(define power-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:power-graph]
               [text "Power "])
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
      `(("Watts" ,axis-power ,values ,power->string)
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
        (define sid (df-get-property data-frame 'session-id))
        (set! zones (get-session-sport-zones sid 3)))
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "pwr" "pwr-zone"))

    (setup-y-axis-items (map car y-axis-items))
    (set-y-axis (second (list-ref y-axis-items 0)))
    (set! selected-y-axis 0)
    ))


;;..................................................... left-right-balance-graph% ....

(define lrbal-graph%
  (class graph-view%
    (init parent)

    (super-new [parent parent]
               [tag 'activity-log:lrbal-graph]
               [text "Left-Right Balance "])

    (inherit set-y-axis get-data-frame setup-y-axis-items)

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
      (super set-data-frame data-frame))

    (define/override (is-valid-for? data-frame)
      (df-contains? data-frame "lrbal"))

    ))


;;.......................................................... teff-graph% ....

(define teff-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:teff-graph]
               [text "Torque Effectiveness "])

    (send this set-y-axis
          axis-left-torque-effectiveness
          axis-right-torque-effectiveness)

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "lteff" "rteff"))

    ))


;;......................................................... psmth-graph% ....

(define psmth-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:psmth-graph]
               [text "Pedal Smoothness "])

    (send this set-y-axis
          axis-left-pedal-smoothness
          axis-right-pedal-smoothness)

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "lpsmth" "rpsmth"))

    ))


;;........................................................... pco-graph% ....

(define pco-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:pco-graph]
               [text "Platform Centre Offset "])

    (send this set-y-axis
          axis-left-platform-centre-offset
          axis-right-platform-centre-offset)

    (define/override (is-valid-for? data-frame)
      (df-contains/any? data-frame "lpco" "rpco"))

    ))


;;.................................................... Power Phase Graph ....

(define power-phase-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:power-phase-graph]
               [text "Power Phase "])

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
      (df-contains/any?
       data-frame
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
               [text "Swim Pace "])

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
               [text "SWOLF "])

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
      (df-contains? data-frame "swolf"))

    ))


;;............................................. swim-stroke-count-graph% ....

(define swim-stroke-count-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-stroke-count-graph]
               [text "Stroke Count "])

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
      (df-contains? data-frame "strokes"))

    ))


;;.................................................. swim-cadence-graph% ....

(define swim-cadence-graph%
  (class graph-view%
    (init parent)
    (super-new [parent parent]
               [tag 'activity-log:swim-cadence-graph]
               [text "Swim Cadence "])

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
      (df-contains? data-frame "cad"))

    ))


;;............................................ select-data-series-dialog% ....

;; A dialog box used to select the data series that are displayed in the
;; charts window.
(define select-data-series-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Data Series"] [icon (edit-icon)]
               [min-width 600] [min-height 450])

    ;; List of graphs that are visible (their order determines how they are
    ;; shown
    (define visible #f)
    ;; List of graphs that are available, but not visible
    (define available #f)

    (define available-lb #f)
    (define visible-lb #f)
    (define show-hide-button #f)
    (define move-up-button #f)
    (define move-down-button #f)

    (let ((p (send this get-client-pane)))
      (let ((p1 (make-horizontal-pane p)))
        (send p1 set-alignment 'center 'center)
        (let ((lp (make-vertical-pane p1)))
          (send lp set-alignment 'center 'center)
          (new message% [parent lp] [label "Available Series"])
          (set! available-lb
                (new list-box% [parent lp] [label #f] [choices '()]
                     [style '(single)]
                     [callback (lambda (s e)
                                 (on-available-selected (send s get-selection)))])))
        (let ((lp (make-vertical-pane p1)))
          (send lp set-alignment 'center 'center)
          (set! show-hide-button
                (new button% [parent lp] [label "Show"]
                     [callback (lambda (b e) (on-show-hide))]))
          (set! move-up-button
                (new button% [parent lp] [label "Move Up"]
                     [callback (lambda (b e) (on-move -1))]))
          (set! move-down-button
                (new button% [parent lp] [label "Move Down"]
                     [callback (lambda (b e) (on-move 1))])))
        (let ((lp (make-vertical-pane  p1)))
          (send lp set-alignment 'center 'center)
          (new message% [parent lp] [label "Visible Series"])
          (set! visible-lb
                (new list-box% [parent lp] [label #f] [choices '()]
                     [style '(single)]
                     [callback (lambda (s e)
                                 (on-visible-selected (send s get-selection)))]))))
      #f)

    (define (enable-disable-buttons)
      (cond ((send visible-lb get-selection)
             => (lambda (index)
                  (send show-hide-button set-label "Hide")
                  (send show-hide-button enable #t)
                  (send move-up-button enable (> index 0))
                  (send move-down-button enable (< index (sub1 (length visible))))))
            ((send available-lb get-selection)
             => (lambda (index)
                  (send show-hide-button set-label "Show")
                  (send show-hide-button enable #t)
                  (send move-up-button enable #f)
                  (send move-down-button enable #f)))
            (#t
             (send show-hide-button set-label "Show")
             (send show-hide-button enable #f)
             (send move-up-button enable #f)
             (send move-down-button enable #f))))

    ;; Un-select any selected item in LB (a list-box%).  Also update the
    ;; states based on the new selection state.
    (define (unselect lb)
      (define index (send lb get-selection))
      (when index
        (send lb select index #f))
      (enable-disable-buttons))

    ;; Called when an item in the available list-box% is selected.  It will
    ;; de-select the item in the visible list box and update buttons for the
    ;; new selection.  This ensures that only one item is selected in either
    ;; list boxes.
    (define (on-available-selected index) (unselect visible-lb))

    ;; Called when an item in the visible list-box% is selected.  It will
    ;; de-select the item in the available list box and update buttons for the
    ;; new selection.  This ensures that only one item is selected in either
    ;; list boxes.
    (define (on-visible-selected index) (unselect available-lb))

    ;; Called when the user clicks on the "Move Up" or "Move Down" buttons.
    ;; It will move the selected item in the VISIBLE-LB list-box% up (if
    ;; DIRECTION is -1) or down (if DIRECTION is 1).  The corresponding
    ;; VISIBLE list is also updated.
    (define (on-move direction)
      (define index (send visible-lb get-selection))
      ;; An item is selected and he list has at least 2 elements
      (when (and index (>= (length visible) 2))
        (define nindex (+ index direction))
        (define i1 (min index nindex))
        (define i2 (max index nindex))
        (when (and (>= i1 0) (< i2 (length visible)))
          (define h (take visible i1))
          (define t (drop visible i1))
          (set! visible
                (append h (cons (car (cdr t)) (cons (car t) (cdr (cdr t))))))
          (send visible-lb clear)
          (for ([item visible])
            (send visible-lb append (send item get-name)))
          (send visible-lb set-selection nindex)
          (enable-disable-buttons))))

    ;; Populate the VISIBLE-LB and AVAILABLE-LB list-box% objects with data
    ;; from the VISIBLE and AVAILABLE lists.
    (define (refill)
      (send visible-lb clear)
      (for ([item visible])
        (send visible-lb append (send item get-name)))
      (send available-lb clear)
      (for ([item available])
        (send available-lb append (send item get-name))))

    ;; Called when the user clicks on the "Show/Hide" button.  If an item is
    ;; selected in the VISIBLE-LB it will be moved into the AVAILABLE-LB and
    ;; vice-versa.  The VISIBLE and AVAILABLE lists are also updated
    ;; accordingly.
    (define (on-show-hide)
      (cond ((send visible-lb get-selection)
             => (lambda (index)
                  (set! available
                        (append available (list (list-ref visible index))))
                  (set! visible
                        (append (take visible index)
                                (drop visible (add1 index))))
                  (refill)
                  ;; Select the next visible item, this way, if the user
                  ;; repeats the click, he can remove another item
                  (let ((nindex (sub1 index)))
                    (when (and (> nindex 0) (> (length visible) nindex))
                      (send visible-lb set-selection nindex)))
                  (enable-disable-buttons)))
            ((send available-lb get-selection)
             => (lambda (index)
                  (set! visible
                        (append visible (list (list-ref available index))))
                  (set! available
                        (append (take available index)
                                (drop available (add1 index))))
                  (refill)
                  ;; Select the next available item, this way, if the user
                  ;; repeats the click, he can add another item.
                  (let ((nindex (sub1 index)))
                    (when (and (> nindex 0) (> (length available) nindex))
                      (send available-lb set-selection nindex)))
                  (enable-disable-buttons)))))

    ;; Show the dialog to allow the user to select which graphs should be
    ;; visible and what the presentation order is.
    ;;
    ;; PARENT is the parent window for the dialog.
    ;;
    ;; VISIBLE-TAGS is a list of graph tags that are visible (and the order in
    ;; which they should be displayed.  As a special value, #f means that all
    ;; graphs are visible, and '() means that no graphs are visible.
    ;;
    ;; ALL-GRAPHS is a list of all possible graphs objects
    ;;
    ;; Dialog returns an updated list of VISIBLE-TAGS (or #f if the user
    ;; canceled the dialog)
    (define/public (show-dialog parent visible-tags all-graphs)

      ;; split ALL-GRAPHS into visible and available based on VISIBLE-TAGS
      (cond ((eq? visible-tags #f)
             (set! visible all-graphs)
             (set! available '()))
            ((null? visible-tags)
             (set! visible '())
             (set! available all-graphs))
            (#t
             (set! visible
                   (for*/list ([tag visible-tags]
                               [g (in-value (findf (lambda (g) (eq? tag (send g get-tag))) all-graphs))]
                               #:when g)
                     g))
             (set! available
                   (for/list ([g all-graphs] #:unless (member g visible)) g))))
      (refill)
      (enable-disable-buttons)
      (if (send this do-edit parent)
          (for/list ([item visible]) (send item get-tag))
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

(define (make-default-graphs parent hover-callback)
  ;; NOTE: all graphs are created as 'deleted, so they are not visible.  The
  ;; graphs-panel% will control visibility by adding/removing graphs from the
  ;; parent panel
  (list
   (new speed-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new elevation-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new heart-rate-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new cadence-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new vosc-vratio-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new gct-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new power-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new wbal-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new lrbal-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new teff-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new psmth-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new pco-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new power-phase-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])))

(define (make-swim-graphs parent hover-callback)
  ;; NOTE: all graphs are created as '(deleted), so they are not visible.  The
  ;; graphs-panel% will control visibility by adding/removing graphs from the
  ;; parent panel
  (list
   (new swim-pace-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new swim-swolf-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new swim-stroke-count-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])
   (new swim-cadence-graph% [parent parent] [style '(deleted)] [hover-callback hover-callback])))

(define graph-panel%
  (class object%
    (init parent)
    (super-new)

    (define the-pref-tag 'activity-log:graph-panel)

    ;; The session for which we display the graph
    (define the-session #f)
    (define data-frame #f)

    ;; These are settings for all the graphs
    (define show-avg? #f)           ; display the average line
    (define zoom-to-lap? #f)        ; zoom current lap via a stretch-transform
    (define color-by-zone? #f)      ; color series by zone (if there zones are
    ; defined)
    (define filter-amount 0)        ; amount of filtering to use in graphs

    ;; Map the preferred x-axis (as an index) by sport, this is saved as a
    ;; user preference
    (define x-axis-by-sport (make-hash))

    ;; Map the visible graphs by sport, this is saved as a user preference.
    ;; Each sport allows a different list of graphs to be shown and in a
    ;; different order.
    (define graphs-by-sport (make-hash))

    ;; The axis-choices for the graphs, either default-x-axis-choices or
    ;; swim-x-axis-choices, depending on the session's sport
    (define x-axis-choices '())

    (define graphs '())      ; the list of graphs we are currently  displaying

    ;; Restore the preferences now.
    (let ((pref (get-pref the-pref-tag (lambda () #f))))
      (when (and pref (hash? pref))
        (set! show-avg? (hash-ref pref 'show-avg? #f))
        (set! zoom-to-lap? (hash-ref pref 'zoom-to-lap? #f))
        (set! color-by-zone? (hash-ref pref 'color-by-zone? #f))
        (set! filter-amount (hash-ref pref 'filter-amount 0))
        (set! x-axis-by-sport (hash-copy (hash-ref pref 'x-axis-by-sport (hash))))
        (set! graphs-by-sport (hash-copy (hash-ref pref 'graphs-by-sport (hash))))))

    (define (zoom-to-lap zoom)
      (set! zoom-to-lap? zoom)
      (for-each (lambda (g) (send g zoom-to-lap zoom)) graphs))

    (define (color-by-zone flag)
      (set! color-by-zone? flag)
      (for-each (lambda (g) (send g color-by-zone flag)) graphs))

    (define (show-average-line show)
      (set! show-avg? show)
      (for-each (lambda (g) (send g show-average-line show)) graphs))

    (define (highlight-lap n lap)
      (let* ((start (lap-start-time lap))
             (elapsed (lap-elapsed-time lap))
             ;; use floor because timestamps are at 1 second precision and
             ;; this ensures swim laps are correctly highlighted.
             (end (floor (+ start elapsed))))
        (for ([g graphs])
          (send g highlight-interval start end))))

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

    (define interval-view-panel (new vertical-pane%
                                     [parent panel]
                                     [border 0]
                                     [spacing 1]
                                     [min-width 220]
                                     [stretchable-width #f]
                                     [alignment '(left top)]))

    (define interval-choice #f)
    (let ((p (new horizontal-pane%
                  [parent interval-view-panel]
                  [spacing 10]
                  [stretchable-height #f]
                  [alignment '(left center)])))
      (new message% [parent p] [label "Laps"] [font *header-font*])
      (set! interval-choice (new interval-choice% [tag 'interval-choice-graphs] [parent p] [label ""])))

    (define interval-view (new mini-interval-view%
                               [parent interval-view-panel]
                               [tag 'activity-log:charts-mini-lap-view]
                               [callback (lambda (n lap)
                                           (let ((lap-num (dict-ref lap 'lap-num #f)))
                                             (when lap-num
                                               (highlight-lap (- lap-num 1) lap))))]))

    (send interval-choice set-interval-view interval-view)

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
         [value color-by-zone?]
         [label "Color by Zone"]
         [callback (lambda (b e) (color-by-zone (send b get-value)))])

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
           [label "Select Data Series..."]
           [callback (lambda (b e) (on-select-data-series))]))

    (send filter-amount-choice set-selection filter-amount)

    (define graphs-panel (new vertical-panel%
                              [parent charts-panel]
                              [border 0]
                              [spacing 1]
                              [style '(vscroll)]
                              [alignment '(left top)]))

    (define default-graphs-1 #f)
    (define (default-graphs)
      (unless default-graphs-1
        (set! default-graphs-1 (make-default-graphs graphs-panel
                                                    (lambda (y)
                                                      (when default-graphs-1
                                                        (for ((g (in-list default-graphs-1)))
                                                          (send g draw-marker-at y)))))))
      default-graphs-1)

    (define swim-graphs-1 #f)
    (define (swim-graphs)
      (unless swim-graphs-1
        (set! swim-graphs-1 (make-swim-graphs graphs-panel
                                              (lambda (y)
                                                (when swim-graphs-1
                                                  (for ((g (in-list swim-graphs-1)))
                                                    (send g draw-marker-at y))))
                                              )))
      swim-graphs-1)

    (define sds-dialog #f)

    (define (on-select-data-series)
      (when the-session
        (unless sds-dialog
          (set! sds-dialog (new select-data-series-dialog%)))
        (let ((toplevel (send panel get-top-level-window))
              (visible-tags (hash-ref graphs-by-sport (session-sport the-session) #f))
              (all-graphs (if (is-lap-swimming? data-frame)
                              (swim-graphs) (default-graphs))))
          (cond ((send sds-dialog show-dialog toplevel visible-tags all-graphs)
                 => (lambda (ngraps)
                      (hash-set! graphs-by-sport
                                 (session-sport the-session)
                                 ngraps)
                      (setup-graphs-for-current-session)))))))

    (define/public (save-visual-layout)
      (send interval-view save-visual-layout)
      (send interval-choice save-visual-layout)
      (for-each (lambda (g) (send g save-visual-layout)) (default-graphs))
      (for-each (lambda (g) (send g save-visual-layout)) (swim-graphs))
      (put-pref
       the-pref-tag
       (hash
        'show-avg? show-avg?
        'zoom-to-lap? zoom-to-lap?
        'color-by-zone? color-by-zone?
        'filter-amount filter-amount
        'x-axis-by-sport x-axis-by-sport
        'graphs-by-sport graphs-by-sport)))

    (define (setup-graphs-for-current-session)

      ;; Return the available graphs for SESSION.  For non-lap swimming
      ;; activities, we only use the graphs for which we have data.
      (define (get-graphs-for-session session)
        (define visible (hash-ref graphs-by-sport (session-sport session) #f))

        (define candidates
          (if (is-lap-swimming? data-frame)
              (swim-graphs)
              (for/list ([g (default-graphs)] #:when (send g is-valid-for? data-frame))
                g)))

        ;; NOTE: a #f value for VISIBLE means all graphs are visible.  The '()
        ;; value means that no graphs are visible.
        (if visible
            (for*/list ([tag visible]
                        [g (in-value (findf (lambda (g) (eq? tag (send g get-tag))) candidates))]
                        #:when g)
              g)
            candidates))

      (set! graphs (get-graphs-for-session the-session))
      (send graphs-panel change-children
            (lambda (old) (map (lambda (g) (send g get-panel)) graphs)))
      ;; Attempt to reflow the container so the plot canvas knows its size
      ;; when the plots are about to be inserted -- this does not work,
      ;; unfortunately and the plots are inserted than resized immediately,
      ;; causing an unpleasant flicker.  I suspect this has to do something
      ;; with the 'vscroll option on the graph panel, as the other plots
      ;; (scatter, histogram, etc) don't have this problem.
      (send graphs-panel reflow-container)
      (let* ((sel (send x-axis-choice get-selection))
             (x-axis (cdr (list-ref x-axis-choices sel))))
        (for-each (lambda (g)
                    (send g suspend-flush)
                    (send g set-x-axis x-axis)
                    (send g zoom-to-lap zoom-to-lap?)
                    (send g color-by-zone color-by-zone?)
                    (send g show-average-line show-avg?)
                    (send g set-filter-amount filter-amount)
                    (send g set-data-frame data-frame)
                    (send g resume-flush))
                  graphs)))

    (define/public (set-session session df)
      (set! the-session session)
      (set! data-frame df)

      (let ((lap-swimming? (is-lap-swimming? data-frame)))

        ;; note: some activities might not contain a distance series.
        (set! x-axis-choices
              (for/list ([axis (in-list (if lap-swimming?
                                            swim-x-axis-choices
                                            default-x-axis-choices))]
                         #:when (df-contains? df (send (cdr axis) series-name)))
                axis))

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
          (setup-graphs-for-current-session)))

      (send interval-choice set-session session df))

    ))
