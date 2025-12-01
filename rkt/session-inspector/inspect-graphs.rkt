#lang racket/base
;; inspect-graphs.rkt -- graphs for various data series for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018-2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         data-frame/private/bsearch
         data-frame/slr
         math/statistics
         plot-container
         plot-container/hover-util
         plot/no-gui
         plot/utils
         racket/class
         racket/contract
         racket/dict
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/math
         racket/vector
         map-widget
         colormaps                      ; needed to register the color maps
         "../al-widgets.rkt"
         "../fit-file/activity-util.rkt"
         "../fmt-util.rkt"
         "../session-df/native-series.rkt"
         "../session-df/series-metadata.rkt"
         "../session-df/session-df.rkt"
         "../session-df/xdata-series.rkt"
         "../session-df/shifting.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "../widgets/dragable-split-panel.rkt"
         "../models/aerobic-decoupling.rkt")

(provide graph-panel%)
(provide elevation-graph% grade+calt-graph% grade+alt-graph%
         calt+shaded-grade-graph% alt+shaded-grade-graph%
         graph-view%)

(define *header-font*
  (send the-font-list find-or-create-font 15 'default 'normal 'normal))


;;.............................................................. helpers ....

(define (is-lap-swimming/df? data-frame)
  (df-get-property data-frame 'is-lap-swim?))

(define (is-ow-swimming/df? data-frame)
  (df-get-property data-frame 'is-ow-swim?))


;;.......................................................... chart-view% ....

;; Return the X values in the data-frame DF for START and END timestamps.
;; Returns a list of two values, start-x and end-x
;;
;; NOTE: this function is called a lot of times for each plot, but with the
;; same arguments and it would make a great candidate for memoization,
;; although it is not currently a bottleneck.
(define (ivl-extents df series start end)
  (define max-timestamp (df-ref df (sub1 (df-row-count df)) "timestamp"))
  (df-lookup* df "timestamp" series start (min end max-timestamp)))

;; Same as `ivl-extents` but works on the extracted data series -- this works
;; correctly for swim sessions, since their data series are processes to add
;; extra points for the vertical lines.
(define (ivl-extents/swim data-series start end)
  (define (by-timestamp v) (vector-ref v 2))
  (let ((start-idx (and start (lower-bound data-series start #:key by-timestamp)))
        (end-idx (and end (lower-bound data-series end #:key by-timestamp)))
        (max-idx (vector-length data-series)))
    (unless start-idx (set! start-idx 0))
    (unless (< start-idx max-idx)
      (set! start-idx (sub1 max-idx)))
    (unless (and end-idx (< end-idx max-idx))
      (set! end-idx (sub1 max-idx)))
    (list
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

;; Split DATA-SERIES into segments having the same factor (according to
;; FACTOR-FN).  For example, if the data series contains heart rate and the
;; FACTOR-FN returns zones, the function will split the data into segments
;; that have the same heart rate zone.
;;
;; Note that the intent for this data is to construct line renderers from it,
;; and may not be useful for other purposes (in particular it groups the data
;; points into lists, one for each factor values and "separates" segments
;; within a factor value using +nan.0 values.
(define (split-by-factor data-series factor-fn
                         #:key (key-fn values)
                         #:epsilon (epsilon 0.1))
  (define result (make-hash))
  (define current-factor #f)
  (define seq '())

  (define (put-sequence factor seq)
    (hash-update!
     result factor
     (lambda (old)
       (if (empty? old)
           seq
           ;; NOTE: the sequence of points with the same factor can be
           ;; rendered all at once by inserting an element with a +nan.0 item
           ;; in it, which will effectively put a gap in the lines -- doing
           ;; this results in significantly faster rendering than having
           ;; several line renderers, one for each segment.
           (append old (list (vector +nan.0 +nan.0 0)) seq)))
     '()))

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
              (put-sequence current-factor (reverse seq))
              (set! seq (list mp))
              (set! current-factor (factor-fn (key-fn mp)))
              (if (equal? factor current-factor)
                  (begin
                    (set! seq (cons item seq))
                    (loop (add1 index)))
                  (loop index)))))))
  ;; Add the last one
  (when current-factor
    (put-sequence current-factor (reverse seq)))
  (for/list ([(key value) (in-hash result)])
    (cons key value)))

;; Return a list of plot renderers for data created by `split-by-factor`.
(define (make-plot-renderer-for-splits fdata yr factor-colors)
  (for/list ((item fdata))
    (match-define (cons factor data) item)
    (define color (cdr (assoc factor factor-colors)))
    (make-plot-renderer data yr #:color color #:width 2.0)))


;; Construct a function which converts a grade value into an index for a color
;; map.  COLOR-COUNT represents the number of colors in the color map, while
;; INVERT? indicates that the color map is to be inverted (i.e. as the grade
;; goes up, the color map color index goes down).  This function is intended
;; to be used to assign colors to grade values for a diverging color map, for
;; an example, see the colormaps package at:
;; https://docs.racket-lang.org/colormaps/index.html
;;
;; The returned function will convert a grade value such that grade values
;; between -1, 1 fall in the middle of the color map (if there are an odd
;; number of colors, the middle color index will be the range -1, 1, if there
;; is an even number of colors, the middle two colors are the ranges -1, 0 and
;; 0, 1, from there colors are assigned to each side using a log2 base for the
;; grade (i.e. next colors are grades 1 to 2%, followed by 2% to 4% etc and a
;; similar thing happens for negative grades.
;;
;; For example, if there are 7 colors in a color map:
;;
;; color 0 will represent grades less than -4%
;; color 1 between -4 and -2%
;; color 2 between -2 and -1%
;; color 3 (the middle one) between -1, to 1%
;; color 4 between 1% and 2%
;; color 5 between 2 and 4%
;; color 6 grades greater than 4%

(define (make-grade-color-indexer color-count invert?)
  (define offset (exact-floor (/ color-count 2)))
  (lambda (grade)
    (define index0
      (* (if invert? -1 1)
         (sgn grade)
         (exact-floor
          (let ([absolute-grade (abs grade)])
            (if (< absolute-grade 1.0) 0 (add1 (log absolute-grade 2)))))))
    (define index1
      (if (odd? color-count)
          (+ offset index0)
          (if (> grade 0)
              (+ offset index0)
              (+ offset -1 index0))))
    (inexact->exact (min (sub1 color-count) (max 0 index1)))))

;; Create a list of "spans" of constant color from a data set, this is used in
;; SHADED-AREA plots to determine the span along the X axis of each constant
;; shade (color).
;;
;; DATA is the input data set, as produced by `extract-data`: it is a vector
;; of elements, each element being a (vector x y ...).  DATUM->COLOR is a
;; function which converts the Y value into an integer, representing a color
;; in a color map.  For an example of constructing such a function, see
;; MAKE-GRADE-COLOR-INDEXER.
;;
;; Returns a list of spans, each span is a (list START END COLOR) where START
;; and END correspond X values in the original data set (although new X data
;; points can be created through interpolation).  The spans are continuous,
;; i.e. then END of one span corresponds to the START of the next one.
(define (make-shade-area-spans data datum->color)
  ;; For distance series, which are in KM or Miles, EPSILON is about 1 meter.
  ;; For time series, this will be 1 ms, which is probably too much...
  (define epsilon 1e-3)

  (define (find-change-point x1 y1 x2 y2 c)
    (let loop ([x1 x1]
               [y1 y1]
               [x2 x2]
               [y2 y2])
      (if (< (- x2 x1) epsilon)
          (values x2 y2)
          (let* ([x-len (- x2 x1)]
                 [y-height (- y2 y1)]
                 [mid-x (+ x1 (/ x-len 2.0))]
                 [mid-y (+ y1 (/ y-height 2.0))])
            (if (equal? (datum->color mid-y) c)
                (loop mid-x mid-y x2 y2)
                (loop x1 y1 mid-x mid-y))))))

  (define limit (vector-length data))
  (match-define (vector xstart ystart zstart ...) (vector-ref data 0))
  (define start-color (datum->color ystart))
  (let loop ([index 1]
             [result '()]
             [current-span-start xstart]
             [current-span-end xstart]
             [current-span-y ystart]
             [current-span-color (datum->color ystart)])
    (if (>= index limit)
        (reverse (cons (list current-span-start
                             current-span-end
                             current-span-color)
                       result))
        (let ([p (vector-ref data index)])
          (match-define (vector x y z ...) p)
          (define c (datum->color y))
          (if (equal? c current-span-color)
              ;; Same color, extend the current span
              (loop (add1 index) result current-span-start x y current-span-color)
              (let-values ([(x-change y-change)
                            (find-change-point current-span-end current-span-y x y current-span-color)])
                (loop index
                      (cons (list current-span-start x-change current-span-color)
                            result)
                      x-change
                      x-change
                      y-change
                      (datum->color y-change))))))))

;; Create and return renderer tree from SDATA with area under the plot being
;; colored by data from SDATA2 (See also make-shade-area-spans).  YR is the Y
;; Range for the plot and PS is the plot state for which we are creating the
;; plot (see PS struct).
(define (make-plot-renderer/shade-area sdata sdata2 yr ps)
  (match-define (list color-map datum->color) (ps-shade-area ps))
  (match-define (cons y-min y-max) yr)

  (define limit (vector-length sdata))

  ;; NOTE: don't draw the line color, here, just produce the shaded area (this
  ;; looks nicer, otherwise the lines at the top are interrupted where the
  ;; shaded areas join).
  (define (make-renderer color points)
    (define end (vector-ref (car points) 0))
    (define data (reverse points))
    (define start (vector-ref (car data) 0))
    (lines-interval
     data
     (list (vector start y-min) (vector end y-min))
     #:y-min y-min
     #:y-max y-max
     #:line1-style 'transparent
     #:line2-style 'transparent
     #:alpha 1.0
     #:color (->pen-color color)))

  (define color-spans (make-shade-area-spans sdata2 datum->color))

  (parameterize ([plot-pen-color-map color-map])
    (let loop ([renderers '()]
               [color-spans color-spans]
               [index 1]
               [points (list (vector-ref sdata 0))])
      (cond ((null? color-spans)
             (reverse renderers))
            ((>= index limit)
             (match-let ([(list start end color) (car color-spans)])
               (reverse (cons (make-renderer color points) renderers))))
            (#t
             (define current-point (vector-ref sdata index))
             (define x (vector-ref current-point 0))
             (define end (list-ref (car color-spans) 1))
             (cond ((< x end)
                    ;; Current point is inside the current span
                    (loop renderers
                          color-spans
                          (add1 index)
                          (cons current-point points)))
                   (#t
                    (define y (vector-ref current-point 1))
                    (define previous-point (vector-ref sdata (sub1 index)))
                    (define x0 (vector-ref previous-point 0))
                    (define y0 (vector-ref previous-point 1))
                    (define alpha
                      ;; handle bad data, two points with same X value, see AB#82
                      (if (zero? (- x x0))
                          0.5
                          (/ (- end x0) (- x x0))))
                    (define end-y (+ y0 (* alpha (- y y0))))
                    (define split-point (vector end end-y))
                    (define npoints (cons split-point points))
                    (define color (list-ref (car color-spans) 2))
                    (loop (cons (make-renderer color npoints) renderers)
                          (cdr color-spans)
                          index
                          (list split-point)))))))))

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
                  dual-axis?
                  shade-area
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
      boolean?
      (or/c #f (list/c symbol? (-> any/c exact-nonnegative-integer?)))
      any/c
      any)
  (when (and shade-area dual-axis?)
    (error (format "~a: shade-area not compatible with dual-axis" struct-name)))
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
   ivl
   dual-axis?
   shade-area))

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
   ivl
   ;; when #t, the secondary plot data is not in the same scale as the first
   ;; one, so it needs to be adjusted and another set of labels will need to
   ;; be plotted to the right of the plot for the secondary axis
   dual-axis?
   ;; When not #f, it contains a list of a color map and a color classifier
   ;; function.  Plots will be shaded under the curve according to data from
   ;; Y-AXIS2.  This is not compatible with DUAL-AXIS? (since they use the
   ;; Y-AXIS2 for different purposes)
   shade-area)
  #:guard ps-guard
  #:transparent)

;; The "empty" plot state, defined for convenience
(define empty-ps (ps 0 #f #f #f #f #f #f #f 0 #f #f #f))

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
         guest-transform
         struct-name)
  (-> nonnegative-integer?
      (or/c #f ts-data/c)
      (or/c #f ts-data/c)
      (or/c #f (listof (cons/c any/c ts-data/c)))
      (or/c #f y-range/c)
      (or/c #f (treeof renderer2d?))
      (or/c #f (list/c number? number? (is-a?/c color%)))
      (or/c #f invertible-function?)
      any/c
      any)
  (values
   token                             ; NOTE: don't generate a new token here!
   sdata
   sdata2
   fdata
   y-range
   plot-rt
   hlivl
   guest-transform))

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
   hlivl
   ;; When the plot state indicates that the plot has dual axis, this slot
   ;; contains the guest transform for the secondary axis.
   guest-transform)
  #:guard pd-guard
  #:transparent)

;; An empty plot data structure, for convenience.
(define empty-pd (pd 0 #f #f #f #f #f #f #f))

;; Returns a function which finds the y values corresponding to the X value in
;; the plot data.  This is used to display the Y values on hover.  It the
;; built function will return six values: x, y1, y2 and labels for x y1 and y2
;; using the corresponding series formatter.  y2 and the label for y2 will be
;; #f if there is no secondary axis defined for the plot.
;;
;; NOTE: the y values displayed are the actual values at position X.  There
;; might be a discrepancy between what is displayed and what the plot shows,
;; as there is some filtering and line simplification going on for the plots.
(define (make-y-values-finder plot-state)

  (define df (ps-df plot-state))
  (define x-axis (ps-x-axis plot-state))

  (define xseries (and x-axis (send x-axis series-name)))

  (cond
    ((or (not df)
         (not x-axis)
         (not (df-contains? df xseries)))
     ;; We need at least a data frame and an x series
     (lambda (x) (values x #f #f "" "" "")))
    (#t
     (define y-axis (ps-y-axis plot-state))
     (define y-axis2 (ps-y-axis2 plot-state))

     (define yseries1 (and y-axis (send y-axis series-name)))
     (define yseries2 (and y-axis2 (send y-axis2 series-name)))

     (define sport (df-get-property df 'sport))
     (define sid (df-get-property df 'sid))

     (define xfmt
       (send x-axis value-formatter sport sid #:show-unit-label? #t))
     (define yfmt
       (and y-axis
            (send y-axis value-formatter sport sid #:show-unit-label? #t)))
     (define yfmt2
       (and y-axis2
            (send y-axis2 value-formatter sport sid #:show-unit-label? #t)))

     (define have-yseries1? (and yseries1 (df-contains? df yseries1)))
     (define have-yseries2? (and yseries2 (df-contains? df yseries2)))

     (cond ((and have-yseries1? have-yseries2?)
            (lambda (x)
              (define y1 (or (df-lookup df xseries yseries1 x)
                             (send y-axis missing-value)))
              (define y2 (or (df-lookup df xseries yseries2 x)
                             (send y-axis2 missing-value)))
              ;; NOTE: we might land on a #f value in the original data
              ;; series.  Normally, we should look up a neighbor...
              (values x y1 y2 (xfmt x) (if y1 (yfmt y1) "") (if y2 (yfmt2 y2) ""))))
           (have-yseries1?
            (lambda (x)
              (define y1 (or (df-lookup df xseries yseries1 x)
                             (send y-axis missing-value)))
              (values x y1 #f (xfmt x) (if y1 (yfmt y1) "") "")))
           (have-yseries1?
            (lambda (x)
              (define y2 (or (df-lookup df xseries yseries2 x)
                             (send y-axis2 missing-value)))
              (values x #f y2 (xfmt x) "" (if y2 (yfmt2 y2) ""))))
           (#t
            (lambda (x)
              (values x #f #f (xfmt x) "" "")))))))

;; Find the swim stroke name at the X value on the plot.  Returns #f if the
;; PLOT-STATE is not for a swim activity or the y-axis is not supposed to
;; color plots by swim stroke.  This means the function can be safely called
;; for any data frame + x-axis + y-axis combination and will only return a
;; swim stroke if appropriate.
(define (find-swim-stroke plot-state x sport-charms)
  (define df (ps-df plot-state))
  (define xseries (send (ps-x-axis plot-state) series-name))
  (and
   (send (ps-y-axis plot-state) plot-color-by-swim-stroke?)
   (df-contains? df xseries "swim_stroke")
   (let ((stroke (df-lookup df xseries "swim_stroke" x)))
     (and stroke (send sport-charms get-swim-stroke-name stroke)))))

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
    (cond ((and df (is-lap-swimming/df? df)
                (send y plot-color-by-swim-stroke?)
                (df-contains? df "swim_stroke"))
           (make-plot-renderer/swim-stroke
            sdata y-range (df-select df "swim_stroke")))
          ((and sdata sdata2 (ps-shade-area ps))
           (list
            (make-plot-renderer/shade-area sdata sdata2 y-range ps)
            (make-plot-renderer sdata y-range #:color (send y plot-color))))
          ((and (ps-color? ps) fdata sdata2)
           (list
            (make-plot-renderer-for-splits fdata y-range (send y factor-colors))
            (make-plot-renderer sdata2 y-range
                                #:color (send y2 plot-color)
                                #:width 2
                                #:alpha 0.9
                                #:label (send y2 plot-label))))
          ((and (ps-color? ps) fdata)
           (make-plot-renderer-for-splits fdata y-range (send y factor-colors)))
          ((and sdata sdata2)
           (list
            (make-plot-renderer sdata y-range
                                #:color (send y plot-color)
                                #:width 2
                                #:alpha 0.9
                                #:label (send y plot-label))
            (make-plot-renderer sdata2 y-range
                                #:color (send y2 plot-color)
                                #:width 2
                                #:alpha 0.9
                                #:label (send y2 plot-label))))
          (sdata
           (make-plot-renderer sdata y-range
                               #:color (send y plot-color)))
          (sdata2
           (make-plot-renderer sdata2 y-range
                               #:color (send y2 plot-color)))
          (#t #f))))

;; Produce a highlight interval data structure from the PD and PS structures.
;; We assume the PD structure is up-to date w.r.t PD structure.
(define/contract (plot-highlight-interval pd ps)
  (-> pd? ps? (or/c #f (list/c number? number? any/c)))
  (let ((df (ps-df ps))
        (y (ps-y-axis ps))
        (ivl (ps-ivl ps))
        (series (send (ps-x-axis ps) series-name)))
    (if (and (cons? ivl) df y)
        (let ((c (send y plot-color)))
          (define result
            (append
             (if (is-lap-swimming/df? df)
                 (ivl-extents/swim (pd-sdata pd) (car ivl) (cdr ivl))
                 (ivl-extents df series (car ivl) (cdr ivl)))
             (list (make-object color% (send c red) (send c green) (send c blue) 0.2))))
          (if (and (number? (car result)) (number? (cadr result)))
              result
              #f))
        #f)))

;; Construct an invertible-function? which transforms a value such that the
;; GUEST-MIN .. GUEST-MAX range is transformed to BASE-MIN .. BASE-MAX ranges.
;; Ideally data should be transformed only between the ranges, but the actual
;; functions would work outside this domain as well.  There are some
;; restriction on the base and guest ranges, see contract below
(define/contract (make-guest-transform base-min base-max guest-min guest-max)

  ;; This contract checks that the parameters are correct as needed by this
  ;; application: they must be real numbers and not NaN and infinites, and
  ;; base-max > base-min and guest-max > guest-min (this is implicitly
  ;; checked, as there is no `abs` call in the #:pre/name section).  Also, we
  ;; check that the range is greater than a small number (1e-4) -- technically
  ;; we would only need to check for zero, as that makes it impossible to
  ;; generate the transform, but very small ranges are also suspicious for the
  ;; types of data we deal with in this application
  (->i ([base-min (and/c real? (not/c infinite?) (not/c nan?))]
        [base-max (and/c real? (not/c infinite?) (not/c nan?))]
        [guest-min (and/c real? (not/c infinite?) (not/c nan?))]
        [guest-max (and/c real? (not/c infinite?) (not/c nan?))])
       ()
       #:pre/name (base-min base-max guest-min guest-max)
       "empty base or guest range"      ; see note above
       (and (> (- base-max base-min) 1e-4)
            (> (- guest-max guest-min) 1e-4))
       (result invertible-function?))

  (define base-range (- base-max base-min))
  (define guest-range (- guest-max guest-min))
  (invertible-function
   (lambda (v)                          ; Transform from base to guest
     (let ([p (/ (- v base-min) base-range)])
       (+ guest-min (* p guest-range))))
   (lambda (v)                          ; Transform from guest to base
     (let ([p (/ (- v guest-min) guest-range)])
       (+ base-min (* p base-range))))))

(define (guest->base guest-data transform)
  (let ([tr (invertible-function-g transform)])
    (for/vector ([item guest-data])
      (define c (vector-copy item))
      (vector-set! c 1 (tr (vector-ref item 1)))
      c)))


;; Produce a new PD structure given an old PD and PS structure and a new PS
;; structure.  Returns two values: the updated PD structure and a flag
;; indicating if the renderer tree has changed.  This function determines what
;; has changed between OLD-PS and NEW-PS and re-computes only what is needed,
;; the remaining data is taken from OLD-PD.
(define/contract (update-plot-data old-pd old-ps new-ps)
  (-> pd? ps? ps? (values pd? boolean?))
  (unless (equal? (ps-token old-ps) (pd-token old-pd))
    (error (format "update-plot-data: token mismatch PS: ~a, PD: ~a"
                   (ps-token old-ps) (pd-token old-pd))))
  (cond
    ((or (eq? old-ps new-ps)
         (equal? (ps-token old-ps) (ps-token new-ps)))
     (values old-pd #f))
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
                          (equal? (ps-filter old-ps) (ps-filter new-ps))
                          (equal? (ps-dual-axis? old-ps) (ps-dual-axis? new-ps)))))))

     ;; need new fdata if we update sdata or color? has changed or there is no
     ;; fdata in old-pd.  Also check if the series actually can produce
     ;; factored data...
     (define need-fdata?
       (and (ps-df new-ps)
            (and (ps-color? new-ps) (not (ps-shade-area new-ps)))
            (let ((df (ps-df new-ps))
                  (y (ps-y-axis new-ps)))
              (let* ((sport (df-get-property df 'sport))
                     (sid (df-get-property df 'session-id)))
                (send y factor-fn sport sid)))
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
                  (lap-swimming? (is-lap-swimming/df? df)))
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
                  (lap-swimming? (is-lap-swimming/df? df)))
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
       (cond (need-fdata?
              (let ((df (ps-df new-ps))
                    (y (ps-y-axis new-ps)))
                (let* ((sport (df-get-property df 'sport))
                       (sid (df-get-property df 'session-id))
                       (factor-fn (send y factor-fn sport sid))
                       (epsilon (expt 10 (- (send y fractional-digits)))))
                  (if (and factor-fn sdata)
                      (split-by-factor sdata factor-fn
                                       #:key (lambda (v) (vector-ref v 1))
                                       #:epsilon epsilon)
                      #f))))
             ;; Only inherit previous FDATA if the data frame has not
             ;; changed...
             ((eq? (ps-df new-ps) (ps-df old-ps))
              (pd-fdata old-pd))
             (else
              #f)))

     ;; Calculate new Y-RANGE, if needed, or reuse the one from OLD-PD.
     (define-values (y-range guest-transform)
       (if (or need-sdata? need-sdata2?)
           (let* ((y (ps-y-axis new-ps))
                  (y2 (ps-y-axis2 new-ps))
                  (st1 (if sdata (ds-stats sdata) #f))
                  (st2 (if sdata2 (ds-stats sdata2) #f))
                  (yr1 (if st1 (get-plot-y-range st1 y) #f))
                  (yr2 (if st2 (get-plot-y-range st2 y2) #f)))
             (cond
               ((and (ps-shade-area new-ps) yr1)
                (values yr1 #f))
               ((and (ps-dual-axis? new-ps) yr1 yr2)
                (values yr1 (make-guest-transform (car yr1) (cdr yr1) (car yr2) (cdr yr2))))
               ((and yr1 yr2)
                (values (combine-y-range yr1 yr2) #f))
               (yr1 (values yr1 #f))
               (yr2 (values yr2 #f))
               (#t #f)))
           (values (pd-y-range old-pd) (pd-guest-transform old-pd))))

     (when (and need-sdata2? guest-transform)
       (set! sdata2 (guest->base sdata2 guest-transform)))

     ;; Temporary PD structure, used to pass them on to the renderer tree
     ;; functions.
     (define tmp-pd (pd (ps-token new-ps) sdata sdata2 fdata y-range #f #f guest-transform))

     ;; Calculate a new plot renderer tree if needed, or reuse the one from
     ;; OLD-PD
     (define-values (plot-rt new-tree?)
       (if (and (ps-df new-ps) (ps-x-axis new-ps) (ps-y-axis new-ps))
           (if (or need-sdata? need-sdata2? need-fdata?
                   (not (equal? (ps-color? old-ps) (ps-color? new-ps))))
               (values (plot-data-renderer-tree tmp-pd new-ps) #t)
               (values (pd-plot-rt old-pd) #f))
           (values #f (if (pd-plot-rt old-pd) #t #f))))

     ;; Calculate a new highlight interval renderer tree if needed, or reuse
     ;; the one from OLD-PD
     (define-values (hlivl new-hlivl?)
       (if (and (ps-ivl new-ps) (ps-df new-ps) (ps-x-axis new-ps) (ps-y-axis new-ps))
           (if (or need-sdata? need-sdata2? need-fdata?
                   (not (equal? (ps-ivl old-ps) (ps-ivl new-ps))))
               (values (plot-highlight-interval tmp-pd new-ps) #t)
               (values (pd-hlivl old-pd) #f))
           (values #f (if (pd-hlivl old-pd) #t #f))))

     ;; Put the renderer tree in the structure and return the result.
     (values (struct-copy pd tmp-pd (plot-rt plot-rt) (hlivl hlivl))
             (or new-tree?
                 (not (equal? (ps-zoom? new-ps) (ps-zoom? old-ps)))
                 (not (equal? (ps-avg? new-ps) (ps-avg? old-ps)))
                 (and (ps-zoom? new-ps) new-hlivl?))))))

;; Interface of the graphs objects as used by the graph-panel%, contains all
;; the methods that the graph-panel% will invoke on graph objects in order to
;; manage them.
(define graph-view-interface<%>
  (interface
      ()
    get-headline
    get-preferences-tag
    get-graph-canvas
    begin-edit-sequence
    end-edit-sequence
    draw-marker-at
    is-valid-for?
    set-data-frame
    zoom-to-lap
    color-by-zone
    set-filter-amount
    show-average-line
    set-x-axis
    highlight-interval
    on-interactive-export-image))

;; Base class for all graphs.  Defines all behavior and data management for
;; graphs.  Derived classes just configure functionality in this one, such as
;; setting the axes or customizing small amounts of behavior.
(define graph-view%
  (class* object% (graph-view-interface<%>)
    (init parent)
    (init-field
     primary-y-axis
     sport-charms
     [headline #f]           ; see get-headline
     [preferences-tag #f]    ; see get-preferences-tag
     [min-height 10]
     [hover-callback (lambda (x) (void))]
     [secondary-y-axis #f]
     [dual-axis? #f]
     [color-map #f]
     [color-index-fn #f]
     [style '()])
    (super-new)

    (define show-stop-points?
      (get-pref 'activity-log:debug:show-stop-points? (lambda () #f)))
    (define stop-point-renderers '())

    ;; NOTE: use update-state to set this value.
    (define plot-state (struct-copy ps empty-ps
                                    [y-axis primary-y-axis]
                                    [y-axis2 secondary-y-axis]
                                    [dual-axis? dual-axis?]
                                    [shade-area (if (and color-map color-index-fn)
                                                    (list color-map color-index-fn)
                                                    #f)]))
    (define previous-plot-state plot-state)
    (define plot-data (struct-copy pd empty-pd [token (ps-token plot-state)]))

    ;; Function to find y values in the plot-data (see `make-y-values-finder`)
    (define find-y-values (make-y-values-finder plot-state))

    ;; The name of the file used by 'on-interactive-export-image'. This is
    ;; remembered between subsequent exports, but reset when one of the axis
    ;; changes.
    (define export-file-name #f)

    (define/public (get-headline)       ; TODO: cache it after we get rid of set-y-axis
      (or headline
          (let ([primary (ps-y-axis plot-state)])
            (send primary axis-label))))

    (define/public (get-preferences-tag) ; TODO: cache it after we get rid of set-y-axis
      (or preferences-tag
          (let ([primary (ps-y-axis plot-state)]
                [secondary (ps-y-axis2 plot-state)])
            (define tag (cond ((and primary secondary)
                               (string-append
                                (send primary series-name)
                                "+"
                                (send secondary series-name)))
                              (primary
                               (send primary series-name))
                              (secondary
                               (send secondary series-name))
                              (#t
                               "unknown")))
            (string->symbol (string-append "al2-graphs:" tag)))))

    (define/private (put-plot output-fn pd ps)
      ;; (-> (-> (treeof renderer2d?) any/c) pd? ps? any/c)

      (define x-axis (ps-x-axis ps))
      (define y-axis (ps-y-axis ps))
      (define y-axis2 (ps-y-axis2 ps))
      ;; NOTE: look for the guest-transform if we want to know if this is a
      ;; dual axis plot -- just because (ps-dual-axis? ps) is #t, it does not
      ;; necessarily mean we actually have a dual axis plot -- the data for
      ;; the secondary series might be missing from the data frame.
      (define dual-axis? (pd-guest-transform pd))
      (define df (ps-df ps))

      (define (full-render-tree)
        (let ((render-tree (list (pd-plot-rt pd))))
          (set! render-tree (cons (tick-grid) render-tree))
          (when (ps-avg? ps)
            (let ((avg (get-redline-renderer)))
              (when avg (set! render-tree (cons avg render-tree)))))
          (append stop-point-renderers (reverse render-tree))))

      (define (get-x-transform)
        (let ((ivl (ps-ivl ps))
              (series (send x-axis series-name)))
          (if (and (cons? ivl) (ps-zoom? ps))
              (match-let ([(list start end)
                           (if (is-lap-swimming/df? df)
                               (ivl-extents/swim (pd-sdata pd) (car ivl) (cdr ivl))
                               (ivl-extents df series (car ivl) (cdr ivl)))])
                (if (and (number? start) (number? end))
                    (stretch-transform start end 30)
                    id-transform))
              id-transform)))

      (define (get-x-axis-ticks)
        (let ((ticks (send x-axis plot-ticks))
              (ivl (ps-ivl ps))
              (series (send x-axis series-name)))
          (if (and (cons? ivl) (ps-zoom? ps))
              (match-let ([(list start end)
                           (if (is-lap-swimming/df? df)
                               (ivl-extents/swim (pd-sdata pd) (car ivl) (cdr ivl))
                               (ivl-extents df series (car ivl) (cdr ivl)))])
                (if (and (number? start) (number? end))
                    (ticks-add ticks (list start end))
                    ticks))
              ticks)))

      (parameterize ([plot-x-transform (get-x-transform)]
                     [plot-x-ticks (get-x-axis-ticks)]
                     [plot-x-label #f]
                     [plot-y-ticks (send y-axis plot-ticks)]
                     [plot-y-label (send y-axis axis-label)]
                     [plot-y-far-label
                      (if dual-axis? (send y-axis2 axis-label) #f)]
                     [plot-y-far-ticks
                      (if dual-axis?
                          (ticks-scale (send y-axis2 plot-ticks)
                                       (pd-guest-transform pd))
                          (send y-axis plot-ticks))])
        (output-fn (full-render-tree))))

    (define/private (put-plot/canvas canvas pd ps)
      (put-plot (lambda (rt) (plot-to-canvas rt canvas)) pd ps))

    (define/private (put-plot/file file-name width height pd ps)
      ;; (-> path-string? exact-positive-integer? exact-positive-integer? pd? ps? any/c)
      (put-plot
       (lambda (renderer-tree)
         (plot-file renderer-tree file-name #:width width #:height height))
       pd ps))

    (define graph-canvas
      (new plot-container% [parent parent] [columns 1]
           [min-height min-height]
           [style '()]))

    (define/public (get-graph-canvas) graph-canvas)

    (define edit-sequence-count 0)

    (define/public (begin-edit-sequence)
      (when (zero? edit-sequence-count)
        (send graph-canvas suspend-flush))
      (set! edit-sequence-count (add1 edit-sequence-count)))

    (define/public (end-edit-sequence)
      (when (zero? edit-sequence-count)
        (error "graph-view%/resume-flush: invalid call"))
      (set! edit-sequence-count (sub1 edit-sequence-count))
      (when (zero? edit-sequence-count)
        (send graph-canvas resume-flush)
        (unless (= (ps-token plot-state) (ps-token previous-plot-state))
          (refresh-plot))))

    (define/private (in-edit-sequence?)
      (> edit-sequence-count 0))

    (define the-plot-snip #f)

    (define/public (draw-marker-at x)
      (when the-plot-snip
        (let ((rt '()))
          (define (add-renderer r) (set! rt (cons r rt)))
          ;; Add the highlight overlay back in...
          (when (pd-hlivl plot-data)
            (match-define (list xmin xmax color) (pd-hlivl plot-data))
            (add-renderer (hover-vrange xmin xmax color)))
          (when x
            (define-values (_ y1 y2 xlab ylab1 ylab2) (find-y-values x))
            (define y-min (car (pd-y-range plot-data)))
            (cond ((and y1 y2)
                   ;; NOTE: if we consider y2, it needs to be transformed when
                   ;; dual-axis? is #t
                   (let ((label (string-append ylab1 " / " ylab2 " @ " xlab)))
                     (add-renderer (hover-label x y-min label))))
                  (y1
                   (let ((label (string-append ylab1 " @ " xlab))
                         (swim-stroke (find-swim-stroke plot-state x sport-charms)))
                     (add-renderer (hover-label x y-min label swim-stroke))))
                  (y2
                   (let ((label (string-append ylab2 " @ " xlab)))
                     (add-renderer (hover-label x y-min label)))))
            (add-renderer (hover-vrule x)))
          (set-overlay-renderers the-plot-snip rt))))

    (define (plot-hover-callback snip event x y)
      (if (good-hover? snip x y event)
          (hover-callback x)
          (hover-callback #f)))

    (define/private (refresh-plot)
      (let ((pstate plot-state)
            (ppstate previous-plot-state)
            (pdata plot-data)
            (pplot-snip the-plot-snip))
        ;; will be set back below, but we don't want `draw-marker-at` to
        ;; attempt to use an invalid series.
        (set! the-plot-snip #f)

        (queue-task
         "graph-view%/refresh-plot"
         (lambda ()
           (define-values (npdata new-render-tree?)
             (update-plot-data pdata ppstate pstate))
           (queue-callback
            (lambda ()
              (when (= (pd-token npdata) (ps-token plot-state))
                (if (pd-plot-rt npdata)
                    (if (or new-render-tree? (not pplot-snip))
                        (begin
                          (set! the-plot-snip (put-plot/canvas graph-canvas npdata pstate))
                          (set-mouse-event-callback the-plot-snip plot-hover-callback))
                        (set! the-plot-snip pplot-snip))
                    (begin
                      (set! the-plot-snip #f)
                      (send graph-canvas clear-all)
                      (send graph-canvas set-background-message "No data for plot...")))

                (when the-plot-snip
                  (if (pd-hlivl npdata)
                      (match-let (((list xmin xmax color) (pd-hlivl npdata)))
                        (set-overlay-renderers the-plot-snip (list (hover-vrange xmin xmax color))))
                      (set-overlay-renderers the-plot-snip #f))
                  ;; a refresh here is unnecessary, since setting the overlay
                  ;; renderers above will trigger a refresh anyway
                  #;(send graph-canvas refresh))

                (set! previous-plot-state pstate)
                (set! plot-state pstate)
                (set! plot-data npdata)
                (set! find-y-values (make-y-values-finder plot-state)))))))))

    (define/private (update-state new-state)
      (set! plot-state new-state)

      (unless (in-edit-sequence?)

        (when show-stop-points?
          ;; NOTE: this code runs for every plot, even though it produces the
          ;; same renderers, so it is inefficient.  Since this is a debug
          ;; feature only, I will leave with the performance degradation.
          (let ([df (ps-df plot-state)]
                [xaxis (ps-x-axis plot-state)])
            (when (and df xaxis)
              (let* ([xseries (send xaxis series-name)]
                     [sp (df-get-property df 'stop-points)]
                     [tp (df-get-property df 'teleport-points)])
                (set! stop-point-renderers
                      (for/list ([p (in-list sp)])
                        (let ([x (df-lookup df "timestamp" xseries p)])
                          ;; Stop points are blue, teleport points are red
                          (vrule x #:style 'short-dash #:color (if (member p tp) "red" "blue")))))))))

        (refresh-plot)))

    (define/public (get-redline-renderer)
      ;; NOTE: we might not have statistics for a series, if a graph is
      ;; selected but there is no data for that graph...
      (define df (ps-df plot-state))
      (define sport (df-get-property df 'sport #f))
      (define primary-renderer
        (let ([metadata (ps-y-axis plot-state)])
          (and metadata
               (let* ([series (send metadata series-name)]
                      [st (df-statistics df series)]
                      [formatter (send metadata value-formatter sport)])
                 (and st
                      (hrule (statistics-mean st)
                             #:label (format "Avg ~a ~a"
                                             (or (send metadata plot-label) "")
                                             (formatter (statistics-mean st)))))))))
      (define secondary-renderer
        (let ([metadata (ps-y-axis2 plot-state)])
          (and metadata
               (let* ([series (send metadata series-name)]
                      [st (df-statistics df series)]
                      [formatter (send metadata value-formatter sport)])
                 (and st
                      (hrule (statistics-mean st)
                             #:label (format "Avg ~a ~a"
                                             (or (send metadata plot-label) "")
                                             (formatter (statistics-mean st)))))))))

      (filter values (list primary-renderer secondary-renderer)))

    ;; Return #t if this graph can display some data for DATA-FRAME (e.g. a
    ;; cadence graph is only valid if there is cadence series in the data
    ;; frame).  This can be overridden, but by default we look for the series
    ;; that are specified by the primary and secondary y axis.
    (define/public (is-valid-for? df)
      (define primary (ps-y-axis plot-state))
      (define secondary (ps-y-axis2 plot-state))
      (or (and primary (df-contains? df (send primary series-name)))
          (and secondary (df-contains? df (send secondary series-name)))))

    (define/public (set-data-frame df)
      (begin-edit-sequence)
      (set! plot-state (struct-copy ps plot-state [df df] [ivl #f]))
      (set! export-file-name #f)
      (set! stop-point-renderers '())   ; clear them, will be set in on-y-axis-selected
      ;; When a new data frame is set, remove the old plot immediately, as it
      ;; is not relevant anymore.
      (set! the-plot-snip #f)
      (send graph-canvas clear-all)
      (send graph-canvas set-background-message "Working...")
      (end-edit-sequence))

    (define/public (zoom-to-lap zoom)
      (unless (equal? zoom (ps-zoom? plot-state))
        (update-state (struct-copy ps plot-state [zoom? zoom]))))

    (define/public (color-by-zone flag)
      (unless (equal? flag (ps-color? plot-state))
        (update-state (struct-copy ps plot-state [color? flag]))))

    (define/public (set-filter-amount a)
      (unless (equal? a (ps-filter plot-state))
        (update-state (struct-copy ps plot-state [filter a]))))

    (define/public (show-average-line show)
      (unless (equal? show (ps-avg? plot-state))
        (update-state (struct-copy ps plot-state [avg? show]))))

    (define/public (set-x-axis new-x-axis)
      (unless (equal? new-x-axis (ps-x-axis plot-state))
        (update-state (struct-copy ps plot-state [x-axis new-x-axis]))))

    (define/public (highlight-interval start-timestamp end-timestamp)
      (define new-ivl (if (and start-timestamp end-timestamp)
                          (cons start-timestamp end-timestamp)
                          #f))
      (unless (equal? new-ivl (ps-ivl plot-state))
        (define new-state (struct-copy ps plot-state [ivl new-ivl]))
        (update-state new-state)))

    (define/public (get-data-frame) (ps-df plot-state))

    (define/private (export-image-to-file file-name)
      (put-plot/file file-name 800 300 plot-data plot-state))

    ;; Return a suitable file name for use by 'on-interactive-export-image'.
    ;; If 'export-file-name' is set, we use that, otherwise we compose a file
    ;; name from the session id and axis names of the plot.
    (define/private (get-default-export-file-name)
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

    ))

;; Displays a map as a graph, with the track and selected track.  Useful, if
;; the user wants to have a map next to the graphs to locate a data point
;; geographically.
(define map-graph%
  (class* object% (graph-view-interface<%>)
    (init parent)
    (init-field
     [style '()]
     [hover-callback (lambda (x) (void))]
     [min-height 10])
    (super-new)

    (define the-session #f)
    (define the-x-axis #f)
    (define zoom-to-lap? #f)
    (define main-track-pen
      (send the-pen-list find-or-create-pen
            (make-object color% 226 34 62) 3 'solid 'round 'round))
    (define lap-track-pen
      (send the-pen-list find-or-create-pen
            (make-object color% 24 60 165)
            7
            'solid 'round 'round))
    (define have-selected-lap? #f)

    (define map-container
      (new plot-container%
           [parent parent]
           [columns 1]
           [min-height min-height]
           [style '()]))

    (define the-map
      (new
       (class map-snip%
         (init)
         (super-new)
         ;; Hack to intercept a resize operation on the snip and
         ;; re-center/re-size the track or selected lap when this happens.
         ;; This is needed, since the snip does not have a size when
         ;; `set-data-frame` is called.
         (define/override (resize w h)
           (super resize w h)
           (define group (if (and zoom-to-lap? have-selected-lap?) 'lap 'main))
           (send this resize-to-fit group)))))
    (define the-cll
      (current-location-layer 'current-location #:track-current-location? #t))
    (send the-map add-layer the-cll)

    (send map-container set-snip the-map)

    (define/public (get-headline)
      "Map (GPS Track)")

    (define/public (get-preferences-tag)
      'al2-graphs:map-graph)

    (define/public (get-graph-canvas)
      map-container)

    (define/public (begin-edit-sequence)
      (void))

    (define/public (end-edit-sequence)
      (void))

    (define/public (draw-marker-at x)
      (when (and the-x-axis the-session)
        (define series (send the-x-axis series-name))
        (when (df-contains? the-session series)
          (define l (and x (df-lookup the-session series '("lat" "lon") x)))
          (if (and l
                   (rational? (vector-ref l 0))
                   (rational? (vector-ref l 1)))
              (send the-cll current-location l)
              (send the-cll current-location #f)))))

    (define/public (is-valid-for? df)
      (df-contains? df "lat" "lon"))

    (define/public (set-data-frame df)
      (set! the-session df)
      ;; TODO: instead of adding the entire track as a single line, we should
      ;; add individual segments based on "teleport" locations, as it is done
      ;; in inspect-map.rkt...
      (define waypoints (df-select* the-session "lat" "lon" #:filter valid-only))
      (set! have-selected-lap? #f)
      (send* the-map
        (begin-edit-sequence)
        (add-layer (line-layer 'main waypoints #:pen main-track-pen))
        (resize-to-fit)
        (end-edit-sequence)))

    (define/public (zoom-to-lap flag)
      (set! zoom-to-lap? flag)
      (define layer (if (and zoom-to-lap? have-selected-lap?) 'lap 'main))
      (send the-map resize-to-fit layer))

    (define/public (color-by-zone flag) ; not applicable
      (void))

    (define/public (set-filter-amount a) ; not applicable
      (void))

    (define/public (show-average-line flag) ; not applicable
      (void))

    (define/public (set-x-axis axis)
      (set! the-x-axis axis))

    (define/public (highlight-interval start end)
      (if (and start end)
          (match-let ([(list start-idx end-idx)
                       (df-index-of* the-session "timestamp" start end)])
            (define waypoints
              (df-select* the-session "lat" "lon"
                          #:filter valid-only
                          #:start start-idx
                          #:stop (add1 end-idx)))
            (set! have-selected-lap? #t)
            (send* the-map
              (begin-edit-sequence)
              (add-layer (line-layer 'lap waypoints
                                     #:pen lap-track-pen
                                     #:zorder 0.1))
              (resize-to-fit (if zoom-to-lap? 'lap 'main))
              (end-edit-sequence)))
          (begin
            (set! have-selected-lap? #f)
            (send* the-map
              (begin-edit-sequence)
              (remove-layer 'lap)
              (resize-to-fit)
              (end-edit-sequence)))))

    (define/public (on-interactive-export-image)
      ;; TODO: would be nice to implement this, once supported by the map widget
      (void))

    ))



;;........................................................ series graphs ....

(define speed-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-speed])))

(define pace-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-pace])))

(define speed-zone-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-speed-zone])))

(define grade-adjusted-pace-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-gap])))

(define elevation-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-elevation])
    ;; Don't display an average line for the elevation
    (define/override (get-redline-renderer) #f)))

(define corrected-elevation-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-corrected-elevation])
    ;; Don't display an average line for the elevation
    (define/override (get-redline-renderer) #f)))

(define grade-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-grade])))

(define grade+calt-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-grade]
               [secondary-y-axis axis-corrected-elevation]
               [dual-axis? #t]
               [headline "Grade + Elevation (corrected)"])
    (define/override (get-redline-renderer) #f)))

(define grade+alt-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-grade]
               [secondary-y-axis axis-elevation]
               [dual-axis? #t]
               [headline "Grade + Elevation (original)"])
    (define/override (get-redline-renderer) #f)))

(define grade-shading-color-map 'cb-rdbu-11)
(define grade-shading-color-indexer
  (make-grade-color-indexer (color-map-size grade-shading-color-map) #t))

(define calt+shaded-grade-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-corrected-elevation]
               [secondary-y-axis axis-grade]
               [color-map grade-shading-color-map]
               [color-index-fn grade-shading-color-indexer]
               [preferences-tag 'al2-graphs:calt+shaded-grade]
               [headline "Elevation (corrected) Color by Grade"])
    (define/override (get-redline-renderer) #f)))

(define alt+shaded-grade-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-elevation]
               [secondary-y-axis axis-grade]
               [color-map grade-shading-color-map]
               [color-index-fn grade-shading-color-indexer]
               [preferences-tag 'al2-graphs:alt+shaded-grade]
               [headline "Elevation (original) Color by Grade"])
    (define/override (get-redline-renderer) #f)))

(define heart-rate-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-hr-bpm])))

(define heart-rate-zones-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-hr-zone])))

(define heart-rate-pct-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-hr-pct])))

(define temperature-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-temperature])))

(define cadence-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-cadence])))

(define stride-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-stride])))

(define vertical-oscillation-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-vertical-oscillation])))

(define vertical-ratio-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-vratio])))

(define ground-contact-time-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-stance-time])))

(define ground-contact-percent-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-stance-time-percent])))

(define wbal-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-wbal])))

(define power-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-power])))

(define power+wbal-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-power]
               [secondary-y-axis axis-wbal]
               [dual-axis? #t]
               [headline "Power + W'Bal"])))

(define power-zone-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-power-zone])))

(define lrbal-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-right-balance])))

(define teff-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-torque-effectiveness]
               [secondary-y-axis axis-right-torque-effectiveness])))

(define psmth-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-pedal-smoothness]
               [secondary-y-axis axis-right-pedal-smoothness])))

(define cpsmth-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-combined-pedal-smoothness])))

(define pco-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-platform-centre-offset]
               [secondary-y-axis axis-right-platform-centre-offset])))

(define pp-start-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-power-phase-start]
               [secondary-y-axis axis-right-power-phase-start])))

(define pp-end-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-power-phase-end]
               [secondary-y-axis axis-right-power-phase-end])))

(define pp-angle-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-power-phase-angle]
               [secondary-y-axis axis-right-power-phase-angle])))

(define ppp-start-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-peak-power-phase-start]
               [secondary-y-axis axis-right-peak-power-phase-start])))

(define ppp-end-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-peak-power-phase-end]
               [secondary-y-axis axis-right-peak-power-phase-end])))

(define ppp-angle-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-left-peak-power-phase-angle]
               [secondary-y-axis axis-right-peak-power-phase-angle])))

(define gear-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-front-gear]
               [secondary-y-axis axis-rear-gear]
               [headline "Gears"])))

(define gear-index-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-front-gear-index]
               [secondary-y-axis axis-rear-gear-index]
               [headline "Gear Index"])))

(define gear-ratio-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-gear-ratio])))

(define pwr-hr-reserve-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-pwr-reserve]
               [secondary-y-axis axis-hr-reserve]
               [dual-axis? #t]
               [headline "Power/HR Reserve"])

    (define/override (get-redline-renderer) #f)))

(define spd-hr-reserve-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-spd-reserve]
               [secondary-y-axis axis-hr-reserve]
               [dual-axis? #t]
               [headline "Speed/HR Reserve"])

    (define/override (get-redline-renderer) #f)))

(define adecl-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-adecl]
               [headline "Aerobic Decoupling"])

    (define session #f)
    (define x-series #f)

    (define/override (set-data-frame df)
      (set! session df)
      (super set-data-frame df))

    (define/override (set-x-axis new-x-axis)
      (set! x-series (send new-x-axis series-name))
      (super set-x-axis new-x-axis))

    (define/override (get-redline-renderer)
      (if (and session x-series)
          (let-values
              ([(xs ys)
                (for/fold ([xs '()]
                           [ys '()])
                          ([(ts adecl) (in-data-frame session x-series "adecl")])
                  (if (and (rational? ts) (rational? adecl))
                      (values (cons ts xs) (cons adecl ys))
                      (values xs ys)))])
            (match-define (slr alpha beta r) (simple-linear-regression xs ys))
            (define fn (lambda (x) (+ alpha (* beta x))))
            ;; NOTE: xs and ys are reversed!
            (define drop
              (let* ([x-max (first xs)]
                     [x-min (last xs)]
                     [y-max (fn x-max)]
                     [y-min (fn x-min)])
                (- y-max y-min)))
            (function fn #:label
                      (if (> drop 0)
                          (format "Raise: ~a (% points)" (~r drop #:precision 1))
                          (format "Drop: ~a (% points)" (~r (- drop) #:precision 1)))))
          #f))
    ))


;;..................................................... swim-pace-graph% ....

(define swim-pace-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-swim-pace]
               [preferences-tag 'activity-log:swim-pace-graph]
               [headline "Swim Pace"])

    (inherit get-data-frame)

    (define avg-speed #f)

    (define (get-avg-speed)
      (unless avg-speed
        (let ((st (df-statistics (get-data-frame) "spd")))
          (set! avg-speed (and st (statistics-mean st)))))
      avg-speed)

    (define/override (get-redline-renderer)
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
    (super-new [primary-y-axis axis-swim-swolf]
               [preferences-tag 'activity-log:swim-swolf-graph]
               [headline "SWOLF"])

    (inherit get-data-frame)

    (define avg-swolf #f)

    (define (get-avg-swolf)
      (unless avg-swolf
        (let ((st (df-statistics (get-data-frame) "swolf")))
          (set! avg-swolf (and st (statistics-mean st)))))
      avg-swolf)

    (define/override (get-redline-renderer)
      (let ((avg (get-avg-swolf)))
        (if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (string-append "Avg " (~r avg #:precision 1)))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-swolf #f)
      (super set-data-frame data-frame))

    ))


;;............................................. swim-stroke-count-graph% ....

(define swim-stroke-count-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-swim-stroke-count]
               [preferences-tag 'activity-log:swim-stroke-count-graph]
               [headline "Stroke Count"])

    (inherit get-data-frame)
    (define avg-stroke-count #f)

    (define (get-avg-stroke-count)
      (unless avg-stroke-count
        (let ((st (df-statistics (get-data-frame) "strokes")))
          (set! avg-stroke-count (and st (statistics-mean st)))))
      avg-stroke-count)

    (define/override (get-redline-renderer)
      (let ((avg (get-avg-stroke-count)))
        (if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (string-append "Avg " (~r avg #:precision 1)))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-stroke-count #f)
      (super set-data-frame data-frame))

    ))


;;.................................................. swim-cadence-graph% ....

(define swim-cadence-graph%
  (class graph-view%
    (super-new [primary-y-axis axis-swim-avg-cadence]
               [preferences-tag 'activity-log:swim-cadence-graph]
               [headline "Swim Cadence"])

    (inherit get-data-frame)

    (define avg-cadence #f)

    (define (get-avg-cadence)
      (unless avg-cadence
        (let ((st (df-statistics (get-data-frame) "cad")))
          (set! avg-cadence (and st (statistics-mean st)))))
      avg-cadence)

    (define/override (get-redline-renderer)
      (let ((avg (get-avg-cadence)))
        (if (and avg (> avg 0))
            (function (lambda (x) avg)
                      #:label (string-append "Avg " (~r avg #:precision 1)))
            #f)))

    (define/override (set-data-frame data-frame)
      (set! avg-cadence #f)
      (super set-data-frame data-frame))

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
            (send visible-lb append (send item get-headline)))
          (send visible-lb set-selection nindex)
          (enable-disable-buttons))))

    ;; Populate the VISIBLE-LB and AVAILABLE-LB list-box% objects with data
    ;; from the VISIBLE and AVAILABLE lists.
    (define (refill)
      (send visible-lb clear)
      (for ([item visible])
        (send visible-lb append (send item get-headline)))
      (send available-lb clear)
      ;; Sort the available series, to make graps easier to find.
      (set! available
            (sort available
                  (lambda (a b)
                    (string<? (send a get-headline)
                              (send b get-headline)))))
      (for ([item available])
        (send available-lb append (send item get-headline))))

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
                               [g (in-value (findf (lambda (g) (eq? tag (send g get-preferences-tag))) all-graphs))]
                               #:when g)
                     g))
             (set! available
                   (for/list ([g all-graphs] #:unless (member g visible)) g))))
      (refill)
      (enable-disable-buttons)
      (if (send this do-edit parent)
          (for/list ([item visible]) (send item get-preferences-tag))
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
  (for/list ([c% (in-list (list speed-graph%
                                pace-graph%
                                speed-zone-graph%
                                grade-adjusted-pace-graph%
                                elevation-graph%
                                corrected-elevation-graph%
                                grade-graph%
                                grade+calt-graph%
                                grade+alt-graph%
                                calt+shaded-grade-graph%
                                alt+shaded-grade-graph%
                                heart-rate-graph%
                                heart-rate-zones-graph%
                                heart-rate-pct-graph%
                                temperature-graph%
                                cadence-graph%
                                stride-graph%
                                vertical-oscillation-graph%
                                vertical-ratio-graph%
                                ground-contact-time-graph%
                                ground-contact-percent-graph%
                                wbal-graph%
                                power-graph%
                                power+wbal-graph%
                                power-zone-graph%
                                lrbal-graph%
                                teff-graph%
                                psmth-graph%
                                cpsmth-graph%
                                pco-graph%
                                pp-start-graph%
                                pp-end-graph%
                                pp-angle-graph%
                                ppp-start-graph%
                                ppp-end-graph%
                                ppp-angle-graph%
                                gear-graph%
                                gear-index-graph%
                                gear-ratio-graph%
                                pwr-hr-reserve-graph%
                                spd-hr-reserve-graph%
                                adecl-graph%
                                map-graph%))])
    (new c% [parent parent] [style '(deleted)] [hover-callback hover-callback])))

(define (make-swim-graphs parent hover-callback)
  ;; NOTE: all graphs are created as '(deleted), so they are not visible.  The
  ;; graphs-panel% will control visibility by adding/removing graphs from the
  ;; parent panel
  (for/list ([c% (in-list (list swim-pace-graph%
                                swim-swolf-graph%
                                swim-stroke-count-graph%
                                swim-cadence-graph%
                                heart-rate-graph%
                                heart-rate-zones-graph%
                                heart-rate-pct-graph%))])
    (new c% [parent parent] [style '(deleted)] [hover-callback hover-callback])))


;; These are the graps used for Open Water Swimming -- some Pool Swimming
;; graphs don't make sense, but neither do the full graphs.
(define (make-owswim-graphs parent hover-callback)
  (for/list ([c% (in-list (list swim-pace-graph%
                                swim-cadence-graph%
                                speed-graph%
                                speed-zone-graph%
                                heart-rate-graph%
                                heart-rate-zones-graph%
                                heart-rate-pct-graph%
                                stride-graph%
                                temperature-graph%))])
    (new c% [parent parent] [style '(deleted)] [hover-callback hover-callback])))

(define (make-xdata-graphs parent hover-callback)
  (for/list ([md (get-available-xdata-metadata)])
    (new graph-view%
         [parent parent]
         [style '(deleted)]
         [hover-callback hover-callback]
         [primary-y-axis md])))

(define graph-panel%
  (class object%
    (init parent)
    (init-field
     [get-preference get-pref]
     [put-preference put-pref])
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
    ;; user preference.  Use `get-x-axis-by-sport` and `put-x-axis-by-sport`
    ;; to get and set values from this hash.
    (define x-axis-by-sport (make-hash))

    ;; Map the visible graphs by sport, this is saved as a user preference.
    ;; Each sport allows a different list of graphs to be shown and in a
    ;; different order.  Use `get-graphs-by-sport` and `put-graphs-by-sport`
    ;; to get and set values from this hash.
    (define graphs-by-sport (make-hash))

    ;; The axis-choices for the graphs, either default-x-axis-choices or
    ;; swim-x-axis-choices, depending on the session's sport
    (define x-axis-choices '())

    (define graphs '())      ; the list of graphs we are currently  displaying

    ;; The initial split of the horizontal-dragable-split-panel% used between
    ;; the lap list and the charts.
    (define initial-panel-split '(1/5 4/5))

    ;; Restore the preferences now.
    (let ((pref (get-preference the-pref-tag (lambda () #f))))
      (when (and pref (hash? pref))
        (set! initial-panel-split (hash-ref pref 'panel-split '(1/5 4/5)))
        (set! show-avg? (hash-ref pref 'show-avg? #f))
        (set! zoom-to-lap? (hash-ref pref 'zoom-to-lap? #f))
        (set! color-by-zone? (hash-ref pref 'color-by-zone? #f))
        (set! filter-amount (hash-ref pref 'filter-amount 0))
        (set! x-axis-by-sport (hash-copy (hash-ref pref 'x-axis-by-sport (hash))))
        (set! graphs-by-sport (hash-copy (hash-ref pref 'graphs-by-sport (hash))))))

    (define (zoom-to-lap zoom)
      (set! zoom-to-lap? zoom)
      (for ([g (in-list graphs)])
        (send g zoom-to-lap zoom)))

    (define (color-by-zone flag)
      (set! color-by-zone? flag)
      (for ([g (in-list graphs)])
        (send g color-by-zone flag)))

    (define (show-average-line show)
      (set! show-avg? show)
      (for ([g (in-list graphs)])
        (send g show-average-line show)))

    ;; Save these values here, since we might change the set of graphs after
    ;; highlight-lap was called...
    (define-values (highlight-interval-start highlight-interval-end) (values #f #f))

    (define (highlight-lap n lap)
      (let ((start (lap-start-time lap))
            (elapsed (lap-elapsed-time lap)))
        (if (and start elapsed)
            ;; use floor because timestamps are at 1 second precision and
            ;; this ensures swim laps are correctly highlighted.
            (let ([end (floor (+ start elapsed))])
              (set! highlight-interval-start start)
              (set! highlight-interval-end end))
            (begin
              (set! highlight-interval-start #f)
              (set! highlight-interval-end #f))))

      (for ([g (in-list graphs)])
        (send g highlight-interval highlight-interval-start highlight-interval-end)))

    (define (unhighlight-lap)
      (set! highlight-interval-start #f)
      (set! highlight-interval-end #f)
      (for ([g (in-list graphs)])
        (send g highlight-interval highlight-interval-start highlight-interval-end)))

    (define/private (put-x-axis-by-sport index)
      (define sport (session-sport the-session))
      (define sub-sport (session-sub-sport the-session))
      (define key
        (if sub-sport
            (cons sport sub-sport)
            sport))
      (hash-set! x-axis-by-sport key index))

    (define/private (get-x-axis-by-sport)
      (define sport (session-sport the-session))
      (define sub-sport (session-sub-sport the-session))
      (define key
        (if sub-sport
            (cons sport sub-sport)
            sport))
      (or (hash-ref x-axis-by-sport key #f)
          (hash-ref x-axis-by-sport sport 0)))

    (define/private (put-graphs-by-sport graphs)
      (define sport (session-sport the-session))
      (define sub-sport (session-sub-sport the-session))
      (define key
        (if sub-sport
            (cons sport sub-sport)
            sport))
      (hash-set! graphs-by-sport key graphs))

    (define/private (get-graphs-by-sport)
      (define sport (session-sport the-session))
      (define sub-sport (session-sub-sport the-session))
      (define key
        (if sub-sport
            (cons sport sub-sport)
            sport))
      (or (hash-ref graphs-by-sport key #f)
          (hash-ref graphs-by-sport sport #f)))

    (define (set-x-axis index)
      (let ((x-axis (cdr (list-ref x-axis-choices index))))
        (when the-session
          (put-x-axis-by-sport index))
        (for ([g (in-list graphs)])
          (send g set-x-axis x-axis))))

    (define (set-filter-amount a)
      (set! filter-amount a)
      (for ([g (in-list graphs)])
        (send g set-filter-amount a)))

    (define panel (new horizontal-dragable-split-panel%
                       [parent parent]
                       [border 0]
                       [alignment '(center top)]))

    (define interval-view-panel (new vertical-pane%
                                     [parent panel]
                                     [border 0]
                                     [spacing 1]
                                     [min-width 220]
                                     [stretchable-width #t]
                                     [alignment '(left top)]))

    (define interval-choice
      (let ((p (new horizontal-pane%
                    [parent interval-view-panel]
                    [spacing 10]
                    [stretchable-height #f]
                    [alignment '(left center)])))
        (new message% [parent p] [label "Laps"] [font *header-font*])
        (new interval-choice%
             [tag 'interval-choice-graphs]
             [parent p]
             [label ""]
             [callback (lambda () (unhighlight-lap))])))

    (define interval-view
      (new mini-interval-view%
           [parent interval-view-panel]
           [tag 'activity-log:charts-mini-lap-view]
           [callback (lambda (n lap selected?)
                       (if selected?
                           (let ((lap-num (dict-ref lap 'lap-num #f)))
                             (when lap-num
                               (highlight-lap (- lap-num 1) lap)))
                           (unhighlight-lap)))]))

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
         [label "Show Redline"]
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

    (define graphs-panel (new grid-pane%
                              [parent charts-panel]
                              [border 0]
                              [spacing 0]
                              [columns 1]
                              [alignment '(left top)]))

    (define default-graphs-1 #f)
    (define swim-graphs-1 #f)
    (define owswim-graphs-1 #f)
    (define xdata-graphs-1 #f)

    ;; Needs to be done after the panel has all its children.
    (send panel set-percentages initial-panel-split)

    (define (hover-callback y)
      (for ([g (in-list graphs)])
        (send g draw-marker-at y)))

    (define (default-graphs)
      (unless default-graphs-1
        (set! default-graphs-1 (make-default-graphs graphs-panel hover-callback)))
      default-graphs-1)

    (define (swim-graphs)
      (unless swim-graphs-1
        (set! swim-graphs-1 (make-swim-graphs graphs-panel hover-callback)))
      swim-graphs-1)

    (define (owswim-graphs)
      (unless owswim-graphs-1
        (set! owswim-graphs-1 (make-owswim-graphs graphs-panel hover-callback)))
      owswim-graphs-1)

    (define (xdata-graphs)
      (unless xdata-graphs-1
        (set! xdata-graphs-1 (make-xdata-graphs graphs-panel hover-callback)))
      xdata-graphs-1)

    (define sds-dialog #f)

    (define (on-select-data-series)
      (when the-session
        (unless sds-dialog
          (set! sds-dialog (new select-data-series-dialog%)))
        (let ((toplevel (send panel get-top-level-window))
              (visible-tags (get-graphs-by-sport))
              (all-graphs (append
                           (cond ((is-lap-swimming/df? data-frame) (swim-graphs))
                                 ((is-ow-swimming/df? data-frame) (owswim-graphs))
                                 (#t (default-graphs)))
                           (xdata-graphs))))
          (cond ((send sds-dialog show-dialog toplevel visible-tags all-graphs)
                 => (lambda (ngraps)
                      (put-graphs-by-sport ngraps)
                      (setup-graphs-for-current-session)))))))

    (define/public (save-visual-layout)
      (send interval-view save-visual-layout)
      (send interval-choice save-visual-layout)
      (put-preference
       the-pref-tag
       (hash
        'panel-split (send panel get-percentages)
        'show-avg? show-avg?
        'zoom-to-lap? zoom-to-lap?
        'color-by-zone? color-by-zone?
        'filter-amount filter-amount
        'x-axis-by-sport x-axis-by-sport
        'graphs-by-sport graphs-by-sport)))

    ;; Return the available graphs for SESSION.  For non-lap swimming
    ;; activities, we only use the graphs for which we have data.
    (define (get-graphs-for-session)
      (define sport (session-sport the-session))
      (define visible (get-graphs-by-sport))

      (define candidates
        (cond (visible
               ;; If we already have a list of graphs that are visible, select
               ;; them.
               (define c
                 (for/list ([g (cond ((is-lap-swimming/df? data-frame) (swim-graphs))
                                     ((is-ow-swimming/df? data-frame) (append (owswim-graphs) (xdata-graphs)))
                                     (#t (append (default-graphs) (xdata-graphs))))]
                            #:when (member (send g get-preferences-tag) visible))
                   g))
               ;; Ensure the graphs are in the same order as the visible list
               ;; (this is controlled by the "select series" dialog box.
               (for/list ([v (in-list visible)])
                 (for/first ([g (in-list c)]
                             #:when (equal? v (send g get-preferences-tag)))
                   g)))
              ;; If there are no visible graphs, select suitable defaults.  We
              ;; have too many plots to put them all, but the user can always
              ;; select the one they want
              (#t
               (define chart-classes
                 (cond
                   ((is-runnig? sport)
                    (list pace-graph% heart-rate-graph% cadence-graph%))
                   ((is-cycling? sport)
                    (list power-graph% heart-rate-graph% speed-graph% cadence-graph%))
                   ((is-lap-swimming? sport)
                    (list swim-pace-graph% swim-cadence-graph% swim-swolf-graph%))
                   (#t
                    (list speed-graph% power-graph% heart-rate-graph% cadence-graph%))))
               (define candidates
                 (cond ((is-lap-swimming/df? data-frame) (swim-graphs))
                       ((is-ow-swimming/df? data-frame) (append (owswim-graphs) (xdata-graphs)))
                       (#t (append (default-graphs) (xdata-graphs)))))
               (for/list ([c (in-list chart-classes)])
                 (for/first ([g (in-list candidates)] #:when (is-a? g c))
                   g)))))

      ;; Since no visible tags were present, we just made up some default
      ;; lists, set them here, so they show up correctly when the user opens
      ;; the "Select data series" dialog.
      (unless visible
        (define tags (for/list ([g (in-list candidates)] #:when g)
                       (send g get-preferences-tag)))
        (hash-set! graphs-by-sport sport tags))

      (for/list ([g (in-list candidates)]
                 #:when (and g (send g is-valid-for? data-frame)))
        g))

    (define (setup-graphs-for-current-session)
      (set! graphs (get-graphs-for-session))
      (let ([graph-count (length graphs)])
        (cond ((<= graph-count 3)
               (send graphs-panel column-count 1))
              ((<= graph-count 6)
               (send graphs-panel column-count 2))
              (#t
               (send graphs-panel column-count 3))))
      (send graphs-panel change-children
            (lambda (old) (map (lambda (g) (send g get-graph-canvas)) graphs)))
      (let* ((sel (send x-axis-choice get-selection))
             (x-axis (cdr (list-ref x-axis-choices sel))))
        (for-each (lambda (g)
                    (send g begin-edit-sequence)
                    (send g set-x-axis x-axis)
                    (send g zoom-to-lap zoom-to-lap?)
                    (send g color-by-zone color-by-zone?)
                    (send g show-average-line show-avg?)
                    (send g set-filter-amount filter-amount)
                    (send g set-data-frame data-frame)
                    ;; highlight-interval must be called after set-data-frame
                    (send g highlight-interval highlight-interval-start highlight-interval-end)
                    (send g end-edit-sequence))
                  graphs)))

    (define/public (set-session session df)
      (unhighlight-lap)
      ;; Rebuild xdata graphs, as new series might have been created...
      (set! xdata-graphs-1 #f)
      (set! the-session session)
      (set! data-frame df)

      (let ((lap-swimming? (is-lap-swimming/df? data-frame)))

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
        (send control-panel reflow-container)

        (let ((sport-x-axis (get-x-axis-by-sport)))
          (if (>= sport-x-axis (length x-axis-choices))
              (send x-axis-choice set-selection 0)
              (send x-axis-choice set-selection sport-x-axis))
          (setup-graphs-for-current-session)))

      (send interval-choice set-session session df))

    ))
