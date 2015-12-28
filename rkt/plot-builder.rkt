#lang racket
;; plot-builder.rkt -- produce plot render functions for track data in
;; sessions
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

;;; Comentary:
;;
;; Produce plot render functions for track data in sessions.  The render
;; functions can be passed to plot type functions to display the graphs.
;; plot-builder% should be used for most activities, and swim-plot-builder%
;; should be used for lap swimming activities.

(require data/queue
         plot
         racket/stream
         "al-prefs.rkt"
         "fmt-util.rkt"
         "activity-util.rkt"
         "plot-axis-def.rkt"
         "sport-charms.rkt"
         "utilities.rkt")

(provide extract-data)
(provide get-axis-plot-color)
(provide best-avg-ticks)
(provide important-best-avg-durations)
(provide make-best-avg)
(provide make-delta-series)
(provide make-histogram)
(provide make-histogram-renderer)
(provide make-low-pass-filter)
(provide plot-builder%)
(provide swim-plot-builder%)

;; Color to be used by default, when an axis definition does not supply a
;; color for the plot.
(define default-plot-color '(0 148 255))

(define (get-axis-plot-color axis-def)
  (or (send axis-def get-line-color) default-plot-color))


;........................................... Extract data from sessions ....

;; Number of samples (from the beginning) to smooth in the data series when
;; FORCE-ZERO-START? is #t
(define num-samples-to-smooth
  (let ((tag 'activity-log:num-samples-to-smooth))
    (al-get-pref tag (lambda () 40))))

;; Number of samples used to compute the average used in smoothing when
;; FORCE-ZERO-START? is #t, these are the samples right after
;; num-samples-to-smooth.
(define num-samples-to-average
  (let ((tag 'activity-log:num-samples-to-average))
    (al-get-pref tag (lambda () 40))))


;; Return a function which will extract graph data from a session.  The
;; function will accept a SESSION and return a list with the data points.
;; Each data point is a vector of [elapsed-time, x-val, y-val].
;;
;; X-FN, Y-FN are extractor functions used to get the x and y values
;;
;; When STOP-DETECTION? is #t, the function will attempt to detect stop points
;; and generate additional data points with 0 Y values, this makes graphs look
;; nicer.
;;
;; When FORCE-ZERO-START? is #t, the function will generate an initial which
;; is the average of the first few datapoints.  This will produce nicer graphs
;; for data series that has the tendency to produce very high values at the
;; start of a session (for example pace, vertical oscillation, etc).
;;
(define (make-data-extractor x-fn y-fn stop-detection? force-zero-start?)

  (define (extract-fn prev current)
    (vector
     (extract-time prev current)
     (x-fn prev current)
     (y-fn prev current)))

  (define (valid? val)
    (and (vector-ref val 0) (vector-ref val 1) (vector-ref val 2)))

  (define (is-stop-point? old new)
    (and stop-detection?
         (let* ((tdiff (- (vector-ref new 0) (vector-ref old 0)))
                (vdiff (abs (- (vector-ref new 2) (vector-ref old 2))))
                (vchange
                 (if (< (vector-ref new 2) 0.001)
                     1.0
                     (/ vdiff (vector-ref new 2)))))
           ;; (and (> tdiff 10)                 ; large elapsed time delta
           ;;      (> vchange 0.1))             ; large value difference (percentage)
           (> tdiff 15)
           )))

  (lambda (session)
    (let ((data '()))
      (for-each-session-trackpoint
       session
       (lambda (prev current)
         (let ((val (extract-fn prev current)))
           (when (valid? val)
             (unless (null? data)
               (when (is-stop-point? (car data) val)
                 ;; stop point
                 (let ((pval (vector-copy (car data))))
                   (vector-set! pval 1 (+ (vector-ref pval 1) 0.00001))
                   (vector-set! pval 2 0)
                   (set! data (cons pval data)))
                 (let ((pval (vector-copy val)))
                   (vector-set! pval 1 (- (vector-ref pval 1) 0.00001))
                   (vector-set! pval 2 0)
                   (set! data (cons pval data)))
                 ))
             (set! data (cons val data))))))
      (let ((data (reverse data)))
        (define nsamples num-samples-to-average)
        (define start num-samples-to-smooth)
        ;; Compute the average of the first `nsamples' and use it to smooth
        ;; the first values.  Note that the average is computed naively, not
        ;; taking into account that the "distance" between data points is not
        ;; the same (for example, when Garmin smart recording is used).  It
        ;; does seem to produce suitable results.
        (when (and force-zero-start? (> (length data) (+ start nsamples)))
          (let ((avg (/ (for/sum ([d (list-tail data start)] [n (in-range nsamples)])
                                 (vector-ref d 2))
                        nsamples)))
            (for ([d data] [n (in-range start)])
              (let ((item (list-ref data n))
                    (split (/ n start)))
                (vector-set! item 2 (+ (* (vector-ref item 2) split)
                                       (* avg (- 1 split))))))))
        data))))

;; Return a filter function based on filter definitions for the X and Y axis
;; and a FILTER-AMOUNT.  This function will always return a valid value:
;; identity is returned when there is no filtering defined.  The rules are:
;;
;; - Y axis definition determines whether filtering is used or not
;;
;; - X asis definition determines the base filter width (which is multiplied
;;   by FILTER-AMOUNT to obtain the actual filter width)
;;
;; - X asis definition determines if stop detection is to be used or not.
;;
;; A low pass filer (see `make-low-pass-filter') is returned or `identity' if
;; no filter can be constructed.
;;
(define (get-filter-function x-axis-def y-axis-def filter-amount)
  (let ((have-filter? (send y-axis-def should-filter?))
        (base-width (send x-axis-def get-filter-width))
        (stop-detection? (send x-axis-def has-stop-detection?)))
    (if (and have-filter? base-width (> filter-amount 0))
        (make-low-pass-filter (* base-width filter-amount) stop-detection?)
        identity)))

;; Create a low-pass filter of WIDTH.  The returned function will accept a
;; data point (a vector of [X Y]) and will return a new filtered datapoint [X
;; Y-filtered]).  The filter will keep state.
;;
;; When STOP-DETECTION? is #t, the function will attempt to detect stop points
;; (where dX is 0) and reset the filter.  This will produce nicer looking
;; graphs.  This works best if the data to be filtered was extracted with stop
;; detection (see `make-low-pass-filter')
;;
(define (make-low-pass-filter width stop-detection?)
  ;; NOTE: this function can be called from multiple threads and the value of
  ;; STATE is shared between subsequent calls to this function, so we make
  ;; sure it has a per-thread value.
  (let ((state (make-thread-cell #f)))
    (lambda (v)
      (if (eq? (thread-cell-ref state) #f)
          (begin (thread-cell-set! state v) v) ; first value

          ;; Start filtering
          (let* ((dt (- (vector-ref v 0) (vector-ref (thread-cell-ref state) 0)))
                 (alpha (/ dt (+ dt width)))
                 (new-v (vector (vector-ref v 0)
                                (+ (* alpha (vector-ref v 1))
                                   (* (- 1 alpha)
                                      (vector-ref (thread-cell-ref state) 1))))))
            ;; NOTE: stop detection introduces duplicate items (same X axis
            ;; value) to make graphs look nicer.  Don't break it, reset the
            ;; filter instead.
            (if (and stop-detection? (< dt 0.001))
                (let ((v (vector-copy v)))
                  (vector-set! v 1 0)
                  (thread-cell-set! state v)
                  v)
                (begin (thread-cell-set! state new-v) new-v)))))))

;; Return a histogram from DATA which is a list of data points, each point is
;; a vector of [X Y].  The Y values are grouped into buckets of BUCKET-WIDTH
;; (see `val->bucket` inside this function).  The value accumulated in the
;; buckets is the delta X difference between two datapoints (since data points
;; might not be equally spaced, for example in Gramin smart recording).
;;
;; NOTE: stop detection is not performed on the data series!
;;
;; When AS-PERCENTAGE? is #t, the histogram values are percentage of the total
;; x range of the data series, otherwise the histogram contains the delta-x
;; value in each bucket.
;;
;; When INCLUDE-ZEROES? is #t, values that end up in bucket 0 will be
;; discarded, this is usefull when plotting cadence values for example, where
;; coasting has a cadence of 0.
;;
;; A vector of data points is returned, each data point being a vector pf
;; [BUCKET PCT-OR-X].  The vector is sorted by BUCKET number.
;;
(define (make-histogram data-series bucket-width as-percentage? include-zeroes?)
  (when (equal? bucket-width 0) (set! bucket-width 1))

  ;; NOTE: using `exact-truncate' instead of `exact-round' works more
  ;; correctly for distributing values into buckets for zones.  The bucket
  ;; value is the start of the interval (as opposed to the middle of the
  ;; interval if `exact-round` would be used.
  (define (val->bucket v)
    (exact-truncate (/ v bucket-width)))

  (let ((histogram (make-hash))
        (total 0))

    (when (cons? data-series)
      (let loop ((prev data-series)
                 (next (cdr data-series)))
        (when (cons? next)
          (let ((delta (- (vector-ref (car next) 0)
                          (vector-ref (car prev) 0)))
                (bucket (let ((y (/ (+ (vector-ref (car next) 1)
                                       (vector-ref (car prev) 1))
                                    2)))
                          (val->bucket y))))
            (when (or (not (eq? bucket 0)) include-zeroes?)
              (set! total (+ total delta))
              (let ((value (hash-ref histogram bucket 0)))
                (hash-set! histogram bucket (+ delta value)))))
          (loop (cdr prev) (cdr next)))))
          
    (let ((result (for/vector
                   ((k (sort (hash-keys histogram) <)))
                   (vector (* k bucket-width)
                           (let ((val (hash-ref histogram k #f)))
                             (if (and as-percentage? (> total 0))
                                 (* 100 (/ val total))
                                 val))))))
      (if (= (vector-length result) 0) #f result))))

;; Return a list of the buckets in a histogram (as made by `make-histogram`).
(define (get-histogram-buckets h)
  (for/list ([e (in-vector h)])
    (vector-ref e 0)))

;; Merge two sorted lists.
(define (merge-lists l1 l2)
  (let loop ((l1 l1)
             (l2 l2)
             (result '()))
    (cond ((null? l1) (append (reverse result) l2))
          ((null? l2) (append (reverse result) l1))
          ((= (car l1) (car l2)) (loop (cdr l1) (cdr l2) (cons (car l1) result)))
          ((< (car l1) (car l2)) (loop (cdr l1) l2 (cons (car l1) result)))
          (#t (loop l1 (cdr l2) (cons (car l2) result))))))

;; Ensure that HISTOGRAM has all buckets in BUCKETS (a sorted list).  This is
;; done by adding buckets with 0 elements if needed.  This is used when
;; histograms for two data series need to be displayed on a single plot.
(define (normalize-histogram histogram buckets)
  (for/vector ([b buckets])
    (or (for/first ([h histogram]
                    #:when (eqv? b (vector-ref h 0)))
          h)
        (vector b 0))))

;; Return a list of a SESSION's lap start points inside DATA.  DATA is a list
;; of data points as produced by `make-data-extractor`, each data point is a
;; vector of [ELAPSED-TIME X Y].  We return a list of X values from the data
;; points closest to each lap start, which is determined by comparing the
;; lap's start time with the data point's ELAPSED-TIME value
;;
(define (get-lap-markers session data)
  ;; Return the first data point in DATA-SERIES with a timestamp after the
  ;; LAP's start time. Might return #f if no such point is found.
  (define (find-first-data-point lap)
    (let ((lap-start (lap-start-time lap)))
      (findf (lambda (d) (<= lap-start (vector-ref d 0))) data)))

  (for/list ((lap (in-list (session-laps session))))
    (let ((data-point (find-first-data-point lap)))
      (if data-point (vector-ref data-point 1) #f))))

;; Create a historgam plot renderer from DATA (a list of [BUCKET
;; NUM-SAMPLES]), as received from `make-histogram` (which see). COLOR will be
;; the color of the plot.
;;
;; The resulting plot renderer can be passed to `plot` or any related
;; functions to be displayed.
;;
(define (make-histogram-renderer data color
                                 #:skip [skip (discrete-histogram-skip)]
                                 #:x-min [x-min 0]
                                 #:label [label #f])
  (let ((kwd '())
        (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    (let ((max-val #f))
      ;; Determine the max value in the plot
      (for ((d (in-vector data)))
        (let ((v (vector-ref d 1)))
          (when (or (not max-val) (> v max-val))
            (set! max-val v))))
      ;; Make the max value of the plot larger, so the top value does not
      ;; reach the top of the plot area.
      (add-arg '#:y-max (* max-val 1.1)))
    (add-arg '#:x-min x-min)
    (add-arg '#:skip skip)
    (add-arg '#:line-width 2)
    (add-arg '#:line-color color)
    (add-arg '#:label label)
    (add-arg '#:color color)
    (add-arg '#:alpha 0.8)
    (keyword-apply discrete-histogram kwd val data '())))

;; Create a plot renderer which plots a box with the bounds START, END,
;; MIN-VAL, MAX-VAL and it is ranged according to AXIS-DEF.  COLOR specifies
;; the color of the box, if #f, the color is retrieved from the ASIX-DEF.
;;
(define (make-box-renderer start end min-val max-val axis-def [color #f])
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted
    (let ((y-range (send axis-def get-y-range)))
      (when y-range
        (add-arg '#:y-min (car y-range))
        (add-arg '#:y-max (cdr y-range))))
    
    (add-arg '#:color (or color (get-axis-plot-color axis-def)))
    (add-arg '#:alpha 0.2)

    (keyword-apply
         lines-interval kwd val
         (list (vector start max-val) (vector end max-val))
         (list (vector start min-val) (vector end min-val))
         '())))

;; Create a plot renderer which plots DATA-SERIES (a list of [X Y]) using
;; points.  MIN-X, MAX-X, MIN-Y, MAX-Y are the bounding value of the plot.
;;
(define (make-scatter-renderer data-series min-x max-x min-y max-y color)
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted
    
    ;; Enlarge the graph area for scatter graphs, so that points are not drawn
    ;; along the edges.
    (let ((x-extend (* 0.05 (abs (- max-x min-x))))
          (y-extend (* 0.05 (abs (- max-y min-y)))))
      (add-arg '#:y-min (- min-y y-extend))
      (add-arg '#:y-max (+ max-y y-extend))
      (add-arg '#:x-min (- min-x x-extend))
      (add-arg '#:x-max (+ max-x x-extend)))
    (add-arg '#:color color)
    (keyword-apply points kwd val data-series '())))

;; Create a plot renderer which plots DATA-SERIES (a list of [X Y]) using
;; lines (a "normal" plot).  Y-RANGE is either #f or a cons of the min and max
;; Y values of the graph.
;;
(define (make-plot-renderer data-series y-range color)
  (let ((kwd '()) (val '()))
    (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
    ;; WARNING: keywords used by keyword-apply have to be sorted

    ;; Use min and max y values for the graphs, if these are specified.
    ;; Otherwise, we let the plot library determine bounds.
    (when y-range
      (add-arg '#:y-min (car y-range))
      (add-arg '#:y-max (cdr y-range)))

    (add-arg '#:width 2.5)
    (add-arg '#:color color)
    (keyword-apply lines kwd val data-series '())))

;; Re-sample DATA, a (Listof (Vector Elapsed-Time X Y)), so that we have
;; samples at 1 second interval.  Returns a (Streamof (Vector Elapsed-Time X
;; Y)).
(define (re-sample-1sec data)

  (define (interpolate t x y dx dy t-lim)
    (if (>= t t-lim)
        empty-stream
        (stream-cons
         (vector t x y)
         (interpolate (+ t 1) (+ x dx) (+ y dy) dx dy t-lim))))
  
  (cond ((null? data) empty-stream)
        ((null? (cdr data)) (stream-cons (car data) empty-stream))
        (#t
         (match-define (vector t1 x1 y1) (first data))
         (match-define (vector t2 x2 y2) (second data))
         (define delta-t (- t2 t1))
         (define dx (/ (- x2 x1) delta-t))
         (define dy (/ (- y2 y1) delta-t))
         (stream-append
          ;; long delays indicate stops, don't interpolate between those
          (if (< delta-t 10)
              (interpolate t1 x1 y1 dx dy t2)
              (stream-cons (first data) empty-stream))
          (re-sample-1sec (cdr data))))))

;; Delays the Y axis in DATA, a (Listof (Vector Elapsed-Time X Y)), by DELAY
;; seconds.  Returns a (Streamof (Vector Elapsed-Time X Delayed-Y)), can be
;; converted back to a list using stream->list.
(define (delay-data data delay)
  (define (delay1 d1 d2)
    (cond ((or (stream-empty? d1)
               (stream-empty? d2))
           empty-stream)
          (#t
           (let ((datum1 (stream-first d1))
                 (datum2 (stream-first d2)))
             (match-define (vector t1 x1 y1) datum1)
             (match-define (vector t2 x2 y2) datum2)
             (if (>= (- t2 t1) delay)
                 (stream-cons
                  (vector t1 x1 y2)
                  (if (< (- t2 t1) (+ delay 1))
                      (delay1 (stream-rest d1) (stream-rest d2))
                      (delay1 (stream-rest d1) d2)))
                 (delay1 d1 (stream-rest d2)))))))
  (let ((stream (re-sample-1sec data)))
    (delay1 stream stream)))
                
;; Extract graph data from SESSION using X and Y axis definitions. This
;; function returns multiple values: the actual graph data, the lap markers
;; (see `get-lap-markers`) and the bounding box of the data.
;;
(define (extract-data session x-axis y-axis [filter-amount 0] [delay-amount #f])
  (if (and session x-axis y-axis)
      (let* ((session-id (assq1 'database-id session))

             (extract-fn (make-data-extractor 
                          (send x-axis get-extractor-fn session-id)
                          (send y-axis get-extractor-fn session-id)
                          (send x-axis has-stop-detection?)
                          (send y-axis should-force-zero-start?)))
             (filter-fn (get-filter-function x-axis y-axis filter-amount))

             (data (extract-fn session)) ; (Listof (Vector ELAPSED-TIME X Y))

             (data-series '())             ; (Listof (Vector X Y))
             (min-x #f)
             (max-x #f)
             (min-y #f)
             (max-y #f))

        (when (and delay-amount (> delay-amount 0))
          (set! data (stream->list (delay-data data delay-amount))))

        ;; Filter the extracted data and compute the bounding box
        (for-each (lambda (v)
                    (let* ((fv (filter-fn (vector (vector-ref v 1) (vector-ref v 2))))
                           (x (vector-ref fv 0))
                           (y (vector-ref fv 1)))
                      (set! data-series (cons fv data-series))
                      (set! min-x (if min-x (min min-x x) x))
                      (set! max-x (if max-x (max max-x x) x))
                      (set! min-y (if min-y (min min-y y) y))
                      (set! max-y (if max-y (max max-y y) y))))
                  data)

        (values
         (if (null? data-series) #f (reverse data-series))
         ;; NOTE that lap markers are computed of data
         (if (null? data) #f (get-lap-markers session data))
         min-x max-x min-y max-y))
      (values #f #f #f #f #f #f)))

;; Do a running average filter on DATA-SERIES using FILTER-WIDTH samples.  The
;; X and Y are filtered independently of each other.  This is currently used
;; in scatter plots, as it is too complicated to do a low-pass filtering on
;; that data.
(define (ravg-filter data-series filter-width)
  (let ((q (make-queue)))
    (for/list ((datum (in-list data-series)))
      (enqueue! q datum)
      (when (> (queue-length q) filter-width) (dequeue! q))
      (let ((x-total 0) (y-total 0))
        (for ((e (in-queue q)))
          (set! x-total (+ x-total (vector-ref e 0)))
          (set! y-total (+ y-total (vector-ref e 1))))
        (vector (/ x-total (queue-length q)) (/ y-total (queue-length q)))))))

(define (generate-best-avg-durations start limit [growth-factor 1.05])
  (let loop ((series (list start)) (current start))
    (let ((nval (exact-round (* current growth-factor))))
      (when (< nval (+ current 5))
        (set! nval (+ 20 current)))     ; ensure min growth
      (if (< nval limit)
          (loop (cons nval series) nval)
          (reverse series)))))

(define default-best-avg-durations
  (generate-best-avg-durations 10 (* 300 60) 1.2))

(define important-best-avg-durations
  (list 1 5 10 30 60 90 (* 3 60) (* 5 60) (* 10 60) (* 15 60)
        (* 20 60) (* 30 60) (* 45 60) (* 60 60)
        (* 90 60) (* 120 60) (* 180 60)))

;; (printf "(length default-best-avg-durations): ~a~%"
;;         (length default-best-avg-durations))
;; (printf "(length important-best-avg-durations): ~a~%"
;;         (length important-best-avg-durations))

;; Plot ticks for the best-avg plot.  Produces ticks at
;; important-best-avg-durations locations.
(define (best-avg-ticks)
  (ticks
   (lambda (start end)
     (for/list ((d important-best-avg-durations) #:when (and (>= d start) (<= d end)))
       (pre-tick d #t)))
   (lambda (start end ticks)
     (for/list [(tick ticks)]
       (duration->string (pre-tick-value tick))))))


;; Given a data series (Listof (Vector X Y)), compute the delta series by
;; combining adjacent samples.  The result is a (Listof (Vector Delta-X
;; Slice-Y)), where Delta-X is the difference between two adjacent X values
;; and Slice-Y is the "area" (integral) of the slice between the two X values.
(define (make-delta-series data-series)
  (for/list ([first data-series]
             [second (cdr data-series)])
    (match-define (vector x1 y1) first)
    (match-define (vector x2 y2) second)
    (let ((dt (- x2 x1)))
      (vector dt (* dt (/ (+ y1 y2) 2)) x1))))

;; Compute the best averave value from a delta series (as produced by
;; MAKE-DELTA-SERIES) over DURATION.  If INVERTED? is #t, the "best" is
;; condidered the smallest value (this is usefull for pace, vertical
;; oscilation, etc.)
(define (get-best-avg delta-series duration inverted?)

  (define cmp-fn (if inverted? < >))
  
  (define best-total #f)
  (define best-avg-pos #f)

  (define (maybe-update total start-pos)
    (when (or (not best-total) (and total (cmp-fn total best-total)))
      (set! best-total total)
      (set! best-avg-pos start-pos)))
  
  (let loop ((running-duration 0)
             (running-total 0)
             (head delta-series)
             (tail delta-series))
    (unless (null? tail)
      (match-define (vector dt y _) (car tail))
      (let ((diff (- (+ running-duration dt) duration)))
        (if (< diff 0)
            ;; running-duration is too small, add more samples
            (loop (+ running-duration dt) (+ running-total y) head (cdr tail))
            ;; ELSE: current sample completes the necessary duration, compute
            ;; the partial slice (for running, dt can be up to 7 seconds!)
            ;; and update the average.
            (let* ((partial-dt (- dt diff))
                   (partial-y (* y (/ partial-dt dt))))
              (match-define (vector dt y s) (car head)) ; NOTE: different dt, y
              (maybe-update (+ running-total partial-y) s)
              ;; Remove oldest element from running-duration, running-total
              ;; and continue.
              (loop (- running-duration dt) (- running-total y) (cdr head) tail))))))

  (vector duration (if best-total (/ best-total duration) #f) best-avg-pos))

;; Construct a data series over the best average values of DATA over
;; DURATIONS.  INVERTED? is passed to get-best-avg.
(define (make-best-avg data [inverted? #f] [durations default-best-avg-durations])
  (if (or (null? data) (null? (cdr data))) ; need at least two sample points
      '()
      (let ((delta-series (make-delta-series data)))
        (for/list ([d durations])
          (get-best-avg delta-series d inverted?)))))

;; Builds various plot renderers for sessions.  Allows setting the X and Y
;; axes plus the session and maintains a cached state to minimize
;; computations.
;;
(define plot-builder%
  (class object% (init) (super-new)

    (define session #f)                 ; as returned by `db-extract-session`, which see
    (define x-axis #f)                  ; an axis definition (see plot-axis-def.rkt)
    (define y-axis #f)                  ; like x-axis
    (define secondary-y-axis #f)        ; secondary Y axis dual plots
                                        ; (e.g. left, right torque efficiency)
    (define filter-amount 0)            ; used by `get-filter-function`, which see
    (define delay-amount 0)

    (define data-series #f)              ; (Listof (Vector X Y))
    (define secondary-data-series #f)
    (define lap-markers #f)              ; (Listof X), see `get-lap-markers`
    
    ;; Bounding box for the graph
    (define min-graph-x #f)
    (define max-graph-x #f)
    (define min-graph-y #f)
    (define max-graph-y #f)

    (define (invalidate-data-series)
      (set! data-series #f)
      (set! secondary-data-series #f)
      (set! lap-markers #f)
      (set! min-graph-x #f)
      (set! max-graph-x #f)
      (set! min-graph-y #f)
      (set! max-graph-y #f))

    (define (prepare-data-series)
      (when session
        (let-values (([data lapmkr min-x max-x min-y max-y]
                      (extract-data session x-axis y-axis filter-amount delay-amount)))
          (set! data-series data)
          (set! lap-markers lapmkr)
          (set! min-graph-x min-x)
          (set! max-graph-x max-x)
          (set! min-graph-y min-y)
          (set! max-graph-y max-y))
        (when secondary-y-axis 
          (let-values (([data lapmkr min-x max-x min-y max-y]
                        (extract-data session x-axis secondary-y-axis filter-amount)))
            (set! secondary-data-series data)
            (set! min-graph-x (if min-graph-x (min min-graph-x min-x) min-x))
            (set! max-graph-x (if max-graph-x (max max-graph-x max-x) max-x))
            (set! min-graph-y (if min-graph-y (min min-graph-y min-y) min-y))
            (set! max-graph-y (if max-graph-y (max max-graph-y max-y) max-y))))))

    (define (get-max-graph-value)
      (cond ((and y-axis
                  (send y-axis get-y-range)
                  (cdr (send y-axis get-y-range)))
             (cdr (send y-axis get-y-range)))
            (max-graph-y)
            (#t 10000)))

    (define (get-min-graph-value)
      (cond ((and y-axis
                  (send y-axis get-y-range)
                  (car (send y-axis get-y-range)))
             (car (send y-axis get-y-range)))
            (min-graph-y)
            (#t 0)))

    (define/public (set-filter-amount a)
      (set! filter-amount a)
      (invalidate-data-series))

    (define/public (set-x-axis x)
      (set! x-axis x)
      (invalidate-data-series))

    (define/public (set-y-axis y)
      (set! y-axis y)
      (invalidate-data-series))

    (define/public (set-secondary-y-axis y)
      (set! secondary-y-axis y)
      (invalidate-data-series))

    (define/public (set-delay-amount d)
      (set! delay-amount d)
      (invalidate-data-series))

    (define/public (set-session s)
      (set! session s)
      (invalidate-data-series))

    (define/public (get-plot-renderer [scatter? #f])
      (unless data-series (prepare-data-series))
      (if data-series 
          (if scatter?
              (let ((ds (if (> filter-amount 0)
                            (ravg-filter data-series (* filter-amount 5))
                            data-series)))
                (if (> (length ds) 0)
                    (make-scatter-renderer 
                     ds min-graph-x max-graph-x min-graph-y max-graph-y 
                     (get-axis-plot-color y-axis))
                    #f))
              (make-plot-renderer
               data-series (send y-axis get-y-range)
               (get-axis-plot-color y-axis)))
          #f))

    ;; Return a histogram renderer for one data series. Assumes at least the
    ;; primary data series is configured and exists.
    (define (get-histogram-renderer/single bucket-width as-percentage? include-zeroes?)
      (let ((data (make-histogram data-series bucket-width as-percentage? include-zeroes?)))
        (if data
            (list (make-histogram-renderer data (get-axis-plot-color y-axis)))
            #f)))

    ;; Retun a histogram renderer for two data series.  Assumes two data
    ;; series are configured and exist.
    (define (get-histogram-renderer/dual bucket-width as-percentage? include-zeroes?)
      (let* ((data (make-histogram data-series bucket-width as-percentage? include-zeroes? ))
             (data2 (make-histogram secondary-data-series bucket-width as-percentage? include-zeroes?)))
        (if (and data data2)
            (let ((nbuckets (merge-lists (get-histogram-buckets data) (get-histogram-buckets data2))))
              (set! data (normalize-histogram data nbuckets))
              (set! data2 (normalize-histogram data2 nbuckets))
              (let ((h1 (make-histogram-renderer
                         data (get-axis-plot-color y-axis)
                         #:skip 2.5 #:x-min 0 #:label (send y-axis get-series-label)))
                    (h2 (make-histogram-renderer
                         data2 (get-axis-plot-color secondary-y-axis)
                         #:skip 2.5 #:x-min 1 #:label (send secondary-y-axis get-series-label))))
                (list h1 h2)))
            #f)))

    (define/public (get-histogram-renderer bucket-width as-percentage? include-zeroes?)
      (unless data-series (prepare-data-series))
      (if data-series
          (if secondary-data-series
              ;; Dual histogram
              (get-histogram-renderer/dual bucket-width as-percentage? include-zeroes?)
              ;; Single histogram
              (get-histogram-renderer/single bucket-width as-percentage? include-zeroes?))
          #f))

    (define/public (get-lap-start-end lap-num)
      (unless data-series (prepare-data-series))
      (cond ((or (< lap-num 0)
		 (>= lap-num (length lap-markers)))
	     #f)
	    ((< lap-num (- (length lap-markers) 1))
             (let ([start (list-ref lap-markers lap-num)]
                   [end (list-ref lap-markers (+ 1 lap-num))])
               (and start end (cons start end))))
	    (#t
	     (let ([start (list-ref lap-markers lap-num)]
                   [end (vector-ref (last data-series) 0)])
               (and start end (cons start end))))))

    (define/public (get-plot-renderer-for-lap lap-num)
      (unless data-series (prepare-data-series))
      (if data-series
	  (let ((range (get-lap-start-end lap-num)))
            ;; NOTE: we need to handle broken activities, where sensor data is
            ;; lost durning the activtiy.
	    (if (and range (or (car range) (cdr range)))
		(make-box-renderer
                 (or (car range) min-graph-x)
                 (or (cdr range) max-graph-x)
                 (get-min-graph-value) (get-max-graph-value)
                 y-axis)
		#f))
	    #f))
    ))


;...................................................... swimming graphs ....

;; Construct a function that extracts graph data from a swim session.  The
;; funcion will accept a SESSION and return a list with the data points. Each
;; data point is a vector of 4 elements: the [elapsed-time, x-val, y-val,
;; swim-stroke] values.
;;
;; NOTE: this function is applied to swimming activities and will map over
;; the session lengths not over the track points.
;;
;; X-FN and Y-FN are extractor functions used to gget the x and y values.  See
;; `get-extractor-fn`
;;
;; FIXUP, is either #f or a function that will filter data before it is
;; returned.  It is currently used to fixup x axis values for time based swim
;; graphs (see `fixup-swim-time`)
;;
(define (make-swim-data-extractor x-fn y-fn fixup)

  (define (extract-fn length)
    (vector
     (length-start-time length)
     (x-fn length)
     (y-fn length)
     (length-swim-stroke length)))

  (lambda (session)
    (let ((data (map-session-lengths session extract-fn)))
      (set! data
            (filter (lambda (d)
                      (and (vector-ref d 0)
                           (vector-ref d 1)
                           (vector-ref d 2)))
                    data))
      (if fixup (fixup data) data))))

;; Duplicate items in DATA such that plots conect with vertical lines.
(define (add-verticals data)
  (let ((result '()))
    (when (pair? data)
      (for-each
       (lambda (d e)
         (set! result (cons d result))
         (set! result (cons (let ((v (vector-copy e)))
                              (vector-set! v 1 (vector-ref d 1))
                              v)
                            result)))
       (drop-right data 1)
       (cdr data))
      (set! result (cons (last data) result)))
    (if (pair? result)
        (let ((result (reverse result)))
          (let ((f (vector-copy (car result))))
            (vector-set! f 1 0)
            (cons f result)))
        result)))

;; Return a plot renderer that colours segments of the plot based on the swim
;; stroke.  This is used instead of `make-plot-renderer` when plotting some of
;; the swimming graphs.  The function expects the swim stroke to be stored in
;; the 4th slot of each data point in DATA-SERIES (Vectorof time x y
;; swim-stroke)

(define (make-swim-coloured-renderer data-series)

  (define renderers '())

  (define (plot-item data-point)
    (vector (vector-ref data-point 1) (vector-ref data-point 2)))

  (define (swim-stroke data-point)
    (vector-ref data-point 3))

  (define (add-renderer slice width color)
    (set! renderers (cons (lines slice #:width width #:color color) renderers)))

  (let ((slice '())
        (color "gray")
        (stroke #f))
    (for-each (lambda (d)
                (if (eqv? (swim-stroke d) stroke)
                    (set! slice (cons (plot-item d) slice))
                    (begin
                      (when (pair? slice)
                        (add-renderer (reverse slice) 5 color)
                        (add-renderer (list (car slice) (plot-item d)) 2 "gray"))
                      (set! stroke (swim-stroke d))
                      (set! color (get-swim-stroke-color stroke))
                      (set! slice (cons (plot-item d) '())))))
              data-series)

    ;; Add the last slice
    (when (pair? slice)
      (add-renderer (reverse slice) 5 color)))

  (reverse renderers))

;; Builds various plot renderers for lap swiminng sessions.  Allows setting
;; the X and Y axes plus the session and maintains a cached state to minimize
;; computations.
;;
(define swim-plot-builder%
  (class object%
    (init)
    (super-new)

    (define the-session #f)

    (define x-axis #f)
    (define y-axis #f)

    (define data-extractor #f)

    (define data-series #f)
    (define lap-markers #f)

    (define min-graph-x #f)
    (define max-graph-x #f)
    (define min-graph-y #f)
    (define max-graph-y #f)

    (define (invalidate-data-series)
      (set! data-series #f)
      (set! lap-markers #f)
      (set! min-graph-x #f)
      (set! max-graph-x #f)
      (set! min-graph-y #f)
      (set! max-graph-y #f))

    (define (prepare-data-series)
      (invalidate-data-series)
      (when (and the-session data-extractor)
        (set! data-series (data-extractor the-session))
        (set! lap-markers 
              (get-lap-markers the-session (add-verticals data-series)))
        (setup-bounding-box data-series)))

    (define (setup-bounding-box data-series)
      (set! min-graph-x #f)
      (set! max-graph-x #f)
      (set! min-graph-y #f)
      (set! max-graph-y #f)

      (for ((data (in-list data-series)))
        (let ((x (vector-ref data 1))
              (y (vector-ref data 2)))
          (set! min-graph-x (if min-graph-x (min min-graph-x x) x))
          (set! max-graph-x (if max-graph-x (max max-graph-x x) x))
          (set! min-graph-y (if min-graph-y (min min-graph-y y) y))
          (set! max-graph-y (if max-graph-y (max max-graph-y y) y)))))

    (define (get-min-graph-value)
      (cond ((and y-axis
                  (send y-axis get-y-range)
                  (car (send y-axis get-y-range)))
             (car (send y-axis get-y-range)))
            (min-graph-y)
            (#t 0)))

    (define (get-max-graph-value)
      (cond ((and y-axis 
                  (send y-axis get-y-range) 
                  (cdr (send y-axis get-y-range)))
             (cdr (send y-axis get-y-range)))
            (max-graph-y)
            (#t 10000)))

    (define (on-axis-changed)
      (invalidate-data-series)
      (set! data-extractor
            (if (and x-axis y-axis)
                (make-swim-data-extractor (send x-axis get-extractor-fn #f)
                                          (send y-axis get-extractor-fn #f)
                                          (send x-axis get-fixup-fn))
                #f)))

    (define/public (set-filter-amount a)
      ;; No filtering for swim graphs at this time
      #f)

    (define/public (set-x-axis x)
      (set! x-axis x)
      (on-axis-changed))

    (define/public (set-y-axis y)
      (set! y-axis y)
      (on-axis-changed))

    (define/public (set-secondary-y-axis y) ; not implemented
      #f)

    (define/public (set-delay-amount d) ; not implemented
      #f)

    (define/public (set-session session)
      (set! the-session session)
      (invalidate-data-series))

    (define/public (get-plot-renderer [scatter? #f])
      ;; Convert the swim data series from a [Vectorof start-time x y
      ;; swim-stroke] to an [x y]
      (define (mk-series data)
        (for/list ([d data]) (vector (vector-ref d 1) (vector-ref d 2))))
          
      ;; Determining the line color is no simple matter :-)
      (define (get-line-color)
        (let ((color (get-axis-plot-color y-axis)))
          (if (and (eq? color 'smart) scatter?)
              default-plot-color
              color)))

      (unless data-series (prepare-data-series))

      (if data-series
          (let ((color (get-line-color))
                (y-range (send y-axis get-y-range)))
            (cond (scatter? 
                   (make-scatter-renderer 
                    (mk-series data-series)
                    min-graph-x max-graph-x min-graph-y max-graph-y 
                    color))
                  ((eq? color 'smart)
                   (make-swim-coloured-renderer (add-verticals data-series)))
                  (#t 
                   (make-plot-renderer (mk-series (add-verticals data-series)) y-range color))))
          #f))

    (define/public (get-histogram-renderer bucket-width as-percentage? include-zeroes?)
      ;; Convert the swim data series from a [Vectorof start-time swim-stroke
      ;; x y] to an [x y]
      (define (mk-series data)
        (map (lambda (d)
               (vector (vector-ref d 1) (vector-ref d 2)))
             data))

      ;; Determining the line color is no simple matter :-)
      (define (get-line-color)
        (let ((color (get-axis-plot-color y-axis)))
          (if (eq? color 'smart)
              default-plot-color
              color)))

      (unless data-series (prepare-data-series))
      (if data-series
          (let ((data (make-histogram (mk-series data-series)
                                      bucket-width as-percentage? include-zeroes?)))
            (if (not data)
                ;; Can happen if BUCKET-WIDTH is large enough for all samples
                ;; to get into bucked 0 and INCLUDE-ZEROES? is #t
                #f
                (list (make-histogram-renderer data (get-line-color)))))
          #f))
      
    (define/public (get-lap-start-end lap-num)
      (unless data-series (prepare-data-series))
      (cond ((or (< lap-num 0)
                 (>= lap-num (length lap-markers)))
             #f)
            ((< lap-num (- (length lap-markers) 1))
             (cons (list-ref lap-markers lap-num)
                   (list-ref lap-markers (+ 1 lap-num))))
            (#t
             (cons (list-ref lap-markers lap-num)
                   (vector-ref (last data-series) 1)))))

    (define/public (get-plot-renderer-for-lap lap-num)
      (unless data-series (prepare-data-series))
      (if data-series
          (let ((range (get-lap-start-end lap-num))
                (color (get-swim-stroke-color (lap-swim-stroke (list-ref (session-laps the-session) lap-num)))))
            (if range
                (make-box-renderer
                 (car range) (cdr range)
                 (get-min-graph-value) (get-max-graph-value)
                 y-axis color)
                #f))
            #f))
    ))
