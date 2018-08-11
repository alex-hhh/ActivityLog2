#lang racket/base
;; meanmax.rkt -- Mean Max calculations and plots for data frames
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         plot/utils
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/math
         "df.rkt"
         "spline.rkt")


;;............................................................. best avg ....

(define important-mean-max-durations
  (list 10 15 30 45 60 75 90 (* 2 60) (* 3 60) (* 5 60) (* 10 60) (* 15 60)
        (* 20 60) (* 30 60) (* 45 60) (* 60 60)
        (* 90 60) (* 120 60) (* 180 60)))

(define (generate-mean-max-durations start limit [growth-factor 1.05] [max-growth 300])
  (let loop ((series (list start)) (current start))
    (let ((nval (exact-round (* current growth-factor))))
      (when (< nval (+ current 5))
        (set! nval (+ 20 current)))     ; ensure min growth
      (when (> nval (+ current max-growth))
        (set! nval (+ current max-growth)))
      (if (< nval limit)
          (loop (cons nval series) nval)
          (reverse series)))))

;; Merge the durations produced by 'generate-mean-max-durations' with
;; 'important-mean-max-durations'
(define default-mean-max-durations
  (let loop ((result '())
             (fill (generate-mean-max-durations 10 (* 300 60) 1.2))
             (important important-mean-max-durations))
    (cond ((and (null? fill) (null? important))
           (reverse result))
          ((null? fill)
           (loop (cons (car important) result)
                 fill
                 (cdr important)))
          ((null? important)
           (loop (cons (car fill) result)
                 (cdr fill)
                 important))
          (#t
           (let ((f (car fill))
                 (i (car important)))
             (cond ((= f i)
                    (loop (cons f result) (cdr fill) (cdr important)))
                   ((< f i)
                    (loop (cons f result) (cdr fill) important))
                   (#t
                    (loop (cons i result) fill (cdr important)))))))))

;; Plot ticks for the mean-max plot.  Produces ticks at
;; important-mean-max-durations locations (among other places).
(define (mean-max-ticks)

  (define (->ticks duration-list)
    (for/list ([d duration-list]) (pre-tick d #t)))

  ;; Truncate VAL so it is a multiple of NEAREST.
  (define (trunc val nearest)
    (* nearest (quotient (exact-truncate val) nearest)))

  ;; Generate numbers between START and END, at least MARK-COUNT of them.
  ;; Marks will be generated at a rate that is a multiple of BASE-SKIP.  The
  ;; start position is "rounded" down to a multiple of NEAREST-START.
  (define (generate-marks start end mark-count base-skip nearest-start)
    (let ((interval (max 1 (trunc (/ (- end start) mark-count) base-skip)))
          (actual-start (trunc start nearest-start)))
      (for/list ([d (in-range actual-start end interval)]) d)))

  (define (merge c1 c2)
    (sort (remove-duplicates (append c1 c2)) <))

  (define (generate-ticks start end)
    (define candidates
      (for/list ([d important-mean-max-durations]
                 #:when (and (>= d start) (<= d end)))
        d))
    (if (>= (length candidates) 5)
        (->ticks candidates)
        (let ((marks (generate-marks start end 10 5 30)))
          (->ticks (merge candidates marks)))))

  (define (duration->string seconds [high-precision? #f])
    (define (~p x (width 0))
      (~a x #:min-width width #:align 'right #:left-pad-string "0"))
    
    (let* ((seconds (if high-precision? seconds (round seconds)))
           (h (exact-truncate (/ seconds 3600.0)))
           (m (exact-truncate (/ (- seconds (* h 3600.0)) 60.0)))
           (s (exact-truncate (- seconds (* h 3600.0) (* m 60.0))))
           (ms (exact-truncate (* 10 (- seconds s (* h 3600.0) (* m 60.0))))))
      (if high-precision? 
          (if (> h 0)
              (string-append (~p h) ":" (~p m) ":" (~p s) "." (~p ms 3))
              (string-append (~p m) ":" (~p s) "." (~p ms 3)))
          (if (> h 0)
              (string-append (~p h) ":" (~p m) ":" (~p s))
              (string-append (~p m) ":" (~p s))))))

  (define (format-ticks start end ticks)
    (for/list [(tick ticks)]
      (duration->string (pre-tick-value tick))))

  (ticks generate-ticks format-ticks))

;; Given a data series (Vectorof (Vector X Y)), compute the delta series by
;; combining adjacent samples.  The result is a (Listof (Vector Delta-X
;; Slice-Y Pos-X)), where Delta-X is the difference between two adjacent X
;; values and Slice-Y is the "area" (integral) of the slice between the two X
;; values and Pos-X is the X position in the DATA-SERIES for this slice.
(define (make-delta-series data-series)
  (for/list ([first (in-vector data-series)]
             [second (in-vector data-series 1)])
    (match-define (vector x1 y1) first)
    (match-define (vector x2 y2) second)
    (let ((dt (- x2 x1)))
      (vector dt (* dt (/ (+ y1 y2) 2)) x1))))

;; Compute the mean maximal value from a delta series (as produced by
;; MAKE-DELTA-SERIES) over DURATION.  If INVERTED? is #t, the "best" is
;; condidered the smallest value (this is usefull for pace, vertical
;; oscilation, etc.)
(define (get-mean-max delta-series duration inverted?)

  (define cmp-fn (if inverted? < >))

  (define best-total #f)
  (define mean-max-pos #f)

  (define (maybe-update total start-pos)
    (when (or (not best-total) (and total (cmp-fn total best-total)))
      (set! best-total total)
      (set! mean-max-pos start-pos)))

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

  (vector duration (if best-total (/ best-total duration) #f) mean-max-pos))

;; Construct a data series over the best average values of DATA over
;; DURATIONS.  INVERTED? is passed to get-mean-max.
(define (make-mean-max data [inverted? #f] [durations default-mean-max-durations])
  (if (< (vector-length data) 2)
      '()
      (let ((delta-series (make-delta-series data)))
        (for*/list ([d durations]
                    [b (in-value (get-mean-max delta-series d inverted?))]
                    #:when (vector-ref b 1))
          b))))

;; Compute an average in DELTA-SERIES starting at POSITION over the specified
;; DURATION.
(define (compute-avg-at-position delta-series duration position)
  (let ((xtotal 0)
        (ytotal 0))
    (for ([item delta-series] #:break (>= xtotal duration))
      (match-define (vector dx dy pos) item)
      (when (>= pos position)
        (let ((remaining (- duration xtotal)))
          (if (> remaining dx)
              (begin
                (set! xtotal (+ dx xtotal))
                (set! ytotal (+ dy ytotal)))
              (begin
                (let ((slice (/ remaining dx)))
                  (set! xtotal (+ remaining xtotal))
                  (set! ytotal (+ (* slice dy) ytotal))))))))
    (if (> xtotal 0)
        (/ ytotal xtotal)
        #f)))

;; Compute auxiliary averages on DATA-SERIES based on a MEAN-MAX graph.  For
;; each value in MEAN-MAX we compute the corresponding average in DATA-SERIES
;; (at the same position and duration).
;;
;; For example, for a power mean-max, we can compute the average cadence for
;; the segment on which the best power-duration item was computed.
(define (make-mean-max-aux data-series mean-max)
  (let ((delta-series (make-delta-series data-series)))
    (for/list ([best mean-max])
      (match-define (vector d _ p) best)
      (vector d (compute-avg-at-position delta-series d p) p))))

;; Return "mean maximal" values from data frame COLUMN, for a set of
;; durations.  For each duration in DURATION the series is searched for the
;; segment with the maximum mean value of that duration.  This can be used,
;; for example to find the best average power for some predefined intervals
;; (e.g. 5, 10 and 20 minutes)
;;
;; DF -- is the data frame
;;
;; COLUMN -- is the name of the series of which bests are calculated
;;
;; INVERTED? -- if #t, the data is minimized (best is smallest) . This is used
;; for example for Pace, or Ground Contact Time values, where smaller is
;; better.
;;
;; WEIGHT-SERIES is the name of the column to act as a base.  If it is a time
;; column, the bests are computed for time intervals, if it is a distance
;; column the bests are computed over distance intervals.
;;
;; DURATIONS is a list of values for which the bests are calculated.  For
;; example, if WEIGHT-SERIES is "elapsed" and DURATIONS contains '(300 600),
;; than the best efforts for 5 and 10 minutes are found.  If WEIGHT-SERIES is
;; "dst" and DURATIONS contains '(1000 1600), the best efforts for 1 km and 1
;; mile are calculated.
;;
;; Returns a list of items where each item is a vector of DURATION, VALUE,
;; POSITION, this is the position in the data frame where the corresponding
;; best interval starts.
(define (df-mean-max df column
                     #:inverted? (inverted? #f)
                     #:weight-series [weight "elapsed"]
                     #:durations [durations default-mean-max-durations])
  (define (filter-fn val)
    (and (vector-ref val 0) (vector-ref val 1)))
  (define data (df-select* df weight column #:filter filter-fn))
  (make-mean-max data inverted? durations))

;; Adapt the `df-mean-max` calculation for lap swimming data frames.
;;
;; The problem: Data frames for lap swim activities are special in that data
;; is **not** recorded continuously at short intervals (usually at 1 second).
;; Instead, data is recorded at the end of each length, also pauses have all
;; non-timing series set to #f.  This makes "avg" calculations (which average
;; data between two consecutive points) produce the wrong result, also #f
;; values (and thus pauses) are ignored by `df-mean-max`, resulting in higher
;; than reasonable mean-max values being produced.
;;
;; The solution: We pre-process the data before passing it on to
;; `make-mean-max`, which does the real work.  We do the following: we
;; duplicate each entry, effectively creating two data points for each length,
;; one at the start of the length, one at the end (there will be a 0 length
;; delta between consecutive lengths, but this does not affect the
;; calculations.  Also, #f values are not discarded, instead they are replaced
;; with 0.
;;
;; NOTE: this function does not need to be used of Open Water Swimming
;; activities.
(define (df-mean-max/lap-swim df column
                          #:inverted? (inverted? #f)
                          #:weight-series [weight "elapsed"]
                          #:durations [durations default-mean-max-durations])
  (define (filter-fn val) (and (vector-ref val 0) (vector-ref val 1)))
  (define data (df-select* df weight column #:filter filter-fn))
  (define ndata (make-vector (* 2 (vector-length data)) #f))
  (define prev-weight 0)
  (for (((item index) (in-indexed (in-vector data))))
    (match-define (vector weight value) item)
    (vector-set! ndata (* 2 index) (vector prev-weight (or value 0)))
    (vector-set! ndata (+ (* 2 index) 1) (vector weight (or value 0)))
    (set! prev-weight weight))
  (make-mean-max ndata inverted? durations))

;; Return average values for a second data series at the same positions as the
;; MEAN-MAX-DATA bests are determined.  This can be used, for example, to
;; determine the average cadence for each best power value obtained from
;; `df-mean-max`
(define (df-mean-max-aux df column mean-max-data
                         #:weight-series [weight "elapsed"])
  (define (filter-fn val) (and (vector-ref val 0) (vector-ref val 1)))
  (define data (df-select* df weight column #:filter filter-fn))
  (make-mean-max-aux data mean-max-data))

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

;; Return a function that will plot the MEAN-MAX data using spline
;; interpolation
(define (mean-max->spline mean-max)
  (let ((data (for/list ([e mean-max] #:when (vector-ref e 1))
                (match-define (vector d m s) e)
                (vector d m))))
    ;; need at least 3 points for spline interpolation
    (if (> (length data) 3) (spline data) #f)))

(define (transform v smin smax tmin tmax)
  (let ((p (/ (- v smin) (- smax smin))))
    (+ tmin (* p (- tmax tmin)))))

(define (inv-transform v smin smax tmin tmax)
  (let ((p (/ (- v tmin) (- tmax tmin))))
    (+ smin (* p (- smax smin)))))

;; Return the set of transformation parameters so that MEAN-MAX-AUX values map
;; onto MEAN-MAX plot (for example a 0-100 cadence range can be mapped to a
;; 0-500 watt power graph.  The returned values can be passed to `transform'
;; and `inv-transform'.
(define (get-transform-params mean-max-aux mean-max [zero-base? #t])
  (define tmin (if zero-base? 0 #f))
  (define tmax #f)
  (for ([b mean-max])
    (match-define (vector _1 value _2) b)
    (when value
      (set! tmin (if tmin (min tmin value) value))
      (set! tmax (if tmax (max tmax value) value))))
  (define smin (if zero-base? 0 #f))
  (define smax #f)
  (for ([b mean-max-aux])
    (match-define (vector _1 value _2) b)
    (when value
      (set! smin (if smin (min smin value) value))
      (set! smax (if smax (max smax value) value))))
  (values smin smax tmin tmax))

(define (mk-inverse mean-max-aux mean-max zero-base?)
  (let-values ([(smin smax tmin tmax)
                (get-transform-params mean-max-aux mean-max zero-base?)])
    (lambda (v)
      (inv-transform v smin smax tmin tmax))))

(provide mk-inverse)

(define (mk-invertible-function mean-max-aux mean-max zero-base?)
  (let-values ([(smin smax tmin tmax)
                (get-transform-params mean-max-aux mean-max zero-base?)])
    (invertible-function
     (lambda (v) (inv-transform v smin smax tmin tmax))
     (lambda (v) (transform v smin smax tmin tmax)))))

(provide mk-invertible-function)

;; Normalize (transform) the values in MEAN-MAX-AUX so that they can be
;; displayed on the MEAN-MAX plot.
(define (normalize-aux mean-max-aux mean-max [zero-base? #t])
  (define tmin (if zero-base? 0 #f))
  (define tmax #f)
  (for ([b mean-max])
    (match-define (vector _1 value _2) b)
    (when value
      (set! tmin (if tmin (min tmin value) value))
      (set! tmax (if tmax (max tmax value) value))))
  (define smin (if zero-base? 0 #f))
  (define smax #f)
  (for ([b mean-max-aux])
    (match-define (vector _1 value _2) b)
    (when value
      (set! smin (if smin (min smin value) value))
      (set! smax (if smax (max smax value) value))))
  (define (tr v) (transform v smin smax tmin tmax))
  (for/list ([data mean-max-aux])
    (match-define (vector duration value position) data)
    (if value
        (vector duration (tr value) position)
        data)))

;; Return the plot bounds for BAVG (best average data).  4 values are
;; returned, min-x, max-x, min-y, max-y.  They can be #f if there are no valid
;; values in the plot.
(define (get-mean-max-bounds bavg)
  (define min-x #f)
  (define max-x #f)
  (define min-y #f)
  (define max-y #f)
  (for ([b bavg] #:when (vector-ref b 1))
    (let ((v (vector-ref b 1))
          (d (vector-ref b 0)))
      (set! min-x (if min-x (min min-x d) d))
      (set! max-x (if max-x (max max-x d) d))
      (set! min-y (if min-y (min min-y v) v))
      (set! max-y (if max-y (max max-y v) v))))
  ;; Make the Y bounds of the plot a bit larger
  (when (and min-y max-y)
    (let ((padding (* 0.05 (- max-y min-y))))
      (set! min-y (- min-y padding))
      (set! max-y (+ max-y padding))))
  (values min-x max-x min-y max-y))

;; Create a renderer for MEAN-MAX-DATA (as produced by `df-mean-max') and
;; possibly AUX-DATA (as produced by `df-mean-max-aux`).  The line is
;; interpolated between the data points using spline interpolation.  The plot
;; will use a logarithmic scale for the X axis.
;;
;; color1 -- color to use for the best avg plot
;;
;; color2 -- color to use for the aux data plot
;;
;; zero-base? -- if true, start the Y axis at 0.
;;
(define (mean-max-renderer mean-max-data (aux-data #f)
                           #:color1 (mean-max-color #f)
                           #:color2 (aux-color #f)
                           #:zero-base? (zero-base? #f))
  (define data-fn (and mean-max-data (mean-max->spline mean-max-data)))

  (if (not data-fn)
      #f
      (let-values (((min-x max-x min-y max-y) (get-mean-max-bounds mean-max-data)))

        (define aux-fn
          (and aux-data
               (mean-max->spline (normalize-aux aux-data mean-max-data zero-base?))))

        (define data-rt
          (let ((kwd '()) (val '()))
            (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
            (add-arg '#:width 3)
            (when mean-max-color (add-arg '#:color mean-max-color))
            (keyword-apply function kwd val data-fn min-x max-x '())))

        (define aux-rt
          (if (not aux-fn)
              #f
              (let ((kwd '()) (val '()))
                (define (add-arg k v) (set! kwd (cons k kwd)) (set! val (cons v val)))
                (add-arg '#:width 3)
                (add-arg '#:style 'long-dash)
                (when aux-color (add-arg '#:color aux-color))
                (keyword-apply function kwd val aux-fn min-x max-x '()))))

        (if aux-rt (list data-rt aux-rt) data-rt))))


;;................................................. contract definitions ....

;; vector of DURATION, MAX-VALUE, START-POSITION
(define mean-max-item/c (vector/c real? real? real?))
(define mean-max/c (listof mean-max-item/c))


;;............................................................. provides ....

(provide mean-max-item/c mean-max/c)

(provide/contract
 (df-mean-max (->* (data-frame? string?)
                   (#:inverted? boolean?
                    #:weight-series string?
                    #:durations (listof real?))
                   mean-max/c))
 (df-mean-max/lap-swim (->* (data-frame? string?)
                            (#:inverted? boolean?
                             #:weight-series string?
                             #:durations (listof real?))
                            mean-max/c))
 (df-mean-max-aux (->* (data-frame? string? mean-max/c)
                       (#:weight-series string?)
                       mean-max/c))
 (mean-max-ticks (-> ticks?))
 (transform-ticks (-> ticks? (-> real? real?) ticks?))
 (get-mean-max-bounds (-> mean-max/c
                          (values (or/c #f real?) (or/c #f real?)
                                  (or/c #f real?) (or/c #f real?))))
 (mean-max-renderer (->* (mean-max/c)
                         ((or/c #f mean-max/c)
                          #:color1 (or/c #f any/c)
                          #:color2 (or/c #f any/c)
                          #:zero-base? boolean?)
                         (treeof renderer2d?)))
 (mean-max->spline (-> mean-max/c (or/c #f (-> real? real?))))
 (normalize-aux (->* (mean-max/c mean-max/c) (boolean?) mean-max/c)))
