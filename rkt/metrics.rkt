#lang racket/base
;;; metrics.rkt -- calculate aggregate metrics for activities

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

(require db/base
         racket/class
         racket/match
         racket/math
         racket/contract
         racket/list
         racket/string
         racket/format
         racket/async-channel
         math/statistics
         "dbapp.rkt"                    ; TODO: don't use (current-database)
         "dbutil.rkt"
         "data-frame/spline.rkt"
         "data-frame/meanmax.rkt"
         "data-frame/histogram.rkt"
         "data-frame/scatter.rkt"
         "data-frame/slr.rkt"
         "data-frame/df.rkt"
         "series-meta.rkt"
         "session-df.rkt"
         "utilities.rkt")

;; WARNING: a mean-max set and a histogram in this file has a different
;; structure than the one produced by df-mean-max and df-histogram from
;; data-frame.rkt.


;;................................................................. mmax ....

;; Compute the MEAN-MAX data for SERIES, and convert the result to a JSEXPR
;; (this is a scheme form that can be encoded as JSON, we can only use lists
;; and hashes).
(define (mean-max/jsexpr df series (inverted? #f))
  (let ((mmax
         (if (df-get-property df 'is-lap-swim?)
             (df-mean-max/lap-swim df series #:inverted? inverted?)
             (df-mean-max df series #:inverted? inverted?)))
        (sid (df-get-property df 'session-id)))
    (for/list ((item mmax) #:when (vector-ref item 1))
      (match-define (vector duration value timestamp) item)
      (list sid timestamp duration (/ (exact-round (* value 100.0)) 100.0)))))

(define db-mmax-insert-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into BAVG_CACHE (session_id, series, data) values (?, ?, ?)")))

(define db-mmax-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update BAVG_CACHE set data = ? where id = ?")))

(define db-mmax-lookup-stmt
  (virtual-statement
   (lambda (dbsys)
     "select id from BAVG_CACHE where session_id = ? and series = ?")))

;; PUT the MMAX data into the database (BAVG_CACHE table) for the sid/series
;; key.  If an entry already exists, it is updated.
(define (db-put-mmax db sid series mmax)
  (let ((data (jsexpr->compressed-string mmax)))
    (call-with-transaction
     db
     (lambda ()
       (let ((id (query-maybe-value db db-mmax-lookup-stmt sid series)))
         (if id
             (query-exec db db-mmax-update-stmt data id)
             (query-exec db db-mmax-insert-stmt sid series data)))))))

(define db-mmax-fetch-stmt
  (virtual-statement
   (lambda (dbsys)
     "select data from BAVG_CACHE where session_id = ? and series = ?")))

;; Fetch cached MMAX data from the database.  Return #f if the data does not
;; exist.
(define (db-fetch-mmax db sid series)
  (let ((data (query-maybe-value db db-mmax-fetch-stmt sid series)))
    (and data (compressed-string->jsexpr data))))

;; A cache mapping a sid/series key to the mmax data.  Avoid
;; re-calculating/retrieving the data for subsequent calls.
(define mmax-cache (make-hash))

;; Return the MEAN-MAX data for SID + SERIES.  It is retrieved from one of the
;; caches (db or in memory) if possible, otherwise it is computed and also
;; stored in the cache.
(define (get-mean-max sid series (inverted? #f))
  (or (hash-ref mmax-cache (cons sid series) #f)
      ;; Try the database cache
      (let ((mmax (db-fetch-mmax (current-database) sid series)))
        (if mmax
            (begin
              (hash-set! mmax-cache (cons sid series) mmax)
              mmax)
            #f))
      ;; Get the session data frame
      (let ((df (session-df (current-database) sid)))
        (let ((mmax (if (df-contains? df series)
                        (mean-max/jsexpr df series inverted?)
                        '())))
          (db-put-mmax (current-database) sid series mmax)
          (hash-set! mmax-cache (cons sid series) mmax)
          mmax))))


;; Merge two best avg sets according to whichever has the highest value
;; (unless inverted? is #f, in which case the smallest value is chosen).
(define (merge-mean-max mmax1 mmax2 (inverted? #f))
  (let loop ((result '())
             (b1 mmax1)
             (b2 mmax2))
    (cond ((and (null? b1) (null? b2))
           (reverse result))
          ((null? b1)
           (loop (cons (car b2) result) b1 (cdr b2)))
          ((null? b2)
           (loop (cons (car b1) result) (cdr b1) b2))
          (#t
           (match-let (((list sid1 ts1 dur1 val1) (car b1))
                       ((list sid2 ts2 dur2 val2) (car b2)))
             (cond ((< dur1 dur2)
                    (loop (cons (car b1) result) (cdr b1) b2))
                   ((< dur2 dur1)
                    (loop (cons (car b2) result) b1 (cdr b2)))
                   (#t                  ; same duration
                    (if (if inverted? (< val1 val2) (>= val1 val2))
                        (loop (cons (car b1) result) (cdr b1) (cdr b2))
                        (loop (cons (car b2) result) (cdr b1) (cdr b2))))))))))

;; Return a MEAN-MAX set resulting from merging all sets from SIDS (a list of
;; session ids) for SERIES.  PROGRESS, if specified, is a callback that is
;; called periodically with the percent of completed sessions (a value between
;; 0 and 1)
(define (aggregate-mmax sids series #:inverted? (inverted? #f) #:progress-callback (progress #f))
  (let ((nitems (length sids)))
    (for/fold ((final '()))
              (((sid index) (in-indexed (reorder-sids sids))))
      (let ((mmax (get-mean-max sid series inverted?)))
        (when progress (progress (exact->inexact (/ index nitems))))
        (merge-mean-max final mmax inverted?)))))

;; Return the plot bounds for the aggregate mean-max set as a set of values
;; (min-x max-x min-y max-y)
(define (aggregate-mmax-bounds ammax)
  (let ((min-x #f)
        (max-x #f)
        (min-y #f)
        (max-y #f))
    (for ([item ammax])
      (match-define (list sid ts duration value) item)
      (set! min-x (if min-x (min min-x duration) duration))
      (set! max-x (if max-x (max max-x duration) duration))
      (set! min-y (if min-y (min min-y value) value))
      (set! max-y (if max-y (max max-y value) value)))
    (when (and min-y max-y)
      (let ((padding (* 0.05 (- max-y min-y))))
        (set! min-y (- min-y padding))
        (set! max-y (+ max-y padding))))
    (values min-x max-x min-y max-y)))

;; Return a function (-> Real Real) representing a spline interpolation of the
;; aggregate best avg data.
(define (aggregate-mmax->spline-fn ammax)
  (define data
    (for/list ([item ammax])
      (match-define (list sid ts duration value) item)
      (vector duration value)))
  (if (> (length data) 3) (spline data) #f))

;; Return a heat map for an aggregate-mmax data.  For each duration in the
;; AMMAX, it contains the number of sessions which come within PCT percent of
;; the best at that duration.
;;
;; AMMAX - an aggregate best avg, as produced by `aggregate-mmax'
;;
;; PCT - percentage value used as a threshold test
;;
;; SIDS - list if session IDs to consider for the HEAT MAP
;;
;; SERIES - the name of the series for which we compute the MMAX
;;
;; INVERTED? - whether to use an inverter best avg.
;;
(define (aggregate-mmax-heat-map ammax pct sids series
                                 #:inverted? (inverted? #t) #:as-percentage? (as-percentage? #f))
  (define heat-map (make-hash))
  (for ((sid sids))
    (let ((mmax (get-mean-max sid series inverted?)))
      (let loop ((b1 ammax)
                 (b2 mmax))
        (unless (or (null? b1) (null? b2))
          (match-define (list sid1 ts1 duration1 value1) (car b1))
          (match-define (list sid2 ts2 duration2 value2) (car b2))
          (cond ((< duration1 duration2)
                 (loop (cdr b1) b2))
                ((> duration1 duration1)
                 (loop b1 (cdr b2)))
                (#t
                 (if inverted?
                     (when (<= value2 (* value1 (- 2 pct)))
                       (hash-update! heat-map duration1 add1 0))
                     (when (>= value2 (* value1 pct))
                       (hash-update! heat-map duration1 add1 0)))
                 (loop (cdr b1) (cdr b2))))))))
  (if as-percentage?
      (let ((nsids (length sids)))
        (for/list ((item ammax))
          (match-define (list sid ts duration value) item)
          (vector duration (exact->inexact (/ (hash-ref! heat-map duration 0) nsids)))))
      (for/list ((item ammax))
        (match-define (list sid ts duration value) item)
        (vector duration (hash-ref! heat-map duration 0)))))


;;................................................................. hist ....

;; Compute the histogram data for SERIES and convert the result to a JSEXPR.
;; We also remove keys with 0 rank to keep the data smaller (as it will be
;; stored in the database.
(define (histogram/jsexpr df series)
  (let* ((lap-swim? (df-get-property df 'is-lap-swim?))
         (meta (find-meta-for-series series lap-swim?))
         (bw (if meta (send meta histogram-bucket-slot) 1))
         (hist (df-histogram df series #:bucket-width bw)))
    (for/list ((item hist) #:when (> (vector-ref item 1) 0))
      (match-define (vector value rank) item)
      (list value rank))))

(define db-hist-insert-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into HIST_CACHE (session_id, series, data) values (?, ?, ?)")))

(define db-hist-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update HIST_CACHE set data = ? where id = ?")))

(define db-hist-lookup-stmt
  (virtual-statement
   (lambda (dbsys)
     "select id from HIST_CACHE where session_id = ? and series = ?")))

;; PUT the HIST data into the database (HIST_CACHE table) for the sid/series
;; key.  If an entry already exists, it is updated.
(define (db-put-hist db sid series hist)
  (let ((data (jsexpr->compressed-string hist)))
    (call-with-transaction
     db
     (lambda ()
       (let ((id (query-maybe-value db db-hist-lookup-stmt sid series)))
         (if id
             (query-exec db db-hist-update-stmt data id)
             (query-exec db db-hist-insert-stmt sid series data)))))))

(define db-hist-fetch-stmt
  (virtual-statement
   (lambda (dbsys)
     "select data from HIST_CACHE where session_id = ? and series = ?")))

;; Fetch cached HIST data from the database.  Return #f if the data does not
;; exist.
(define (db-fetch-hist db sid series)
  (let ((data (query-maybe-value db db-hist-fetch-stmt sid series)))
    (and data (compressed-string->jsexpr data))))

;; A cache mapping a sid/series key to the hist data.  Avoid
;; re-calculating/retrieving the data for subsequent calls.
(define hist-cache (make-hash))

;; Return the histogram data for SID, a session id, adn SERIES, a series name.
;; It is retrieved from one of the caches (db or in memory) if possible,
;; otherwise it is computed and also stored in the cache.
(define (get-histogram sid series)
  (or (hash-ref hist-cache (cons sid series) #f)
      ;; Try the database cache
      (let ((hist (db-fetch-hist (current-database) sid series)))
        (if hist
            (begin
              (hash-set! hist-cache (cons sid series) hist)
              hist)
            #f))
      ;; Get the session data frame
      (let ((df (session-df (current-database) sid)))
        (let ((hist (if (df-contains? df series)
                        (histogram/jsexpr df series)
                        '())))
          (db-put-hist (current-database) sid series hist)
          (hash-set! hist-cache (cons sid series) hist)
          hist))))

;; Merge two histograms by adding the ranks of identical keys and merging the
;; rest of the keys.
(define (merge-histograms h1 h2)
  ;; NOTE: we assume that the keys in h1 and h2 are sorted (as df-histogram
  ;; produces them)
  (let loop ((h1 h1)
             (h2 h2)
             (result '()))
    (cond
      ((null? h1)
       (append (reverse result) h2))
      ((null? h2)
       (append (reverse result) h1))
      (#t
       (match-let (((list k1 v1) (car h1))
                   ((list k2 v2) (car h2)))
         (let ((diff (abs (- k1 k2))))
           (cond
             ((< diff 0.000001)         ; practically equal
              (loop (cdr h1) (cdr h2) (cons (list k1 (+ v1 v2)) result)))
             ((< k1 k2)
              (loop (cdr h1) h2 (cons (car h1) result)))
             (#t
              (loop h1 (cdr h2) (cons (car h2) result))))))))))

;; Convert histogram H, as produced by AGGREGATE-HIST into a histogram as used
;; by the histogram plots from data-frame.rkt.  Histogram objects as used in
;; this file are lists of values (see the AGGREGATE-HIST/C definition) and
;; don't have any entries where the rank is 0 (to save space when storing it
;; into the database cache.  Also these histograms are always stored as "time"
;; and the bucket width is the minimum bucket width for the data
;; series. Histograms used by data-frame.rkt functions are vectors and contain
;; all buckets and have the 0 ranks (see the HISTOGRAM/C contract).
;;
;; Parameters below have similar functionality as the equivalent ones in
;; DF-HISTOGRAM
;;
;; ZEROES?, if #t will include the bucket with a value of 0
;; BWIDTH, represents the bucket width multiplier
;; ASPCT?, if #t will convert the ranks into percentages
;;
(define (expand-histogram h
                          #:include-zeroes? (zeroes? #t)
                          #:bucket-width (bwidth 1)
                          #:as-percentage? (aspct? #f))
  (define (key->bucket k) (exact-truncate (/ k bwidth)))
  (define buckets (make-hash))
  (define total 0)

  (for ([item h])
    (match-define (list key rank) item)
    (let ((bucket (key->bucket key)))
      (when (or zeroes? (not (zero? bucket)))
        (hash-update! buckets (key->bucket key)
                      (lambda (old) (+ old rank))
                      0)
        (set! total (+ total rank)))))

  (unless (> total 0) (set! total 1))   ; avoid division by 0

  (define keys (sort (hash-keys buckets) <))

  (if (> (length keys) 0)
      (let ([min (first keys)]
            [max (last keys)])
        (for/vector #:length (+ 1 (- max min))
                    ([bucket (in-range min (add1 max))])
          (vector (* bucket bwidth)
                  (let ((val (hash-ref buckets bucket 0)))
                    (if aspct? (* 100.0 (/ val total)) val)))))
      #f))

;; Return a histogram set resulting from merging all sets from SIDS (a list of
;; session ids) for SERIES.  PROGRESS, if specified, is a callback that is
;; called periodically with the percent of completed sessions (a value between
;; 0 and 1).
;;
;; NOTE that the returned histogram is not compatible with histogram plotting
;; functions from data-frame.rkt.  See EXPAND-HISTOGRAM, on how to convert the
;; returned histogram into one that can be used for this purpose.
(define (aggregate-hist sids series #:progress-callback (progress #f))
  (let ((nitems (length sids)))
    (for/fold ((final '()))
              (((sid index) (in-indexed (reorder-sids sids))))
      (let ((hist (get-histogram sid series)))
        (when progress (progress (exact->inexact (/ index nitems))))
        (merge-histograms final hist)))))


;.............................................................. scatter ....

;; Compute the scatter data for SERIES1 and SERIES2 and convert the result to
;; a JSEXPR (an expression that can be serialized to JSON).  The result is a
;; list of (x y rank) where rank is the number of points at the X, Y location.
(define (scatter/jsexpr df series1 series2)

  (define is-lap-swim? (df-get-property df 'is-lap-swim?))
  
  (define (extract)
    (if (df-contains? df series1 series2)
        (df-select* df series1 series2 #:filter valid-only)
        #f))

  (define (group data)
    (let ((meta1 (find-meta-for-series series1 is-lap-swim?))
          (meta2 (find-meta-for-series series2 is-lap-swim?)))
      (let ((gdata (group-samples data
                                  (send meta1 fractional-digits)
                                  (send meta2 fractional-digits))))
        gdata)))

  (define (flatten data)
    (for*/list ((rank (in-hash-keys data))
                (item (in-list (hash-ref data rank #f))))
      (match-define (vector x y) item)
      (list (exact->inexact x) (exact->inexact y) rank)))

  (let ((data (extract)))
    (if data
        (let ((groupped (group data)))
          (let ((flat (flatten groupped)))
            flat))
        '())))

(define db-scatter-insert-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into SCATTER_CACHE (session_id, series1, series2, data) values (?, ?, ?, ?)")))

(define db-scatter-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update SCATTER_CACHE set data = ? where id = ?")))

(define db-scatter-lookup-stmt
  (virtual-statement
   (lambda (dbsys)
     "select id from SCATTER_CACHE where session_id = ? and series1 = ? and series2 = ?")))

;; PUT the SCATTER data into the database (SCATTER_CACHE table) for the
;; sid/series1/series2 key.  If an entry already exists, it is updated.
(define (db-put-scatter db sid series1 series2 scatter)
  (let ((data (jsexpr->compressed-string scatter)))
    (call-with-transaction
     db
     (lambda ()
       (let ((id (query-maybe-value db db-scatter-lookup-stmt sid series1 series2)))
         (if id
             (query-exec db db-scatter-update-stmt data id)
             (query-exec db db-scatter-insert-stmt sid series1 series2 data)))))))

(define db-scatter-fetch-stmt
  (virtual-statement
   (lambda (dbsys)
     "select data from SCATTER_CACHE where session_id = ? and series1 = ? and series2 = ?")))

;; Fetch cached SCATTER data from the database.  Return #f if the data does
;; not exist.
(define (db-fetch-scatter db sid series1 series2)
  (let ((data (query-maybe-value db db-scatter-fetch-stmt sid series1 series2)))
    (and data (compressed-string->jsexpr data))))

;; A cache mapping a sid/series key to the scatter data.  Avoid
;; re-calculating/retrieving the data for subsequent calls.
(define scatter-cache (make-hash))

;; Return the histogram data for SID, a session id, adn SERIES, a series name.
;; It is retrieved from one of the caches (db or in memory) if possible,
;; otherwise it is computed and also stored in the cache.
(define (get-scatter sid series1 series2)
  (or (hash-ref scatter-cache (vector sid series1 series2) #f)
      ;; Try the database cache
      (let ((scatter (db-fetch-scatter (current-database) sid series1 series2)))
        (if scatter
            (begin
              (hash-set! scatter-cache (vector sid series1 series2) scatter)
              scatter)
            #f))
      ;; Get the session data frame
      (let ((df (session-df (current-database) sid)))
        (let ((scatter (if (df-contains? df series1 series2)
                           (scatter/jsexpr df series1 series2)
                           '())))
          (db-put-scatter (current-database) sid series1 series2 scatter)
          (hash-set! scatter-cache (vector sid series1 series2) scatter)
          scatter))))

;; Helper function to aggregate scatter plots for different sessions.
;; SCATTER-HIST contains the already aggregated data and the function will add
;; the data in SCATTER-JSEXPR to it.  Use `scatter-data->scatter-group' to
;; obtain the final histogram data.
;;
;; FRAC-DIGITS1, FRAC-DIGITS2 represent the relevant fractional digits to keep
;; for the values in each series.  Values are truncated to their respective
;; fractional digits, so that they can be grouped together, reducing the
;; number of actual points.
(define (add-scatter-data scatter-hist scatter-jsexpr frac-digits1 frac-digits2)

  (define mult1 (expt 10 frac-digits1))
  (define mult2 (expt 10 frac-digits2))

  (for ([item scatter-jsexpr])
    (match-define (list x y rank) item)
    (define key (cons
                 (exact-round (* x mult1))
                 (exact-round (* y mult2))))
    (hash-set! scatter-hist
               key
               (+ rank (hash-ref scatter-hist key 0))))

  scatter-hist)


;; Convert SCATTER-HIST, which was used to aggregate histograms into a grouped
;; histogram for general consumption.
(define (scatter-data->scatter-group scatter-hist frac-digits1 frac-digits2)
  (define inv1 (expt 10 (- frac-digits1)))
  (define inv2 (expt 10 (- frac-digits2)))

  (define result (make-hash))

  (for ([key+value (in-hash-pairs scatter-hist)])
    (match-define (cons s1 s2) (car key+value))
    (define rank (cdr key+value))
    (hash-set! result
               rank
               (cons (vector (* s1 inv1) (* s2 inv2))
                     (hash-ref result rank '()))))

  result)

;; Return an aggregate scatter plot for all sessions in SIDS, plotting SERIES1
;; vs SERIES2.  The plot groups identical pairs in ranks (the number of
;; identical pairs) and returns a hash mapping the rank to a list of items
;; with that rank.  The result can be used directly by the plotting function
;; `make-scatter-group-renderer'
(define (aggregate-scatter sids series1 series2 #:progress-callback (progress #f))
  (let* ((nitems (length sids))
         (meta1 (find-meta-for-series series1))
         (meta2 (find-meta-for-series series2))
         (frac-digits1 (send meta1 fractional-digits))
         (frac-digits2 (send meta2 fractional-digits))
         (scatter-hist (make-hash)))
    (for (((sid index) (in-indexed (reorder-sids sids))))
      (let ((scatter (get-scatter sid series1 series2)))
        (when progress (progress (exact->inexact (/ index nitems))))
        (add-scatter-data scatter-hist scatter frac-digits1 frac-digits2)))
    (scatter-data->scatter-group scatter-hist frac-digits1 frac-digits2)))

;; Calculate the scatter plot bounds for SCATTER-GROUP (a
;; aggregate-scatter/c).  Returns a vector of (minx maxx miny maxy)
;; representing the min and max values for the data.
;;
;; X-DIGITS and Y-DIGITS represent the precision of the values in the scatter
;; group, it is used to align the bounds so that values at the edge are not
;; cut out.
(define (aggregate-scatter-bounds scatter-group x-digits y-digits)
  ;; TODO: there is a similar function in inspect-scatter.rkt, but that works
  ;; on a data series, instead of a scatter group hash.  We could refactor
  ;; them.

  (define (good-or-false num)
    (and (number? num) (not (nan? num)) (not (infinite? num)) num))

  (define (round n digits)
    (* (exact-round (* n (expt 10 digits))) (expt 10 (- digits))))

  (let ((xmin #f)
        (xmax #f)
        (ymin #f)
        (ymax #f))
    (for* ([value (in-hash-values scatter-group)]
           [item (in-list value)])
      (match-define (vector x y) item)
      (set! xmin (if xmin (min xmin x) x))
      (set! xmax (if xmax (max xmax x) x))
      (set! ymin (if ymin (min ymin y) y))
      (set! ymax (if ymax (max ymax y) y)))

    ;; Round the min and max to the actual digits, so we don't cut them out in
    ;; case we have values along the boundary
    (set! xmin (and xmin (round xmin x-digits)))
    (set! xmax (and xmax (round xmax x-digits)))
    (set! ymin (and ymin (round ymin y-digits)))
    (set! ymax (and ymax (round ymax y-digits)))

    (define xrange (if (and xmin xmax) (- xmax xmin) #f))
    (define yrange (if (and ymin ymax) (- ymax ymin) #f))

    (when xrange
      (when xmin (set! xmin (- xmin (* xrange 0.05))))
      (when xmax (set! xmax (+ xmax (* xrange 0.05)))))
    (when yrange
      (when ymin (set! ymin (- ymin (* yrange 0.05))))
      (when ymax (set! ymax (+ ymax (* yrange 0.05)))))
    (vector
       (good-or-false xmin)
       (good-or-false xmax)
       (good-or-false ymin)
       (good-or-false ymax))))

;; Return a set of bounds for SCATTER-GROUP (a aggregate-scatter/c), such that
;; values outside Q .. (1 - Q) are left out side the bounds.  For example, if
;; q is 0.01, the bounds will contain values in the 1 to 99 quantile.
;;
;; Returns a vector of (minx maxx miny maxy) representing the min and max
;; values for the data.
(define (aggregate-scatter-bounds/quantile scatter-group q)
  ;; TODO: there is a similar function in inspect-scatter.rkt, but that works
  ;; on a data series, instead of a scatter group hash.  We could refactor
  ;; them.
  (if (= (hash-count scatter-group) 0)
      (vector #f #f #f #f)
      (let ((xs '())
            (ys '())
            (ws '()))
        (for* ([key+value (in-hash-pairs scatter-group)]
               [item (in-list (cdr key+value))])
          (match-define (vector x y) item)
          (set! xs (cons x xs))
          (set! ws (cons (car key+value) ws))
          (set! ys (cons y ys)))
        (let* ((xmin (quantile q < xs ws))
               (xmax (quantile (- 1 q) < xs ws))
               (ymin (quantile q < ys ws))
               (ymax (quantile (- 1 q) < ys ws))
               (xrange (- xmax xmin))
               (yrange (- ymax ymin)))
          (set! xmin (- xmin (* xrange 0.05)))
          (set! xmax (+ xmax (* xrange 0.05)))
          (set! ymin (- ymin (* yrange 0.05)))
          (set! ymax (+ ymax (* yrange 0.05)))
          (vector xmin xmax ymin ymax)))))

;; Calculate simple linear regression parameters for SCATTER-GROUP (a
;; aggregate-scatter/c).  We just expand the SCATTER-GROUP and call MAKE-SLR.
(define (aggregate-scatter-slr scatter-group)
  ;; TODO: when computing scatter plots, we need both the bounds and SLR data.
  ;; We currently expand the SCATTER-GROUP both here and in
  ;; 'aggregate-scatter-bounds/quantile' to further process it, resulting in
  ;; duplicate work and memory use.  Perhaps we can optimize this, especially
  ;; since scatter plots already take a significant time to compute and
  ;; render.
  (if (= (hash-count scatter-group) 0)
      #f
      (let ((xs '())
            (ys '())
            (ws '()))
        (for* ([key+value (in-hash-pairs scatter-group)]
               [item (in-list (cdr key+value))])
          (match-define (vector x y) item)
          (set! xs (cons x xs))
          (set! ws (cons (car key+value) ws))
          (set! ys (cons y ys)))
        (make-slr xs ys ws))))


;;................................................................. rest ....

;; Fetch session IDs filtering by sport/sub-sport and within a date range.
;;
;; #:label-ids is a list of LABEL.id values, if present, only sessions with
;; ALL the specified labels are selected.
;;
;; #:equipment-ids is a list of EQUIPMENT.id values, if present, only sessions
;; with ALL the specified equipment are selected.
;;
;; The session IDs are also filtered by the 'equipment-failure' label, so we
;; don't compute best avg data off bad data.  This is a bit of a hack, but bad
;; HR data produced completely wrong bests data.
(define (fetch-candidate-sessions db sport-id sub-sport-id start-timestamp end-timestamp
                                  #:label-ids (labels #f)
                                  #:equipment-ids (equipment #f))
  ;; NOTE: it would be nice to be able to use parameters instead of
  ;; constructing the SQL using format, however, I'm not sure how to write
  ;; such a query (e.g. when sub-sport-id is #f, we want to select *ALL* sub
  ;; sports for the sport, not just the ones with NULL, same for sport-id,
  ;; start and end timestamps)
  (define q
    (format "
select S.id
from A_SESSION S
where ~a
  and ~a
  and ~a
  and ~a
  and ~a
  and ~a
  and S.id not in (
   select SL.session_id
     from SESSION_LABEL SL, LABEL L
    where SL.label_id = L.id
      and L.name = 'equipment-failure')"
            (if sport-id (format "S.sport_id = ~a" sport-id) "1=1")
            (if sub-sport-id (format "S.sub_sport_id = ~a" sub-sport-id) "1=1")
            (if start-timestamp (format "S.start_time >= ~a" start-timestamp) "1=1")
            (if end-timestamp (format "S.start_time <= ~a" end-timestamp) "1=1")
            (if (pair? labels)
                (let ((labels-str (string-join (map ~a (sort labels <)) ", ")))
                  ;; NOTE: We want to select sessions that have ALL the labels
                  ;; listed in LABELS.  To do this, we use a group by than
                  ;; concatenate the equipment values in a string and compare
                  ;; against another string.  This works only if the equipment
                  ;; IDs are sorted before being passed to group_concat().
                  ;; This seems to be the case, but I believe it is a side
                  ;; effect of the SQLite query planner, not an explicit SQL
                  ;; statement, so it may break in the future.
                  (format "S.id in (
select X.session_id
  from (select session_id,
               group_concat(label_id, ', ') as test
          from SESSION_LABEL
         where label_id in (~a)
         group by session_id) as X
 where X.test = '~a')" labels-str labels-str))
                "1 = 1")
            (if (pair? equipment)
                (let ((equipment-str (string-join (map ~a (sort equipment <)) ", ")))
                  ;; NOTE: We want to select sessions that have ALL the
                  ;; equipment listed in EQUIPMENT.  To do this, we use a
                  ;; group by than concatenate the equipment values in a
                  ;; string and compare against another string.  This works
                  ;; only if the equipment IDs are sorted before being passed
                  ;; to group_concat().  This seems to be the case, but I
                  ;; believe it is a side effect of the SQLite query planner,
                  ;; not an explicit SQL statement, so it may break in the
                  ;; future.
                  (format "S.id in (
select X.session_id
  from (select session_id,
               group_concat(equipment_id, ', ') as test
          from EQUIPMENT_USE
         where equipment_id in (~a)
         group by session_id) as X
 where X.test = '~a')" equipment-str equipment-str))
                "1 = 1")
            ))
  (define result (query-list db q))
  result)

;; Clear all internal caches.  This is needed whenever the database is closed
;; and a new one is opened.
(define (clear-metrics-cache)
  (set! mmax-cache (make-hash))
  (set! hist-cache (make-hash))
  (set! scatter-cache (make-hash)))

(define (clear-saved-metrics-for-series series)
  (query-exec (current-database)
              "delete from BAVG_CACHE where series = ?" series)
  (query-exec (current-database)
              "delete from HIST_CACHE where series = ?" series)
  (query-exec (current-database)
              "delete from SCATTER_CACHE where series1 = ? or series2 = ?" series series))

;; Clear the metrics that depend on the measurement system, these will have to
;; be recomputed.  This is a somewhat expensive operation (and will result in
;; the metrics having to be recomputed once they are required again).
;; Hopefully, the user will not switch measurement system too often.
(define (clear-ms-dependent-metrics)
  (call-with-transaction
   (current-database)
   (lambda ()
     (clear-saved-metrics-for-series "pace")
     (clear-saved-metrics-for-series "speed")
     (clear-saved-metrics-for-series "stride")
     ;; (clear-saved-metrics-for-series "alt")
     ;; (clear-saved-metrics-for-series "calt")
     ;; (clear-saved-metrics-for-series "vosc")
     (clear-metrics-cache))))

;; Clear all saved and cached metrics for a session identified by SID.  This
;; is done when the session data has been changed and therefore the metrics
;; might become invalid.
(define (clear-metrics-for-sid sid)
  (query-exec (current-database) "delete from BAVG_CACHE where session_id = ?" sid)
  (query-exec (current-database) "delete from HIST_CACHE where session_id = ?" sid)
  (query-exec (current-database) "delete from SCATTER_CACHE where session_id = ?" sid)
  (hash-remove! mmax-cache sid)
  (hash-remove! hist-cache sid)
  (hash-remove! scatter-cache sid))

(define dummy
  (let ((s (make-log-event-source)))
    (thread/dbglog
     #:name "session df change processor"
     ;; NOTE there might be multithreading race conditions here...
     (lambda ()
       (let loop ((item (async-channel-get s)))
         (when item
           (match-define (list tag data) item)
           (case tag
             ((measurement-system-changed) (clear-ms-dependent-metrics))
             ((session-updated-data) (clear-metrics-for-sid data)))
           (loop (async-channel-get s))))))))

;; Session-id, timestamp, duration , value.  This is a different layout than
;; the mean-max/c defined in data-frame.rkt
(define aggregate-mmax-item/c (list/c exact-nonnegative-integer? ; session-id
                                      real?                      ; time stamp
                                      (and/c real? positive?)    ; duration
                                      real?))                    ; value
(define aggregate-mmax/c (listof aggregate-mmax-item/c))

(define aggregate-hist-item/c (list/c real? real?)) ; key, rank
(define aggregate-hist/c (listof aggregate-hist-item/c))

(define aggregate-scatter/c (hash/c exact-positive-integer?
                                    (listof (vector/c number? number?))))

(define bounds/c (vector/c (or/c #f number?)
                           (or/c #f number?)
                           (or/c #f number?)
                           (or/c #f number?)))

(provide
 aggregate-mmax/c aggregate-mmax-item/c)

(provide/contract
 (fetch-candidate-sessions (->* (connection?
                                 (or/c #f exact-nonnegative-integer?)
                                 (or/c #f exact-nonnegative-integer?)
                                 (or/c #f exact-nonnegative-integer?)
                                 (or/c #f exact-nonnegative-integer?))
                                (#:label-ids (or/c #f (listof exact-nonnegative-integer?))
                                 #:equipment-ids (or/c #f (listof exact-nonnegative-integer?)))
                                (listof exact-nonnegative-integer?)))
 (clear-metrics-cache (-> any/c))
 (aggregate-mmax (->* ((listof exact-nonnegative-integer?) string?)
                      (#:inverted? boolean? #:progress-callback (or/c #f (-> real? any/c)))
                      aggregate-mmax/c))
 (aggregate-mmax-bounds (-> aggregate-mmax/c
                            (values (or/c #f real?) (or/c #f real?)
                                    (or/c #f real?) (or/c #f real?))))
 (aggregate-mmax->spline-fn (-> aggregate-mmax/c
                                (or/c #f (-> real? (or/c #f real?)))))
 (aggregate-mmax-heat-map (->* (aggregate-mmax/c
                                (and/c real? positive?)
                                (listof exact-nonnegative-integer?)
                                string?)
                               (#:inverted? boolean? #:as-percentage? boolean?)
                               (listof (vector/c (and/c real? positive?) (and/c real? positive?)))))
 (aggregate-hist (->* ((listof exact-nonnegative-integer?) string?)
                      (#:progress-callback (or/c #f (-> real? any/c)))
                      aggregate-hist/c))
 (expand-histogram (->* (aggregate-hist/c)
                        (#:include-zeroes? boolean?
                         #:bucket-width (and/c real? positive?)
                         #:as-percentage? boolean?)
                        (or/c #f histogram/c)))
 (aggregate-scatter (->* ((listof exact-nonnegative-integer?) string? string?)
                         (#:progress-callback (or/c #f (-> real? any/c)))
                         aggregate-scatter/c))
 (aggregate-scatter-bounds/quantile (-> aggregate-scatter/c positive? bounds/c))
 (aggregate-scatter-bounds (-> aggregate-scatter/c number? number? bounds/c))
 (aggregate-scatter-slr (-> aggregate-scatter/c slr?)))

