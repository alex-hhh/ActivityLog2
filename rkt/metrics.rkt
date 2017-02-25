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

(require json
         db
         file/gzip
         file/gunzip
         racket/class
         racket/match
         racket/math
         racket/contract
         racket/list
         "dbapp.rkt"                    ; TODO: don't use (current-database)
         "spline-interpolation.rkt"
         "data-frame.rkt"
         "series-meta.rkt"
         "session-df.rkt")

;; WARNING: a best-avg set and a histogram in this file has a different
;; structure than the one produced by df-best-avg and df-histogram from
;; data-frame.rkt.

;; Take a JSEXPR and return a compressed string from it.  It is intended to be
;; stored in the database.
(define (jsexpr->compressed-string jsexpr)
  (let* ((str (jsexpr->string jsexpr))
         (in (open-input-bytes (string->bytes/utf-8 str)))
         (out (open-output-bytes)))
    (gzip-through-ports in out #f 0)
    (get-output-bytes out)))

;; Convert a compressed string back into a JSEXPR
(define (compressed-string->jsexpr data)
  (let ((in (open-input-bytes data))
        (out (open-output-bytes)))
    (gunzip-through-ports in out)
    (let ((str (bytes->string/utf-8 (get-output-bytes out))))
      (string->jsexpr str))))


;;................................................................. bavg ....

;; Compute the BEST-AVG data for SERIES, and convert the result to a JSEXPR
;; (this is a scheme form that can be encoded as JSON, we can only use lists
;; and hashes).
(define (best-avg/jsexpr df series (inverted? #f))
  (let ((bavg (df-best-avg df series #:inverted? inverted?))
        (sid (send df get-property 'session-id)))
    (for/list ((item bavg) #:when (vector-ref item 1))
      (match-define (vector duration value timestamp) item)
      (list sid timestamp duration (/ (exact-round (* value 100.0)) 100.0)))))

(define db-bavg-insert-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into BAVG_CACHE (session_id, series, data) values (?, ?, ?)")))

(define db-bavg-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update BAVG_CACHE set data = ? where id = ?")))

(define db-bavg-lookup-stmt
  (virtual-statement
   (lambda (dbsys)
     "select id from BAVG_CACHE where session_id = ? and series = ?")))

;; PUT the BAVG data into the database (BAVG_CACHE table) for the sid/series
;; key.  If an entry already exists, it is updated.
(define (db-put-bavg db sid series bavg)
  (let ((data (jsexpr->compressed-string bavg)))
    (call-with-transaction
     db
     (lambda ()
       (let ((id (query-maybe-value db db-bavg-lookup-stmt sid series)))
         (if id
             (query-exec db db-bavg-update-stmt data id)
             (query-exec db db-bavg-insert-stmt sid series data)))))))

(define db-bavg-fetch-stmt
  (virtual-statement
   (lambda (dbsys)
     "select data from BAVG_CACHE where session_id = ? and series = ?")))

;; Fetch cached BAVG data from the database.  Return #f if the data does not
;; exist.
(define (db-fetch-bavg db sid series)
  (let ((data (query-maybe-value db db-bavg-fetch-stmt sid series)))
    (and data (compressed-string->jsexpr data))))

;; A cache mapping a sid/series key to the bavg data.  Avoid
;; re-calculating/retrieving the data for subsequent calls.
(define bavg-cache (make-hash))

;; Return the BEST-AVG data for SID + SERIES.  It is retrieved from one of the
;; caches (db or in memory) if possible, otherwise it is computed and also
;; stored in the cache.
(define (get-best-avg sid series (inverted? #f))
  (or (hash-ref bavg-cache (cons sid series) #f)
      ;; Try the database cache
      (let ((bavg (db-fetch-bavg (current-database) sid series)))
        (if bavg
            (begin
              (hash-set! bavg-cache (cons sid series) bavg)
              bavg)
            #f))
      ;; Get the session data frame
      (let ((df (session-df (current-database) sid)))
        (let ((bavg (if (send df contains? series)
                        (best-avg/jsexpr df series inverted?)
                        '())))
          (db-put-bavg (current-database) sid series bavg)
          (hash-set! bavg-cache (cons sid series) bavg)
          bavg))))


;; Merge two best avg sets according to whichever has the highest value
;; (unless inverted? is #f, in which case the smallest value is chosen).
(define (merge-best-avg bavg1 bavg2 (inverted? #f))
  (let loop ((result '())
             (b1 bavg1)
             (b2 bavg2))
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

;; Return a BEST-AVG set resulting from merging all sets from SIDS (a list of
;; session ids) for SERIES.  PROGRESS, if specified, is a callback that is
;; called periodically with the percent of completed sessions (a value between
;; 0 and 1)
(define (aggregate-bavg sids series #:inverted? (inverted? #f) #:progress-callback (progress #f))
  (let ((nitems (length sids)))
    (for/fold ((final '()))
              (((sid index) (in-indexed (reorder-sids sids))))
      (let ((bavg (get-best-avg sid series inverted?)))
        (when progress (progress (exact->inexact (/ index nitems))))
        (merge-best-avg final bavg inverted?)))))

;; Return the plot bounds for the aggregate best-avg set as a set of values
;; (min-x max-x min-y max-y)
(define (aggregate-bavg-bounds abavg)
  (let ((min-x #f)
        (max-x #f)
        (min-y #f)
        (max-y #f))
    (for ([item abavg])
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
(define (aggregate-bavg->spline-fn abavg)
  (define data
    (for/list ([item abavg])
      (match-define (list sid ts duration value) item)
      (vector duration value)))
  (if (> (length data) 3) (mk-spline-fn data) #f))

;; Return a heat map for an aggregate-bavg data.  For each duration in the
;; ABAVG, it contains the number of sessions which come within PCT percent of
;; the best at that duration.
;;
;; ABAVG - an aggregate best avg, as produced by `aggregate-bavg'
;;
;; PCT - percentage value used as a threshold test
;;
;; SIDS - list if session IDs to consider for the HEAT MAP
;;
;; SERIES - the name of the series for which we compute the BAVG
;;
;; INVERTED? - whether to use an inverter best avg.
;;
(define (aggregate-bavg-heat-map abavg pct sids series
                                 #:inverted? (inverted? #t) #:as-percentage? (as-percentage? #f))
  (define heat-map (make-hash))
  (for ((sid sids))
    (let ((bavg (get-best-avg sid series inverted?)))
      (let loop ((b1 abavg)
                 (b2 bavg))
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
        (for/list ((item abavg))
          (match-define (list sid ts duration value) item)
          (vector duration (exact->inexact (/ (hash-ref! heat-map duration 0) nsids)))))
      (for/list ((item abavg))
        (match-define (list sid ts duration value) item)
        (vector duration (hash-ref! heat-map duration 0)))))


;;................................................................. hist ....

;; Compute the histogram data for SERIES and convert the result to a JSEXPR.
;; We also remove keys with 0 rank to keep the data smaller (as it will be
;; stored in the database.
(define (histogram/jsexpr df series)
  (let* ((meta (find-meta-for-series series))
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
        (let ((hist (if (send df contains? series)
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


;;................................................................. rest ....

;; Fetch session IDs filtering by sport/sub-sport and within a date range.
;; The session IDs are also filtered by the 'equipment-failure' label, so we
;; don't compute best avg data off bad data.  This is a bit of a hack, but bad
;; HR data produced completely wrong bests data.
(define (fetch-candidate-sessions db sport-id sub-sport-id start-timestamp end-timestamp)
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
  and S.id not in (
   select SL.session_id
     from SESSION_LABEL SL, LABEL L
    where SL.label_id = L.id
      and L.name = 'equipment-failure')"
            (if sport-id (format "S.sport_id = ~a" sport-id) "1=1")
            (if sub-sport-id (format "S.sub_sport_id = ~a" sub-sport-id) "1=1")
            (if start-timestamp (format "S.start_time >= ~a" start-timestamp) "1=1")
            (if end-timestamp (format "S.start_time <= ~a" end-timestamp) "1=1")))
  (define result (query-list db q))
  result)

;; Clear all internal caches.  This is needed whenever the database is closed
;; and a new one is opened.
(define (clear-metrics-cache)
  (set! bavg-cache (make-hash))
  (set! hist-cache (make-hash)))

;; Session-id, timestamp, duration , value.  This is a different layout than
;; the best-avg/c defined in data-frame.rkt
(define aggregate-bavg-item/c (list/c exact-nonnegative-integer? ; session-id
                                      exact-nonnegative-integer? ; time stamp
                                      (and/c real? positive?)    ; duration
                                      real?))                    ; value
(define aggregate-bavg/c (listof aggregate-bavg-item/c))

(define aggregate-hist-item/c (list/c real? real?)) ; key, rank
(define aggregate-hist/c (listof aggregate-hist-item/c))

(provide/contract
 (fetch-candidate-sessions (-> connection?
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (listof exact-nonnegative-integer?)))
 (clear-metrics-cache (-> any/c))
 (aggregate-bavg (->* ((listof exact-nonnegative-integer?) string?)
                      (#:inverted? boolean? #:progress-callback (or/c #f (-> real? any/c)))
                      aggregate-bavg/c))
 (aggregate-bavg-bounds (-> aggregate-bavg/c
                            (values (or/c #f real?) (or/c #f real?)
                                    (or/c #f real?) (or/c #f real?))))
 (aggregate-bavg->spline-fn (-> aggregate-bavg/c
                                (or/c #f (-> real? real?))))
 (aggregate-bavg-heat-map (->* (aggregate-bavg/c
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
                        histogram/c))
 )

