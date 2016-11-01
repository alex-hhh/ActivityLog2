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
         "dbapp.rkt"                    ; TODO: don't use (current-database)
         "spline-interpolation.rkt"
         "data-frame.rkt"
         "session-df.rkt")

;; WARNING: a best-avg set in this file has a different structure than the one
;; produced by df-best-avg from data-frame.rkt.

;; Compute the BEST-AVG data for a series, and convert the result to a JSEXPR
;; (this is a scheme form that can be encoded as JSON, we can only use lists
;; and hashes).
(define (best-avg/jsexpr df series (inverted? #f))
  (let ((bavg (df-best-avg df series #:inverted? inverted?))
        (sid (send df get-property 'session-id)))
    (for/list ((item bavg) #:when (vector-ref item 1))
      (match-define (vector duration value timestamp) item)
      (list sid timestamp duration (/ (exact-round (* value 100.0)) 100.0)))))

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

(define db-insert-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into BAVG_CACHE (session_id, series, data) values (?, ?, ?)")))

(define db-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update BAVG_CACHE set data = ? where id = ?")))

(define db-lookup-stmt
  (virtual-statement
   (lambda (dbsys)
     "select id from BAVG_CACHE where session_id = ? and series = ?")))

;; PUT the BAVG data into the database (BAVG_CACHE table) for the sid/series
;; key.  If an entry already exists, it is updated.
(define (db-put db sid series bavg)
  (let ((data (jsexpr->compressed-string bavg)))
    (call-with-transaction
     db
     (lambda ()
       (let ((id (query-maybe-value db db-lookup-stmt sid series)))
         (if id
             (query-exec db db-update-stmt data id)
             (query-exec db db-insert-stmt sid series data)))))))

(define db-fetch-stmt
  (virtual-statement
   (lambda (dbsys)
     "select data from BAVG_CACHE where session_id = ? and series = ?")))

;; Fetch cached BAVG data from the database.  Return #f if the data does not
;; exist.
(define (db-fetch db sid series)
  (let ((data (query-maybe-value db db-fetch-stmt sid series)))
    (and data (compressed-string->jsexpr data))))

;; A cache mapping a sid/series key to the bavg data.  Avoid
;; re-calculating/retrieving the data for subsequent calls.
(define bavg-cache (make-hash))

;; A data frame cache, to avoid reading data frames again if we need to
;; compute BAVG values for several series.  This is a two stage cache,
;; allowing us to expire old entries.  See 'session-df' on how this cache is
;; managed.
(define df-cache (make-hash))
(define df-cache2 (make-hash))

;; Number of data frames to keep in df-cache.  NOTE: total data frame count is
;; up to (hash-count df-cache) + (hash-count df-cache2), so in total number of
;; cached data frames can be up to (* 2 df-cache-limit)
(define df-cache-limit 50)

;; Return the data frame for a session id SID.  Data frames are cached in
;; memory, so retrieving the same one again should be fast.
(define (session-df db sid)
  (cond ((hash-ref df-cache sid #f)
         => (lambda (df) df))
        ((hash-ref df-cache2 sid #f)
         => (lambda (df)
              ;; Promote it to first cache
              (hash-set! df-cache sid df)
              df))
        (#t
         (let ((df (make-session-data-frame db sid)))
           (hash-set! df-cache sid df)
           (when (> (hash-count df-cache) df-cache-limit)
             ;; Cache limit reached, demote df-cache to df-cache2 (loosing old
             ;; data) and create a fresh df-cache
             (set! df-cache2 df-cache)
             (set! df-cache (make-hash)))
           df))))

;; Clear all internal caches.  This is needed whenever the database is closed
;; and a new one is opened.
(define (clear-metrics-cache)
  (set! bavg-cache (make-hash))
  (set! df-cache (make-hash))
  (set! df-cache2 (make-hash)))

;; Return the BEST-AVG data for SID + SERIES.  It is retrieved from one of the
;; caches (db or in memory) if possible, otherwise it is computed and also
;; stored in the cache.
(define (get-best-avg sid series (inverted? #f))
  (or (hash-ref bavg-cache (cons sid series) #f)
      ;; Try the database cache
      (let ((bavg (db-fetch (current-database) sid series)))
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
          (db-put (current-database) sid series bavg)
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
;; session ids) for SERIES.
(define (aggregate-bavg sids series #:inverted? (inverted? #f))
  (for/fold ((final '()))
            ((sid sids))
    (let ((bavg (get-best-avg sid series inverted?)))
      (merge-best-avg final bavg inverted?))))

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

;; Session-id, timestamp, duration , value.  This is a different layout than
;; the best-avg/c defined in data-frame.rkt
(define aggregate-bavg-item/c (list/c exact-nonnegative-integer? ; session-id
                                      exact-nonnegative-integer? ; time stamp
                                      (and/c real? positive?)    ; duration
                                      real?))                    ; value
(define aggregate-bavg/c (listof aggregate-bavg-item/c))

(provide/contract
 (fetch-candidate-sessions (-> connection?
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (or/c #f exact-nonnegative-integer?)
                               (listof exact-nonnegative-integer?)))
 (clear-metrics-cache (-> any/c))
 (aggregate-bavg (->* ((listof exact-nonnegative-integer?) string?)
                      (#:inverted? boolean?)
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
 )
