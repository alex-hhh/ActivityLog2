#lang racket/base

;; hrv.rkt -- extract and analyze HRV (Heart Rate Variability) data from FIT
;; files.
;;
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

;; NOTE: HRV data can be collected by most recent Garmin devices, but it needs
;; to be enabled first. There are several methods to enable this, the simples
;; is to use the ActivityLog2 "Export FIT Settings" option and tick the HRV
;; checkbox.

(require
 racket/contract
 racket/class
 racket/match
 racket/math
 racket/vector
 racket/dict
 math/statistics
 db/base
 "data-frame/df.rkt"
 "data-frame/series.rkt"
 "fit-file/fit-file.rkt"
 "database.rkt"
 )

(provide/contract
 (make-hrv-data-frame/file (-> path-string? (or/c #f data-frame?)))
 (make-hrv-data-frame/db (-> connection? number? (or/c #f data-frame?))))

(provide
 (struct-out hrv-metrics)
 compute-hrv-metrics
 put-hrv-metrics)



;;.............................................. hrv-data-frame-builder% ....

;; Construct a vector from elements in LIST in reverse.  This is used when the
;; list is constructed from incremental samples using cons, and therefore the
;; elements will be in reverse order.  This function works without making
;; unnecessary allocations.
(define (list->vector-rev list)
  (let* ((len (length list))
         (vec (make-vector len #f)))
    (for ([item list]
          [index (in-range (- len 1) -1 -1)])
      (vector-set! vec index item))
    vec))

;; Builder class to construct a HRV data frame from a FIT file.
(define hrv-data-frame-builder% 
  (class fit-event-dispatcher%
    (init)
    (init-field [start-timestamp #f] [end-timestamp #f])
    (super-new)

    (define current-bpm #f)
    (define timestamps '())
    (define hrv-samples '())
    (define bpm-samples '())

    ;; TS is "valid" if it is a number and is between start-timestamp and
    ;; end-timestamp (if they are present).  If start-timestamp or
    ;; end-timestamp are not present, TS is always valid.
    (define (valid-timestamp? ts)
      (and
       (real? ts)
       (or (not (real? start-timestamp)) (<= start-timestamp ts))
       (or (not (real? end-timestamp)) (<= ts end-timestamp))))

    (define/override (on-record data)
      (let ((bpm (dict-ref data 'heart-rate #f)))
        (when bpm
          (set! current-bpm bpm))))
    
    ;; NOTE: hrv values are in milliseconds
    (define/override (on-hrv data)
      (let ((ts (send this get-current-timestamp))
            (hrv-data (dict-ref data 0 #f)))
        (when (and current-bpm hrv-data (valid-timestamp? ts))
          (for ([hrv hrv-data] #:when hrv)
            (set! timestamps (cons ts timestamps))
            (set! hrv-samples (cons hrv hrv-samples))
            (set! bpm-samples (cons current-bpm bpm-samples))))))
    
    (define (make-df)
      (let* ((ts (list->vector-rev timestamps))
             (bpm (list->vector-rev bpm-samples))
             (hrv (list->vector-rev hrv-samples))
             (dhrv (for/vector #:length (vector-length hrv)
                       ((index (in-range (vector-length hrv))))
                     (if (> index 0)
                         (let ((prev (vector-ref hrv(- index 1)))
                               (curr (vector-ref hrv index)))
                           (abs (- curr prev)))
                         0)))
             (df (make-data-frame)))
        (df-add-series df (make-series "timestamp" #:data ts))
        (df-set-sorted df "timestamp" <=)
        (df-add-series df (make-series "bpm" #:data bpm))
        (df-add-series df (make-series "hrv" #:data hrv))
        (df-add-series df (make-series "delta-hrv" #:data dhrv))
        df))

    (define/public (get-data-frame)
      (if (equal? '() timestamps)
          #f                            ; no HRV data in the activity
          (make-df)))
    ))



;;............................................. make-hrv-data-frame/file ....

;; Read FILE, a FIT file and return a data-frame% containing the HRV and BPM
;; data.  If FILE contains a multisport activity, HRV data for the entire file
;; will be read.
(define (make-hrv-data-frame/file file)
  (let ([stream (make-fit-data-stream file)]
        [consumer (new hrv-data-frame-builder%)])
    (read-fit-records stream consumer)
    (send consumer get-data-frame)))


;;............................................... make-hrv-data-frame/db ....

(define session-info-stmt
  (virtual-statement
   (lambda (dbsys)
     "select S.activity_id as aid,
       S.start_time as start_time,
       SS.total_elapsed_time as duration
  from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
   and S.id = ?")))

(define (get-session-info db sid)
  (query-maybe-row db session-info-stmt sid))

;; Return a data-frame% containing the HRV and BPM data for the session SID.
;; The original FIT file is retrieved and parsed.
;;
;; NOTE: activities can contain more than one session, we extract the HRV data
;; for the current session only (based on the sesion start and end timestamps)
(define (make-hrv-data-frame/db db sid)
  (call/cc
   (lambda (done)
     (define sinfo (get-session-info db sid))
     (unless sinfo (done #f))
     (match-define (vector aid start duration) sinfo)
     (when (or (sql-null? aid) (sql-null? start) (sql-null? duration))
       (done #f))
     (define end (exact-ceiling (+ start duration)))
     (define data (db-extract-activity-raw-data aid db))
     (unless data (done #f))
     (define stream (make-fit-data-stream data))
     (define builder (new hrv-data-frame-builder%
                          [start-timestamp start]
                          [end-timestamp end]))
     (read-fit-records stream builder)
     (send builder get-data-frame))))


;;.......................................................... hrv-metrics ....

;; Decide if HRV is a good (plausible) given the BPM at the same datapoint.
;; We consider it plausible if the value is withing 70% of the HRV as
;; determined by the BPM value.  HRV data seems to contain missed heart beats
;; (sometimes several of them) and this results in unrealistically high HRV
;; values.  Sometimes the HRV values are unrealisticallly low.  All this data
;; affects the HRV metrics, so we filter it out.
(define (good-hrv? hrv bpm)
  (let* ((bh (/ 1000.0 (/ bpm 60.0)))   ; HRV based on BPM value
         (delta (abs (- bh hrv)))
         (pct (/ delta bh)))
    (<= pct 0.8)))

;; Holds various metrics related to HRV, see
;; https://en.wikipedia.org/wiki/Heart_rate_variability
(struct hrv-metrics
  
  (sdnn      ; STDDEV of NN intervals (hrv samples)
   rmssd     ; root mean square of successive differences (delta-hrv)
   sdsd      ; STDDEV of successive differences (delta-hrv)
   nn50      ; # of successive pairs that differ by more than 50ms (delta-hrv)
   pnn50     ; nn50 / good-samples
   nn20      ; # of successive pairs that differ by more than 20ms (delta-hrv)
   pnnn20    ; nn20 / good-samples
   good-samples                         ; # of samples where good-hrv? is #t
   bad-samples                          ; # of samples where good-hrv? is #f
   )
  #:transparent)

;; Calculate and return a HRV-METRICS instance based on HRV data in DF (a data
;; frame) between START and END.
(define (compute-hrv-metrics df #:start (start 0) #:stop (end (df-row-count df)))
  (let* ((start (or start 0))
         (end (or end (df-row-count df)))
         (hrv (let ((hrv (df-map df '("bpm" "hrv")
                                 #:start start #:stop end
                                 (lambda (val)
                                   (match-define (list bpm hrv) val)
                                   (if (good-hrv? bpm hrv) hrv #f)))))
                (vector-filter (lambda (x) (real? x)) hrv)))
         (dhrv (let ((dhrv (df-map df '("bpm" "hrv" "delta-hrv")
                                   #:start start #:stop end
                                   (lambda (val)
                                     (match-define (list bpm hrv delta-hrv) val)
                                     (if (good-hrv? bpm hrv) delta-hrv #f)))))
                 (vector-filter (lambda (x) (real? x)) dhrv)))
         (nsamples (df-row-count df))
         (good-samples (vector-length hrv))
         (bad-samples (- (df-row-count df) good-samples)))
    (let ((sdnn (statistics-stddev (update-statistics* empty-statistics hrv)))
          (sdsd (statistics-stddev (update-statistics* empty-statistics dhrv)))
          (nn50 0)
          (nn20 0)
          (dhrv-sq 0)
          (nitems (- end start))
          (delta-hrv (df-select df "delta-hrv")))
      (for ((dhrv (in-vector delta-hrv start end)))
        (set! dhrv-sq (+ dhrv-sq (* dhrv dhrv)))
        (when (>= dhrv 50) (set! nn50 (+ nn50 1)))
        (when (>= dhrv 20) (set! nn20 (+ nn20 1))))
      (hrv-metrics
       ;; Round the values, they are already in milliseconds, we won't get
       ;; extra accuracy by keeping them floating point.
       (exact-round sdnn)
       (exact-round (sqrt (/ dhrv-sq nitems)))
       (exact-round sdsd)
       nn50
       (exact->inexact (/ nn50 nitems))
       nn20
       (exact->inexact (/ nn20 nitems))
       good-samples
       bad-samples))))


;;...................................................... put-hrv-metrics ....

(define session-hrv-id-stmt
  (virtual-statement
   (lambda (dbsys) "select id from SESSION_HRV where session_id = ?")))

(define insert-session-hrv-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into SESSION_HRV(sdnn, rmssd, sdsd, nn50, nn20, 
                              good_samples, bad_samples, session_id)
      values(?, ?, ?, ?, ?, ?, ?, ?)")))

(define update-session-hrv-stmt
  (virtual-statement
   (lambda (dbsys)
     "update SESSION_HRV 
         set sdnn = ?, rmssd = ?, sdsd = ?, 
             nn50 = ?, nn20 = ?, 
             good_samples = ?, bad_samples = ?
       where session_id = ?")))

;; Store HRV METRICS for session id SID.  If SID already has a set of metrics,
;; they are updated.
(define (put-hrv-metrics metrics sid db)
  (let ((id (query-maybe-value db session-hrv-id-stmt sid)))
    (query-exec
     db
     (if id update-session-hrv-stmt insert-session-hrv-stmt)
     (hrv-metrics-sdnn metrics)
     (hrv-metrics-rmssd metrics)
     (hrv-metrics-sdsd metrics)
     (hrv-metrics-nn50 metrics)
     (hrv-metrics-nn20 metrics)
     (hrv-metrics-good-samples metrics)
     (hrv-metrics-bad-samples metrics)
     sid)))
