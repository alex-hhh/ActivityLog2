#lang racket/base
;; time-in-zone.rkt -- time spent in each sport zone for a session
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

(require db/base
         racket/class
         racket/match
         racket/math
         math/statistics
         "utilities.rkt"
         "data-frame/df.rkt"
         "data-frame/statistics.rkt"
         "data-frame/histogram.rkt"
         "session-df.rkt"
         "widgets/main.rkt"
         "hrv.rkt"
         "dialogs/edit-session-tss.rkt")

(provide update-time-in-zone-data)
(provide interactive-update-time-in-zone-data)

(define (decoupling df s1 s2 #:start (start 0) #:stop (stop (df-row-count df)))
  (let ((half-point (exact-truncate (/ (+ start stop) 2))))
    (let ((stat-s1-1 (df-statistics df s1 #:start start #:stop half-point))
          (stat-s1-2 (df-statistics df s1 #:start half-point #:stop stop))
          (stat-s2-1 (df-statistics df s2 #:start start #:stop half-point))
          (stat-s2-2 (df-statistics df s2 #:start half-point #:stop stop)))
      (and stat-s1-1 stat-s1-2 stat-s2-1 stat-s2-2
           (let ((r1 (/ (statistics-mean stat-s1-1)
                        (statistics-mean stat-s2-1)))
                 (r2 (/ (statistics-mean stat-s1-2)
                        (statistics-mean stat-s2-2))))
             (* 100.0 (/ (- r1 r2) r1)))))))

(define (decoupling/laps df s1 s2)
  (let* ((laps (df-get-property df 'laps))
         (limit (vector-length laps)))
    (for/list ([idx (in-range 0 (vector-length laps))])
      (define start
        (df-index-of df "timestamp" (vector-ref laps idx)))
      (define stop
        (if (< idx (sub1 limit))
            (df-index-of df "timestamp" (vector-ref laps (+ idx 1)))
            (df-row-count df)))
      ;; don't compute decoupling for small intervals
      (if (and start stop (> (- stop start) 60))
          (decoupling df s1 s2 #:start start #:stop stop)
          0))))

;; Compute the time spend in each zone for SESSION (as returned by
;; db-fetch-session).  The zones are defined by the AXIS-DEF (can be
;; axis-hr-zone, axis-power-zone) and are defined in "series-meta.rkt"
;;
;; Returns a (Vectorof (Vector Zone Duration)), or #f if no zone data can be
;; calculated.  We cannot calculate zone data either if no zones are defined
;; for this session, or the session does not have the resquired data.
(define (time-in-zone session series)
  (df-histogram session series #:bucket-width 1 #:as-percentage? #f))

(define delete-tiz-stmt
  (virtual-statement
   (lambda (dbsys)
     "delete from TIME_IN_ZONE where session_id = ? and sport_zone_id = ?")))

(define insert-tiz-stmt
  (virtual-statement
   (lambda (dbsys)
     "insert into TIME_IN_ZONE(session_id, sport_zone_id, zone_id, duration)
         values(?, ?, ?, ?)")))

;; Store time in zone data in the TIME_IN_ZONE table in the database.  SID is
;; the session id, ZID is the zone definition id, DATA is what
;; `time-in-hr-zone' or `time-in-power-zone' returns and DB is the database.
(define (store-time-in-zone sid zid data db)
  (call-with-transaction
   db
   (lambda ()
     (query-exec db delete-tiz-stmt sid zid)
     (for ([zd data])
       (match-define (vector zone-id duration) zd)
       (query-exec db insert-tiz-stmt sid zid zone-id duration)))))

(define select-zone-id-stmt
  (virtual-statement
   (lambda (dbsys)
     "select zone_id from V_SPORT_ZONE_FOR_SESSION
    where session_id = ? and zone_metric_id = ?")))

;; Find the definition id that corresponds to SID, a session ID.  METRIC-ID is
;; 1 for heart rate and 3 for power (see the E_ZONE_METRIC table in the
;; database schema)
(define (get-zone-id sid metric-id db)
  (query-maybe-value db select-zone-id-stmt sid metric-id))

(define update-adec-session-stmt
  (virtual-statement
   (lambda (dbsys)
     "update SECTION_SUMMARY set aerobic_decoupling = ?
where id = (select summary_id from A_SESSION where id = ?)")))

(define (update-aerobic-decoupling-for-session db sid adec)
  (query-exec db update-adec-session-stmt (or adec sql-null) sid))

(define update-adec-laps-stmt
  (virtual-statement
   (lambda (dbsys)
     "update SECTION_SUMMARY set aerobic_decoupling = ?
where id = (select summary_id from A_LAP where id = ?)")))

(define (update-aerobic-decoupling-for-lap db lapid adec)
  (query-exec db update-adec-laps-stmt (or adec sql-null) lapid))

(define fetch-lap-ids
  (virtual-statement
   (lambda (dbsys)
     "
select P.id
  from A_LAP P
 where P.session_id = ?
 order by P.start_time")))
  
;; Update the TIME_IN_ZONE table with data for a session.  Both heart rate and
;; power zones are updated (if available).  Previous data for this session is
;; deleted.
(define (update-time-in-zone-data sid db)
  (let* ((session (session-df db sid))
         (sport (df-get-property session 'sport))
         (pwr-zone-id (get-zone-id sid 3 db))
         (hr-zone-id (get-zone-id sid 1 db)))

    ;; Time in zone
    (when (df-contains? session "pwr-zone")
      (let ((data (time-in-zone session "pwr-zone")))
        (when data
          (store-time-in-zone sid pwr-zone-id data db))))
    (when (df-contains? session "hr-zone")
      (let ((data (time-in-zone session "hr-zone")))
        (when data
          (store-time-in-zone sid hr-zone-id data db))))

    ;; Training Stress Scrore
    (maybe-update-session-tss sid session db)

    ;; Aerobic Decoupling
    (cond
      ((and (equal? (vector-ref sport 0) 2) ; bike
            (df-contains? session "pwr" "hr"))
       (let ((adec (decoupling session "pwr" "hr")))
         (update-aerobic-decoupling-for-session db sid adec))
       (let ((adec/laps (decoupling/laps session "pwr" "hr"))
             (id/laps (query-list db fetch-lap-ids sid)))
         (for ([adec adec/laps]
               [lapid id/laps])
           (update-aerobic-decoupling-for-lap db lapid adec))))
      ((and (equal? (vector-ref sport 0) 1) ; run
            (df-contains? session "spd" "hr"))
       (let ((adec (decoupling session "spd" "hr")))
         (update-aerobic-decoupling-for-session db sid adec))
       (let ((adec/laps (decoupling/laps session "spd" "hr"))
             (id/laps (query-list db fetch-lap-ids sid)))
         (for ([adec adec/laps]
               [lapid id/laps])
           (update-aerobic-decoupling-for-lap db lapid adec)))))

    ;; Hrv
    (define hrv (make-hrv-data-frame/db db sid))
    (when hrv
      (define metrics (compute-hrv-metrics hrv))
      (when metrics
        (put-hrv-metrics metrics sid db)))
    
    ))

;; Update the time in zone information for all sessions in the database.
;; Provides a progress bar and allows the user to cancel the operation.
(define (interactive-update-time-in-zone-data database [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update metrics"]
         [icon (sql-export-icon)]))

  (define (task progress-dialog)
    (send progress-dialog set-message "Fetching list of sessions...")
    (define sessions (query-list database "select id from A_SESSION"))
    (define num-sessions (length sessions))
    (send progress-dialog set-message "Starting update...")
    (dbglog "interactive-update-time-in-zone-data started")
    (for ([sid sessions]
          [n (in-range num-sessions)])
      #:break (let ((progress (exact-round (* 100 (/ (+ 1 n) num-sessions)))))
                (not (send progress-dialog set-progress progress)))
      (with-handlers
        (((lambda (e) #t)
          (lambda (e)                   ; log the exception, than propagate it
            (dbglog "while updating session ~a: ~a" sid e)
            (raise e))))
        (update-time-in-zone-data sid database)))
    (dbglog "interactive-update-time-in-zone-data complete"))
  
  (send progress-dialog run parent-window task))

