#lang racket/base
;; time-in-zone.rkt -- time spent in each sport zone for a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018-2021, 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base
         racket/class
         racket/contract
         racket/match
         racket/runtime-path
         "../dbutil.rkt"
         "../session-df/hrv.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "aerobic-decoupling.rkt"
         "tss.rkt")

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

(define delete-all-tiz-stmt
  (virtual-statement
   (lambda (dbsys)
     "delete from TIME_IN_ZONE where session_id = ?")))

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

;; Update some derived metrics for the session SID.  The following are
;; updated:
;;
;; * TIME_IN_ZONE data (previous one, if exists, is deleted first)
;;
;; * Session TSS (only if the session does not already have one)
;;
;; * The Aerobic Decoupling field for the entire session and each individual
;;   lap
;;
;; * HRV for the entire session, if the session has HRV data,
;;
;; NOTE: This function is called for sessions that have been freshly imported,
;; and should probably be moved in import.rkt.
;;
(define (update-some-session-metrics sid db sport-charms)
  (let* ((session (session-df db sid))
         (sport (df-get-property session 'sport))
         (pwr-zone-id (get-zone-id sid 3 db))
         (hr-zone-id (get-zone-id sid 1 db)))

    ;; Delete any Previous time in zone data first, note that the TIZ might
    ;; change because the session might have been assigned a new set of zones
    ;; (when zone validity dates have changed)
    (query-exec db delete-all-tiz-stmt sid)

    ;; Time in zone
    (when (df-contains? session "pwr-zone")
      (let ((data (time-in-zone session "pwr-zone")))
        (when data
          (store-time-in-zone sid pwr-zone-id data db))))
    (when (df-contains? session "hr-zone")
      (let ((data (time-in-zone session "hr-zone")))
        (when data
          (store-time-in-zone sid hr-zone-id data db))))

    ;; Training Stress Score
    (maybe-update-session-tss sid session db sport-charms)

    ;; Aerobic Decoupling
    (let ([adec (aerobic-decoupling session)])
      (update-aerobic-decoupling-for-session db sid adec))
    (for ([adec (in-list (aerobic-decoupling/laps session))]
          [lapid (in-list (query-list db fetch-lap-ids sid))])
      (update-aerobic-decoupling-for-lap db lapid adec))

    ;; Hrv
    (define hrv (make-hrv-data-frame/db db sid))
    (when hrv
      (define metrics (compute-hrv-metrics hrv))
      (when metrics
        (put-hrv-metrics metrics sid db)))

    ))



(define-runtime-path tiz-outdated-query-file "../../sql/queries/tiz-outdated.sql")
(define tiz-outdated-sql (define-sql-statement tiz-outdated-query-file))

;; Return a list of session ID's that had their sport zones changed when we
;; changed validity times for the sport zones.  The TIME_IN_ZONE data for
;; these sessions will have to be updated.
(define (get-tiz-outdated-sessions db)
  (query-list db (tiz-outdated-sql)))



;;............................................................. provides ....

(provide/contract
 (get-tiz-outdated-sessions (-> connection? (listof exact-nonnegative-integer?)))
 (update-some-session-metrics (-> exact-nonnegative-integer? connection? (is-a?/c sport-charms%) any/c)))
