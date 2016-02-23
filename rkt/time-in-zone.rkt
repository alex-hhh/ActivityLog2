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

(require db
         racket/class
         racket/match
         racket/math
         "database.rkt"
         "dbglog.rkt"
         "icon-resources.rkt"
         "plot-axis-def.rkt"
         "plot-builder.rkt"
         "widgets.rkt")

(provide update-time-in-zone-data)
(provide interactive-update-time-in-zone-data)

;; Compute the time spend in each zone for SESSION (as returned by
;; db-fetch-session).  The zones are defined by the AXIS-DEF (can be
;; axis-hr-zone, axis-power-zone) and are defined in "plot-axis-def.rkt"
;;
;; Returns a (Vectorof (Vector Zone Duration)), or #f if no zone data can be
;; calculated.  We cannot calculate zone data either if no zones are defined
;; for this session, or the session does not have the resquired data.
(define (time-in-zone session axis-def)
  (define zone-data
    (let-values ([(data lap-markers min-x max-x min-y max-y)
                  (extract-data session axis-elapsed-time axis-def 0)])
      data))
  (if zone-data (make-histogram zone-data 1 #f #t) #f))

;; Store time in zone data in the TIME_IN_ZONE table in the database.  SID is
;; the session id, ZID is the zone definition id, DATA is what
;; `time-in-hr-zone' or `time-in-power-zone' returns and DB is the database.
(define (store-time-in-zone sid zid data db)
  (call-with-transaction
   db
   (lambda ()
     (query-exec
      db
      "delete from TIME_IN_ZONE where session_id = ? and sport_zone_id = ?"
      sid zid)
     (for ([zd data])
       (match-define (vector zone-id duration) zd)
       (query-exec
        db
        "insert into TIME_IN_ZONE(session_id, sport_zone_id, zone_id, duration)
         values(?, ?, ?, ?)"
        sid zid zone-id duration)))))

;; Find the definition id that corresponds to SID, a session ID.  METRIC-ID is
;; 1 for heart rate and 3 for power (see the E_ZONE_METRIC table in the
;; database schema)
(define (get-zone-id sid metric-id db)
  (query-maybe-value
   db
   "select zone_id from V_SPORT_ZONE_FOR_SESSION
    where session_id = ? and zone_metric_id = ?"
   sid metric-id))

;; Update the TIME_IN_ZONE table with data for a session.  Both heart rate and
;; power zones are updated (if available).  Previous data for this session is
;; deleted.
(define (update-time-in-zone-data sid db)
  (let ((session (db-fetch-session sid db))
        (pwr-zone-id (get-zone-id sid 3 db))
        (hr-zone-id (get-zone-id sid 1 db)))
    (when pwr-zone-id
      (let ((data (time-in-zone session axis-power-zone)))
        (when data
          (store-time-in-zone sid pwr-zone-id data db))))
    (when hr-zone-id
      (let ((data (time-in-zone session axis-hr-zone)))
        (when data
          (store-time-in-zone sid hr-zone-id data db))))))

;; Update the time in zone information for all sessions in the database.
;; Provides a progress bar and allows the user to cancel the operation.
(define (interactive-update-time-in-zone-data database [parent-window #f])

  (define progress-dialog
    (new al-progress-dialog%
         [title "Update time in zone data"]
         [icon sql-export-icon]))

  (define (task progress-dialog)
    (send progress-dialog set-message "Fetching list of sessions...")
    (define sessions
      (query-list
       database
       "select distinct session_id from V_SPORT_ZONE_FOR_SESSION where zone_metric_id in (1, 3)"))
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
            (dbglog (format "while updating session ~a: ~a" sid e))
            (raise e))))
        (update-time-in-zone-data sid database)))
    (dbglog "interactive-update-time-in-zone-data complete"))
  
  (send progress-dialog run parent-window task))

