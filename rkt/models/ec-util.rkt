#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; ec-util.rkt -- elevation correction utilities
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require data-frame
         db/base
         geoid
         racket/class
         racket/contract
         racket/list
         racket/math
         racket/runtime-path
         (only-in "../dbutil.rkt" define-sql-statement)
         "../gps-segments/gps-segments.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "elevation-correction.rkt")

;; Elevation correction does not produce good results when there are small
;; amounts of data and sessions are recorded with mostly flat terrain.  While
;; actual corrected elevation is stored in parallel to the actual elevation
;; recorded by the device and both can be plotted, the summary values (total
;; ascent and descent) are computed from the corrected elevation and, if this
;; is incorrect, these values can be wildly inaccurate.
;;
;; Users can manually delete the corrected elevation and re-calculate it for
;; individual activities, but we also allow disabling this feature on import
;; for users who find their data to be completely incorrect
;;
;; See also discussion thread on #51

(define fix-elevation-on-import-tag 'activity-log:fix-elevation-on-import)
(define fix-elevation-on-import-val (get-pref fix-elevation-on-import-tag (lambda () #t)))
(define (fix-elevation-on-import) fix-elevation-on-import-val)
(define (set-fix-elevation-on-import new-val)
  ;; Write the value back to the store
  (put-pref fix-elevation-on-import-tag new-val)
  (set! fix-elevation-on-import-val new-val)
  (if new-val
      (dbglog "fix elevation on import enabled")
      (dbglog "fix elevation on import disabled")))


;;...................................... updating the corrected altitude ....

;; This section deals with updating the corrected altitude in the database

;; SQL statement to update the corrected altitude for a trackpoint
(define update-trackpoint-stmt
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set corrected_altitude = ? where id = ?")))

;; Update the A_TRACKPOINT rows in the database with altitude data from
;; ALTITUDE-DATA which is a vector of ECPOINT structures as produced by
;; AVERAGE-ALTITUDE.
(define (update-trackpoints db altitude-data
                            [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage
          "Updating altitude for GPS track" (vector-length altitude-data)))
  (call-with-transaction
   db
   (lambda ()
     (for ([(point index) (in-indexed (in-vector altitude-data))])
       (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
         (send progress-monitor set-progress (add1 index)))
       (let ((id (ecpoint-id point))
             (calt (ecpoint-calt point)))
         (query-exec db update-trackpoint-stmt (or calt sql-null) id))))))

(define q-get-altitude1
  (virtual-statement
   (lambda (dbsys) "
select corrected_altitude
  from A_TRACKPOINT
 where corrected_altitude is not null
   and length_id = ? order by timestamp")))

(define q-update-ss1
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_LENGTH L where L.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session length
;; based on the already corrected trackpoints in this length.
(define (update-summary-altitude-for-length db length-id)
  (let ((altitude (query-list db q-get-altitude1 length-id)))
    (unless (null? altitude)
      ;; NOTE: we only accumulate ascent and descent if the elevation gain or
      ;; loss is greater than 1 meter -- this avoids accumulating lots of very
      ;; small elevation changes, which would artificially inflate the total
      ;; elevation gain.
      (define-values (ascent descent)
        (for/fold ([ascent 0.0]
                   [descent 0.0]
                   [base (car altitude)]
                   #:result (values ascent descent))
                  ([current (in-list (cdr altitude))])
          (cond ((> current (add1 base))
                 (values (+ ascent (- current base)) descent current))
                ((< current (sub1 base))
                 (values ascent (+ descent (- base current)) current))
                (#t
                 (values ascent descent base)))))
        (query-exec db q-update-ss1
                    (exact->inexact ascent) (exact->inexact descent) length-id))))

(define q-get-altitude2
  (virtual-statement
   (lambda (dbsys) "
select sum(SS.total_corrected_ascent),
       sum(SS.total_corrected_descent)
  from SECTION_SUMMARY SS,
       A_LENGTH LL
  where LL.lap_id = ? and LL.summary_id = SS.id")))

(define q-update-ss2
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_LAP L where L.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session lap
;; based on the already corrected trackpoints in this lap (the summary data
;; for the laps lengths will also be corrected).
(define (update-summary-altitude-for-lap db lap-id)
  (let ((lengths (query-list db "select id from A_LENGTH where lap_id = ?" lap-id)))
    (for ((length (in-list lengths)))
      (update-summary-altitude-for-length db length))
    (let ((row (query-row db q-get-altitude2 lap-id)))
      (query-exec db q-update-ss2 (vector-ref row 0) (vector-ref row 1) lap-id))))

(define q-get-altitude3
  (virtual-statement
   (lambda (dbsys) "
select sum(SS.total_corrected_ascent),
       sum(SS.total_corrected_descent)
  from SECTION_SUMMARY SS,
       A_LAP L
  where L.session_id = ? and L.summary_id = SS.id")))

(define q-update-ss3
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_SESSION S where S.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session based
;; on the already corrected trackpoints in this session (the summary data for
;; the laps and lengths will also be corrected).
(define (update-summary-altitude-for-session db session-id)
  (let ((laps (query-list db "select id from A_LAP where session_id = ?" session-id)))
    (for ((lap (in-list laps)))
      (update-summary-altitude-for-lap db lap))
    (let ((row (query-row db q-get-altitude3 session-id)))
      (query-exec db q-update-ss3 (vector-ref row 0) (vector-ref row 1) session-id))))


;;......................................... fixup elevation entry points ....

(define-runtime-path session-geoids-query-file "../../sql/queries/ec-session-geoids.sql")
(define session-geoids-query (define-sql-statement session-geoids-query-file))

;; Return the geoids for all track points in a session, along with the
;; trackpoint id. Returns a list of (vector trackpoint-id geoid)
(define (session-trackpoints db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session" 0))
  (for/list ([(id geoid) (in-query db (session-geoids-query) session-id #:fetch 1000)])
    (vector id (sqlite-integer->geoid geoid))))

(define (fixup-elevation-for-session-internal db session-id ec-helper [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session..." 0))
  (define trackpoints (session-trackpoints db session-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for session..." 0))
  (unless (null? trackpoints)           ; maybe the session has no GPS data?
    (define tp-elevation
      (send ec-helper elevation-correction/geoid trackpoints #:progress-monitor progress-monitor))
    (call-with-transaction
     db
     (lambda ()
       (update-trackpoints db tp-elevation progress-monitor)
       (when progress-monitor
         (send progress-monitor begin-stage (format "Updating summary altitude") 0))
       (update-summary-altitude-for-session db session-id)))))

(define (fixup-elevation-for-session db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define ec-helper (new ec-helper% [database db]))
  (if (cons? session-id)
      (for/list ([sid session-id])
        (dbglog "fixup-elevation-for-session ~a started" sid)
        (fixup-elevation-for-session-internal db sid ec-helper progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" sid))
      (begin
        (dbglog "fixup-elevation-for-session ~a started" session-id)
        (fixup-elevation-for-session-internal db session-id ec-helper progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" session-id)))
  (when progress-monitor
    (send progress-monitor finished)))

(define (fixup-elevation-for-all-sessions db [progress-monitor #f])
  (dbglog "fixup-elevation-for-all-sessions started")
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define ec-helper (new ec-helper% [database db]))
  (let ((sessions (query-list db "select id from A_SESSION")))
    (when progress-monitor
      (send progress-monitor begin-stage "Fixup elevation for all sessions"
            (length sessions)))
    (for (((sid index) (in-indexed (in-list sessions))))
      #:break (if progress-monitor
                  (not (send progress-monitor set-progress index))
                  #f)
      (fixup-elevation-for-session-internal db sid ec-helper #f)))
  (when progress-monitor
    (send progress-monitor finished))
  (dbglog "fixup-elevation-for-all-sessions completed"))

(define (interactive-fixup-elevation database session-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data"]
         [icon (sql-export-icon)]))

  (define progress-monitor
    (class object% (init-field progress-dialog) (super-new)

      (define num-items 100)

      (define/public (begin-stage msg max-items)
        (send progress-dialog set-message msg)
        (send progress-dialog set-progress 0)
        (set! num-items max-items))

      (define/public (set-progress n)
        (let ((pct (exact-round (* 100.0 (if (> num-items 0) (/ n num-items) 1.0)))))
          (send progress-dialog set-progress pct)))

      (define/public (finished)
        (send progress-dialog set-progress 100))))

  (define (task progress-dialog)
    (let ((m (new progress-monitor [progress-dialog progress-dialog])))
      (if session-id
          (fixup-elevation-for-session database session-id m)
          (fixup-elevation-for-all-sessions database m))))

  (send progress-dialog run parent-window task))

;; Remove the corrected elevation information for SESSION-ID.  Trackpoint and
;; section summary altitudes are removed.  This will cause all grade and
;; summary information displays to use the recorded elevation.
(define (clear-corrected-elevation-for-session database session-id)
  (call-with-transaction
   database
   (lambda ()
     (query-exec database "
update A_TRACKPOINT
   set corrected_altitude = null
 where length_id in (select L.id
                       from A_LENGTH L, A_LAP P
                      where L.lap_id = P.id
                        and P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select P.summary_id
                from A_LAP P
               where P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select L.summary_id
                from A_LAP P, A_LENGTH L
               where L.lap_id = P.id
                 and P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select S.summary_id
                from A_SESSION S
               where S.id = ?)" session-id))))


;;.......................................... fixup-elevation-for-segment ....

(define-runtime-path segment-geoids-query-file "../../sql/queries/ec-segment-geoids.sql")
(define segment-geoids-query (define-sql-statement segment-geoids-query-file))

;; Return the geoids for all track points in a segment, along with the
;; waypoint id. Returns a list of (vector waypoint-id geoid)
(define (segment-waypoints db segment-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for segment" 0))
  (for/list ([(id geoid) (in-query db (segment-geoids-query) segment-id #:fetch 1000)])
    (vector id (sqlite-integer->geoid geoid))))

;; SQL statement to update the corrected altitude for a trackpoint
(define update-waypoint-stmt
  (virtual-statement
   (lambda (dbsys)
     "update GPS_SEGMENT_WAYPOINT set altitude = ? where id = ?")))

;; Update the GPS_SEGMENT_WAYPOINT rows in the database with altitude data
;; from ALTITUDE-DATA which is a vector of ECPOINT structures as produced by
;; AVERAGE-ALTITUDE.
(define (update-segment-waypoints db altitude-data
                                  [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage
          "Updating altitude for GPS segment" (vector-length altitude-data)))
  (call-with-transaction
   db
   (lambda ()
     (for ([point (in-vector altitude-data)]
           [index (in-naturals)])
       (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
         (send progress-monitor set-progress (add1 index)))
       (let ((id (ecpoint-id point))
             (calt (ecpoint-calt point)))
         (query-exec db update-waypoint-stmt (or calt sql-null) id))))))

;; SQL statement to update the grade for a waypoint
(define update-segment-grade-stmt
  (virtual-statement
   (lambda (dbsys)
     "update GPS_SEGMENT_WAYPOINT set grade = ? where id = ?")))

(define-runtime-path update-segment-summary-query-file "../../sql/queries/ec-update-segment.sql")
(define update-segment-summary-query (define-sql-statement update-segment-summary-query-file))

;; Update in the database the fields derived from altitude data in the segment
;; identified by SEGMENT-ID.  The segment is assumed to already have its
;; altitude data updated (see `update-segment-waypoints`), and ALTITUDE-DATA
;; is used just to find the GPS_SEGMENT_WAYPOINT database IDs of each
;; waypoint.
(define (update-summary-altitude-for-segment db segment-id altitude-data)
  ;; force refresh of the segment, to get new altitude data
  (log-event 'gps-segment-updated segment-id)
  (define segment (fetch-gps-segment db segment-id))
  ;; remove grade series as it is now incorrect, as well as all of the
  ;; altitude related summary properties.
  (df-del-series! segment "grade")
  (for ([p (in-list '(segment-height segment-grade total-ascent total-descent
                                     max-grade min-elevation max-elevation))])
    (df-del-property! segment p))
  ;; Add grade series and new summary data based on fresh altitude
  (fixup-segment-data segment)
  (call-with-transaction
   db
   (lambda ()
     ;; Put the grade values back
     (for ([point (in-vector altitude-data)]
           [grade (in-data-frame segment "grade")])
       (let ((id (ecpoint-id point)))
         (query-exec db update-segment-grade-stmt (or grade sql-null) id)))
     (query-exec
      db
      (update-segment-summary-query)
      (or (df-get-property segment 'segment-height) sql-null)
      (or (df-get-property segment 'segment-grade) sql-null)
      (or (df-get-property segment 'total-ascent) sql-null)
      (or (df-get-property segment 'total-descent) sql-null)
      (or (df-get-property segment 'max-grade) sql-null)
      (or (df-get-property segment 'min-elevation) sql-null)
      (or (df-get-property segment 'max-elevation) sql-null)
      (or (df-get-property segment 'fiets-score) sql-null)
      segment-id))))

;; Same as `fixup-elevation-for-session-internal`, except for GPS segments
;; instead of sessions
(define (fixup-elevation-for-segment-internal db segment-id ec-helper [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for segments..." 0))
  (define trackpoints (segment-waypoints db segment-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for segment..." 0))
  (unless (null? trackpoints)           ; maybe the segment has no GPS data?
    (define tp-elevation
      (send ec-helper elevation-correction/geoid trackpoints #:progress-monitor progress-monitor))
    (call-with-transaction
     db
     (lambda ()
       (update-segment-waypoints db tp-elevation progress-monitor)
       (when progress-monitor
         (send progress-monitor begin-stage (format "Updating summary altitude") 0))
       (update-summary-altitude-for-segment db segment-id tp-elevation)))))

;; Same as `fixup-elevation-for-session`, except for GPS segments instead of
;; sessions
(define (fixup-elevation-for-segment db segment-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define ec-helper (new ec-helper% [database db]))
  (dbglog "fixup-elevation-for-segment ~a started" segment-id)
  (fixup-elevation-for-segment-internal db segment-id ec-helper progress-monitor)
  (dbglog "fixup-elevation-for-segment ~a completed" segment-id)
  (when progress-monitor
    (send progress-monitor finished)))

;; Same as `interactive-fixup-elevation-for-session`, except for GPS segments
;; instead of sessions
(define (interactive-fixup-elevation-for-segment database segment-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data for segment"]
         [icon (sql-export-icon)]))

  (define progress-monitor
    (class object% (init-field progress-dialog) (super-new)

      (define num-items 100)

      (define/public (begin-stage msg max-items)
        (send progress-dialog set-message msg)
        (send progress-dialog set-progress 0)
        (set! num-items max-items))

      (define/public (set-progress n)
        (let ((pct (exact-round (* 100.0 (if (> num-items 0) (/ n num-items) 1.0)))))
          (send progress-dialog set-progress pct)))

      (define/public (finished)
        (send progress-dialog set-progress 100))))

  (define (task progress-dialog)
    (let ((m (new progress-monitor [progress-dialog progress-dialog])))
      (when segment-id
        (fixup-elevation-for-segment database segment-id m))))

  (send progress-dialog run parent-window task))



;;............................................................. provides ....

(provide/contract
 (fix-elevation-on-import (-> boolean?))
 (set-fix-elevation-on-import (-> boolean? any/c))
 (fixup-elevation-for-session (->* (connection?
                                    (or/c exact-nonnegative-integer?
                                          (listof exact-nonnegative-integer?)))
                                   (any/c) ; the progress monitor
                                   any/c))
 (fixup-elevation-for-all-sessions (->* (connection?)
                                        (any/c) ; the progress monitor
                                        any/c))
 (interactive-fixup-elevation (->* (connection?
                                    (or/c #f
                                          exact-nonnegative-integer?
                                          (listof exact-nonnegative-integer?)))
                                   (any/c) ; the parent window
                                   any/c))
 (clear-corrected-elevation-for-session (-> connection? exact-nonnegative-integer? any/c))

 (fixup-elevation-for-segment (->* (connection? exact-nonnegative-integer?)
                                   (any/c) ; the progress monitor
                                   any/c))
 (interactive-fixup-elevation-for-segment (->* (connection?
                                                (or/c #f
                                                      exact-nonnegative-integer?
                                                      (listof exact-nonnegative-integer?)))
                                               (any/c) ; the parent window
                                               any/c)))
