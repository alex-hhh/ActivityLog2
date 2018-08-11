#lang racket/base
;; import.rkt -- import acivities into the database
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
         "database.rkt"
         "utilities.rkt"
         "elevation-correction.rkt"
         "time-in-zone.rkt"
         "weather.rkt")

(provide import-new-activities-from-directory do-post-import-tasks)

(define (import-new-activities-from-directory dir db [file-callback #f] [global-callback #f])
  (query-exec db "delete from LAST_IMPORT")
  (dbglog "importing activities from ~a" dir)
  (when global-callback
    (global-callback (format "Importing activities from ~a~%" dir)))
  (db-import-activities-from-directory dir db file-callback)
  (let ((num-imported (query-value db "select count(*) from LAST_IMPORT")))
    (when (> num-imported 0)
      (do-post-import-tasks db global-callback))
    (dbglog "import complete (~a activities imported)" num-imported)))

;; Perform database maintenance tasks after an import.  `global-callback' will
;; be used to report progress.
(define (do-post-import-tasks db [global-callback #f])
  (define (show-progress msg)
    (dbglog msg)
    (when global-callback (global-callback msg)))

  (show-progress "updating swim drills...")
  (update-swim-drills-for-new-sessions db)
  (show-progress "updating old style equipment serial numbers...")
  (update-old-style-equipment-serial db)
  (show-progress "updating equipment use...")
  (update-equipment-part-of db)
  (show-progress "updating corrected elevation...")
  (update-elevation-for-new-sessions db)
  (show-progress "updating time in zone...")
  (update-tiz-for-new-sessions db)
  (show-progress "updating weather data...")
  (update-weather-for-new-sessions db)
  (for ((sid (get-new-sessions db)))
    (log-event 'session-created sid)))

;; Some Garmin firmware versions reported only the lower two bytes of the
;; serial number of equipment like heart rate monitor or cadence sensors.
;; This is listed in the EQUIPMENT table as a separate row from the equivalent
;; equipment reported with its 32 bit serial number.  This fixup re-assigns
;; all sessions that list the 16 bit serial to the corresponding 32 bit serial
;; equipment.
(define (update-old-style-equipment-serial db)
  (call-with-transaction
    db
    (lambda ()
      (query
       db
       "
insert into EQUIPMENT_USE (equipment_id, session_id)
select SM.neid, OSU.sid
  from (select EQ.id as neid,
               EQ.serial_number as nserial,
               (EQ.serial_number % 65536) as oserial
          from EQUIPMENT EQ
         where EQ.serial_number > 65536) SM,
       (select EQ.id as oeid,
               EQ.serial_number as oserial,
               EU.session_id as sid
          from EQUIPMENT_USE EU, EQUIPMENT EQ, A_SESSION S, LAST_IMPORT LI
         where EU.equipment_id = EQ.id
           and EU.session_id = S.id
           and S.activity_id = LI.activity_id
           and EQ.serial_number < 65536) OSU
 where OSU.oserial = SM.oserial
   and SM.neid not in (
     select EU2.equipment_id
       from EQUIPMENT_USE EU2
      where EU2.session_id = OSU.sid)")

      (query-exec
       db
   "delete from EQUIPMENT_USE
 where equipment_id in
       (select EQ1.id
          from EQUIPMENT EQ1, EQUIPMENT EQ2
         where EQ1.serial_number < 65536
           and EQ2.serial_number >= 65536
           and (EQ2.serial_number % 65536) = EQ1.serial_number)
   and session_id in
       (select id from A_SESSION S, LAST_IMPORT LI
         where S.activity_id = LI.activity_id)"))))

;; Add equipment that is referenced in EQUIPMENT.part_of of the equipment
;; already listed for the imported activities in EQUIPMENT_USE
(define (update-equipment-part-of db)

  ;; NOTE: the "distinct" part of the select is needed if there are multiple
  ;; "part_of" for the same equipment.  E.g. A bike cadence and a power meter
  ;; both being part-of the "bike".

  (define (do-fixup)
    (query db "
insert into EQUIPMENT_USE(session_id, equipment_id)
select distinct S.id, E.part_of
  from LAST_IMPORT LI, A_SESSION S, EQUIPMENT_USE EU, EQUIPMENT E
 where S.activity_id = LI.activity_id
   and EU.session_id = S.id
   and EU.equipment_id = E.id
   and E.part_of is not null
   and E.part_of not in (select EU1.equipment_id
                           from EQUIPMENT_USE EU1
                          where EU1.session_id = S.id)"))

  ;; NOTE: to handle hierarhical part_of realationships, we run this query
  ;; until the query does not insert any more rows.
  (let loop ((fix-result (do-fixup)))
    (unless (equal? 0 (cdr (assq 'affected-rows (simple-result-info fix-result))))
      (loop (do-fixup)))))

(define (get-new-sessions db)
  (query-list db "
select S.id from A_SESSION S, LAST_IMPORT LI where S.activity_id = LI.activity_id"))

(define (update-elevation-for-new-sessions db [progress-monitor #f])
  (let ((sessions (get-new-sessions db)))
    (when progress-monitor
      (send progress-monitor
            begin-stage "Fixup elevation data for new sessions" (length sessions)))
    (update-tile-codes db)
    (fixup-elevation-for-session db sessions #f)
    (when progress-monitor
      (send progress-monitor set-progress (- (length sessions) 1)))))

(define (update-weather-for-new-sessions db [progress-monitor #f])
  (let ((sessions (get-new-sessions db)))
    (when progress-monitor
      (send progress-monitor
            begin-stage "Fetching weather data for new sessions" (length sessions)))
    (for ((sid (in-list sessions))
          (n (in-range (length sessions))))
      (update-session-weather-auto db sid)
      (when progress-monitor
        (send progress-monitor set-progress (+ n 1))))))

(define (update-tiz-for-new-sessions db [progress-monitor #f])
  (let ((sessions (get-new-sessions db)))
    (when progress-monitor
      (send progress-monitor
            begin-stage "Updating metrics for new sessions" (length sessions)))
    (for ((sid (in-list sessions))
          (n (in-range (length sessions))))
      (update-time-in-zone-data sid db)
      (when progress-monitor
        (send progress-monitor set-progress (+ n 1))))))


;;................................................... update-swim-drills ....

;; Get the drill laps for a session.  These are laps with a swim stroke of 4,
;; and will only apply to lap swimming sessions.
(define drill-laps-sql
  (virtual-statement
   (lambda (dbsys)
     "select P.id, P.start_time, SS.total_timer_time, SS.total_distance
        from A_LAP P, SECTION_SUMMARY SS
       where P.summary_id = SS.id
         and SS.swim_stroke_id = 4
         and P.session_id = ?")))

(define lengths-for-lap-sql
  (virtual-statement
   (lambda (dbsys)
     "select L.id, L.summary_id
        from A_LENGTH L
       where L.lap_id = ?")))

(define update-length-sql
  (virtual-statement
   (lambda (dbsys)
     "update A_LENGTH set start_time = ? where id = ?")))

(define update-summary-sql
  (virtual-statement
   (lambda (dbsys)
     "update SECTION_SUMMARY set total_distance = ?, avg_speed = ? where id = ?")))

;; Fixup the lengths in a lap swimming activity.  Some garmin watches will
;; record the same timestamp for every length in a 'drill' lap and will not
;; record any distance or speed for these lenghts.  This function fixes up the
;; start time for such lengths and also adds in distance and speed
;; information.
;;
;; NOTE: we can call this function on any session, as it will do nothing if
;; there are no swim drill laps in it.
(define (fixup-swim-drills db sid)

  (define (do-fixup)
    (for ([row (query-rows db drill-laps-sql sid)])
      (match-define (vector lap-id start-time duration distance) row)
      (define lengths (query-rows db lengths-for-lap-sql lap-id))
      (define ldistance (/ distance (length lengths)))
      (define ltime (/ duration (length lengths)))
      (define lspeed (/ ldistance ltime))
      (for (((row idx) (in-indexed lengths)))
        (match-define (vector length-id summary-id) row)
        (query-exec db update-length-sql
                    (exact-round (+ start-time (* idx ltime))) length-id)
        (query-exec db update-summary-sql
                    (exact-round ldistance) lspeed summary-id))))

  (call-with-transaction db do-fixup))

(provide fixup-swim-drills)             ; used by etc/fixup-drills.rkt

(define (update-swim-drills-for-new-sessions db [progress-monitor #f])
  (let ((sessions (get-new-sessions db)))
    (when progress-monitor
      (send progress-monitor
            begin-stage "Updating lap swim drills for new sessions" (length sessions)))
    (for (((sid idx) (in-indexed sessions)))
      (fixup-swim-drills db sid)
      (when progress-monitor
        (send progress-monitor set-progress (+ idx 1))))))
