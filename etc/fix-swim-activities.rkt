#lang racket
;; fix-swim-activities.rkt -- attempt to fix bad recordings in swim
;; activities.
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
         "../rkt/activity-util.rkt"
         "../rkt/al-prefs.rkt"
         "../rkt/database.rkt"
         "../rkt/edit-session-tss.rkt"
         "../rkt/utilities.rkt")

(define database-file
  (al-get-pref 'activity-log:database-file (lambda () #f)))

(define *db* (db-open-activity-log database-file))

(define a (db-fetch-session 1232 *db*))

(define (analyze session)
  (let ((avg-cadence (session-avg-strokes-per-length session)))
    (for ((lap (in-list (session-laps session)))
          #:when (> (lap-avg-cadence lap) 0))
      (let ((lengths (lap-lengths lap)))
        (for ((a (in-list (drop-right lengths 1)))
              (b (in-list (cdr lengths))))
          (let ((sum (+ (length-total-cycles a) 
                        (length-total-cycles b))))
            (when (< (/ sum avg-cadence) 1.35)
              (printf "Short length (dbid ~a), cadence ~a~%"
                      (assq1 'database-id a)
                      (length-total-cycles a))
              (printf "Short length (dbid ~a), cadence ~a~%"
                      (assq1 'database-id b)
                      (length-total-cycles b)))))
        (for ((length (in-list lengths)))            
          (let ((p (/ (length-total-cycles length) avg-cadence)))
            (when (> p 1.7)
              (printf "Long length (dbid ~a), cadence ~a~%"
                      (assq1 'database-id length)
                      (length-total-cycles length)))))))))

(define (fix-swim-session-1 session-id db)
  (let ((session (db-fetch-session session-id db))) ; NOTE: we don't really need this
    (let ((avg-cadence (session-avg-strokes-per-length session))
          (pool-length (session-pool-length session))
          (fixed-laps '()))
      (for ((lap (in-list (session-laps session)))
            #:when (> (lap-avg-cadence lap) 0))
        (let ((lengths (lap-lengths lap)))
          (for ((a (in-list (drop-right lengths 1)))
                (b (in-list (cdr lengths))))
            (let ((sum (+ (length-total-cycles a) 
                          (length-total-cycles b))))
              (when (< (/ sum avg-cadence) 1.35)
                (let ((length-id1 (assq1 'database-id a))
                      (length-id2 (assq1 'database-id b)))
                  (join-lengths length-id1 length-id2 pool-length db)
                  (set! fixed-laps (cons (assq1 'database-id lap) fixed-laps))))))

          (for ((length (in-list lengths)))            
            (let ((p (/ (length-total-cycles length) avg-cadence)))
              (when (> p 1.7)
                (let ((length-id (assq1 'database-id length)))
                  (split-length length-id pool-length db))
                (set! fixed-laps (cons (assq1 'database-id lap) fixed-laps)))))))

      ;; We're done, fixup the summary data for laps and the session itself.
      (rebuild-trackpoint-distance session-id pool-length db)
      (for ((lap (in-list (remove-duplicates fixed-laps))))
        (update-lap-summary lap pool-length db))
      (update-session-summary session-id pool-length db)))
  (maybe-update-session-tss session-id db #t))

(define (fix-swim-session session-id db)
  (call-with-transaction 
   db 
   (lambda () (fix-swim-session-1 session-id db))))

(define (fetch-ss-data length-id db)
  (let ((row (query-row db "
select SS.total_timer_time, SS.total_elapsed_time, SS.total_cycles, swim_stroke_id 
 from SECTION_SUMMARY SS, A_LENGTH L
 where L.summary_id = SS.id and L.id = ?" length-id)))
    (values (vector-ref row 0) (vector-ref row 1) (vector-ref row 2) (vector-ref row 3))))

(define (store-ss-data total-time total-elapsed total-cycles swim-stroke pool-length db)
  (let ((avg-cadence (exact-truncate (* 60 (/ total-cycles total-elapsed))))
        (avg-speed (/ pool-length total-elapsed)))
    (query-exec db "
insert into SECTION_SUMMARY(
    total_timer_time, total_elapsed_time, total_cycles, 
    swim_stroke_id, avg_speed, avg_cadence)
values (?, ?, ?, ?, ?, ?)"
                total-time total-elapsed total-cycles swim-stroke 
                avg-speed avg-cadence)
    (db-get-last-pk "SECTION_SUMMARY" db)))

(define (fetch-length-data length-id db)
  (let ((row (query-row db "
select L.lap_id, L.start_time 
  from A_LENGTH L
 where L.id = ?" length-id)))
    (values (vector-ref row 0) (vector-ref row 1))))
  
(define (store-length-data lap-id start-time summary-id db)
  (query-exec db "
insert into A_LENGTH(lap_id, start_time, summary_id)
values (?, ?, ?)" lap-id start-time summary-id)
  (db-get-last-pk "A_LENGTH" db))

(define (store-trackpoint length-id timestamp speed db)
  (query-exec db "
insert into A_TRACKPOINT(length_id, timestamp, speed)
values (?, ?, ?)" length-id timestamp speed)
  (db-get-last-pk "A_TRACKPOINT" db))

(define (join-lengths length-id1 length-id2 pool-length db)
  (let-values ([(lap-id-1 start-time-1) (fetch-length-data length-id1 db)]
               [(lap-id-2 start-time-2) (fetch-length-data length-id2 db)])
    (unless (= lap-id-1 lap-id-2) (error "lap id mismatch"))
    (let-values ([(time-1 elapsed-1 cycles-1 stroke-1) (fetch-ss-data length-id1 db)]
                 [(time-2 elapsed-2 cycles-2 stroke-2) (fetch-ss-data length-id2 db)])
      (unless (= stroke-1 stroke-2) (error "stroke mismatch"))
      (let ((tt (+ time-1 time-2))
            (te (+ elapsed-1 elapsed-2))
            (cy (+ cycles-1 cycles-2))
            (stroke stroke-1))
        (let ((ssid (store-ss-data tt te cy stroke pool-length db)))
          (let ((lid (store-length-data lap-id-1 (min start-time-1 start-time-2) ssid db)))
            (store-trackpoint 
             lid (+ (min start-time-1 start-time-2) tt) 
             (/ pool-length te)
             db))))))
  (query-exec
   db
   (format "delete from SECTION_SUMMARY where id in 
              (select L.summary_id from A_LENGTH L where L.id in (~a, ~a))"
           length-id1 length-id2))
  (query-exec 
   db
   (format "delete from A_TRACKPOINT where length_id in (~a, ~a)" 
           length-id1 length-id2))
  (query-exec 
   db
   (format "delete from A_LENGTH where id in (~a, ~a)" 
           length-id1 length-id2)))

(define (split-length length-id pool-length db)
  (let-values ([(lap-id start-time) (fetch-length-data length-id db)]
               [(time elapsed cycles stroke) (fetch-ss-data length-id db)])
    (let ((tt (/ time 2))
          (te (/ elapsed 2))
          (cy (/ cycles 2)))
      (let ((ssid (store-ss-data tt te cy stroke pool-length db)))
        (let ((lid (store-length-data lap-id start-time ssid db)))
          (store-trackpoint 
           lid (+ start-time tt) (/ pool-length te) db)))
      (let ((ssid (store-ss-data tt te cy stroke pool-length db)))
        (let ((lid (store-length-data lap-id (+ start-time tt) ssid db)))
          (store-trackpoint 
           lid (+ start-time tt tt) (/ pool-length te) db)))))
  (query-exec
   db
   (format "delete from SECTION_SUMMARY where id in 
              (select L.summary_id from A_LENGTH L where L.id in (~a))"
           length-id))
  (query-exec 
   db
   (format "delete from A_TRACKPOINT where length_id in (~a)" 
           length-id))
  (query-exec 
   db
   (format "delete from A_LENGTH where id in (~a)" 
           length-id)))


;; After the lengths of a session have been modified, the trackpoints will
;; contain the wrong distance.  We fix the distance by updating each
;; trackpoint in order, to increase the distance by POOL-LENGTH for each
;; point.
(define (rebuild-trackpoint-distance session-id pool-length db)
  (let ((trackpoints (query-list db "
select T.id as tid
from A_LAP L, A_LENGTH E, A_TRACKPOINT T
where L.session_id = ?
  and E.lap_id = L.id
  and T.length_id = E.id
order by T.timestamp" session-id)))

    (let loop ((tpoints trackpoints)
               (distance pool-length))
      (unless (null? tpoints)
        (query-exec 
         db 
         "update A_TRACKPOINT set distance = ? where id = ?"
         distance (car tpoints))
        (loop (cdr tpoints) (+ distance pool-length))))))

;; Update the lap summary after the lap's lengths have been changed.  We only
;; update relevant data.
(define (update-lap-summary lap-id pool-length db)
  (let ((row (query-row 
              db 
              "select count(L.id), total(SS.total_elapsed_time), 
                      total(SS.total_cycles), max(SS.avg_speed), max(SS.avg_cadence) 
                 from A_LENGTH L, SECTION_SUMMARY SS
                where L.lap_id = ? and L.summary_id = SS.id" 
              lap-id)))
    (when (> (vector-ref row 0) 0)
      (let* ((total-distance (* pool-length (vector-ref row 0)))
             (avg-speed (/ total-distance (vector-ref row 1)))
             (max-speed (vector-ref row 3))
             (avg-cadence (exact-truncate (* 60 (/ (vector-ref row 2) (vector-ref row 1)))))
             (max-cadence (vector-ref row 4))
             (avg-cycle-distance (/ total-distance (vector-ref row 2))))
        (query-exec db "update SECTION_SUMMARY
                            set total_distance = ?,
                                avg_speed = ?,
                                max_speed = ?,
                                avg_cadence = ?,
                                max_cadence = ?,
                                avg_cycle_distance = ?
                          where id = (select L.summary_id from A_LAP L where L.id = ?)"
                    total-distance avg-speed max-speed avg-cadence max-cadence
                    avg-cycle-distance lap-id)))))

;; Update the summary data for the session after its laps have been updated.
(define (update-session-summary session-id pool-length db)
  (let ((row (query-row 
              db 
              "select total(SS.total_distance), total(SS.total_elapsed_time), 
                      total(SS.total_cycles), max(SS.avg_speed), max(SS.avg_cadence) 
                 from A_LAP L, SECTION_SUMMARY SS
                where L.session_id = ? and L.summary_id = SS.id" 
              session-id)))
    (when (> (vector-ref row 0) 0)
      (let* ((total-distance (vector-ref row 0))
             ;; Avg speed for swim activities only counts active laps
             (avg-speed (query-value db
                                     "select total(SS.total_distance) / total(SS.total_elapsed_time)
                                           from A_LAP L, SECTION_SUMMARY SS
                                          where L.session_id = ? and L.summary_id = SS.id
                                            and SS.total_distance > 0" session-id))
             (max-speed (vector-ref row 3))
             ;; NOTE: we need to ignore DRILL and REST laps when computing the
             ;; average cadence.
             (avg-cadence (exact-truncate 
                           (query-value db
                                        "select 60 * total(SS.total_cycles) / total(SS.total_elapsed_time)
                                           from A_LAP L, SECTION_SUMMARY SS
                                          where L.session_id = ? and L.summary_id = SS.id
                                            and SS.total_cycles > 0" session-id)))
             (max-cadence (vector-ref row 4))
             (avg-cycle-distance (/ total-distance (vector-ref row 2))))
        (query-exec db "update SECTION_SUMMARY
                            set total_distance = ?,
                                avg_speed = ?,
                                max_speed = ?,
                                avg_cadence = ?,
                                max_cadence = ?,
                                avg_cycle_distance = ?
                          where id = (select S.summary_id from A_SESSION S where S.id = ?)"
                    total-distance avg-speed max-speed avg-cadence max-cadence
                    avg-cycle-distance session-id)))))

