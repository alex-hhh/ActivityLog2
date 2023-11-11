;; SPDX-License-Identifier: GPL-3.0-or-later
;; tss.rkt -- calculate TSS (effort) for a session
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

#lang racket/base
(require db/base
         data-frame
         racket/match
         "../sport-charms.rkt"
         "../dbutil.rkt"
         "coggan.rkt")

;; NOTE: these functions were moved out from edit-session-tss.rkt and were
;; written a long time ago.  It would be good to make the interface nicer
;; (e.g. get-session-effort returns a vector of "stuff"), plus add some
;; contracts to the exports.

(provide
 get-session-effort
 effort-np
 effort-avg-hr
 effort-rpe
 effort-duration
 effort-avg-speed
 effort-tss
 effort-distance
 maybe-update-session-tss
 rpe->tss
 swim-speed->tss
 np->tss
 compute-session-tss/hr
 put-session-cg-metrics)

;; Return information used to calculate TSS for a session
(define (get-session-effort session-id db)
  (query-maybe-row
   db
   "select S.sport_id, S.sub_sport_id,
       SS.total_timer_time,
       S.rpe_scale,
       SS.avg_heart_rate,
       SS.normalized_power,
       S.training_stress_score,
       S.start_time,
       SS.avg_speed,
       SS.total_distance,
       (select ETZ.name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone,
       S.name
 from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
 and S.id = ?" session-id))

(define (effort-np effort-data)
  (sql-column-ref effort-data 5 #f))

(define (effort-avg-hr effort-data)
  (sql-column-ref effort-data 4 #f))

(define (effort-rpe effort-data)
  (sql-column-ref effort-data 3 #f))

(define (effort-duration effort-data)
  (sql-column-ref effort-data 2 #f))

(define (effort-avg-speed effort-data)
  (sql-column-ref effort-data 8 #f))

(define (effort-tss effort-data)
  (sql-column-ref effort-data 6 #f))

(define (effort-distance effort-data)
  (sql-column-ref effort-data 9 #f))

;; https://www.trainingbible.com/joesblog/2009/09/estimating-tss.html Uses the
;; following conversion from RPE or HR Zone to TSS/hr.  This works for the
;; Friel HR zone model (with 7 HR zones).  We also interpolate, since in
;; ActivityLog2 zones are linear (e.g. zone 2.5)
;;
;; RPE	HR Zone		TSS/Hr
;;
;; 1	1 (low)		20
;; 2	1		30
;; 3	1 (high)	40
;; 4	2 (low)		50
;; 5	2 (high)	60
;; 6	3		70
;; 7	4		80
;; 8	5a		100
;; 9	5b		120
;; 10	5c		140


;; Convert a "Rating of Perceived Extertion" value into a TSS/hour value, the
;; result can be multiplied by the activity duration to get the TSS of the
;; activity.
(define (rpe->tss/hour rpe)
  (cond ((<= rpe 1) 20)
        ((<= rpe 2) 30)
        ((<= rpe 3) 40)
        ((<= rpe 4) 50)
        ((<= rpe 5) 60)
        ((<= rpe 6) 70)
        ((<= rpe 7) 80)
        ((<= rpe 8) 100)
        ((<= rpe 9) 120)
        ((<= rpe 10) 140)
        (#t 140)))

;; Convert a (heart rate) zone into a TSS/hour value, the result can be
;; multiplied by the activity duration to get the TSS of the activity.
(define (hr-zone->tss/hour zone)
  (cond
    ;; Zone 0 raises from 0 to 20
    ((< zone 1) (+ 0 (* zone 20)))
    ;; Zone 1 raises from 20 to 50
    ((< zone 2) (+ 20 (* (- zone 1) 30)))
    ;; Zone 2 raises from 50 to 70
    ((< zone 3) (+ 50 (* (- zone 2) 20)))
    ;; Zone 3 raises from 70 to 80
    ((< zone 4) (+ 70 (* (- zone 3) 10)))
    ;; Zone 4 raises from 80 to 100
    ((< zone 5) (+ 80 (* (- zone 4) 20)))
    ;; Zone 5a raises from 100 to 120
    ((< zone 6) (+ 100 (* (- zone 5) 20)))
    ;; Zone 5b raises from 120 to 140
    ((<= zone 7) (+ 120 (* (- zone 6) 20)))
    ;; everything else is 140
    (#t 140)))

;; Compute TSS based on RPE and duration
(define (rpe->tss rpe duration)
  (* (rpe->tss/hour rpe) (/ duration 3600.0)))

;; Compute TSS based on zone and duration
(define (hr-zone->tss zone duration)
  (* (hr-zone->tss/hour zone) (/  duration 3600.0)))

;; Compute TSS based on NP (normalized power) and duration
(define (np->tss ftp np duration)
  (let ((intensity-factor (/ np ftp)))
    (* intensity-factor intensity-factor (/ duration 3600.0) 100.0)))

;; Compute TSS based on swim FTP pace and duration
(define (swim-speed->tss tpace speed duration)
  (let ((intensity-factor (/ speed tpace)))
    (* intensity-factor intensity-factor intensity-factor (/ duration 3600.0) 100)))

;; Compute the TSS of a session based on heart rate zones using the data-frame
;; DF.  This is done by computing a fractional TSS for each track point and
;; should provide a better TSS value than simply taking the average HR for the
;; entire session (it is also slower).
(define (compute-session-tss/hr df)
  ;; NOTE: we use the timer series, so we don't count TSS while the recording
  ;; is stopped.  We could use the elapsed series to count TSS while stopped
  ;; as well.
  (if (df-contains? df "hr-zone" "timer")
      (df-fold
       df
       '("timer" "hr-zone")
       0
       (lambda (tss prev next)
         (if prev
             (match-let (((list t0 z0) prev)
                         ((list t1 z1) next))
               (if (and (number? t0) (number? z0) (number? t1) (number? z1))
                   (+ tss (hr-zone->tss (/ (+ z0 z1) 2) (- t1 t0)))
                   tss))
             tss)))
      #f))

(define (calculate-session-tss effort df sid db)
  (let ((duration (effort-duration  effort)))
    (if duration
        (or (let ((np (effort-np effort))
                  (ftp (get-athlete-ftp db)))
              (and np ftp (np->tss ftp np duration)))
            (let ((sport (sql-column-ref effort 0 #f))
                  (dist (effort-distance effort))
                  (tpace (get-athlete-swim-tpace db)))
              (and sport (eq? sport 5) dist tpace
                   ;; For swim sessions, we use the normalized speed (total
                   ;; distance/total time), which includes pauses.  The
                   ;; AVG_SPEED stored in the session summary only counts
                   ;; moving time.
                   (swim-speed->tss tpace (/ dist duration) duration)))
            (compute-session-tss/hr df)
            (let ((rpe (effort-rpe effort)))
              (and rpe (rpe->tss rpe duration))))
        #f)))

;; Store/Update the Coggan metrics CGMETRICS for session SID.
(define (put-session-cg-metrics sid cgmetrics #:database db)
  (match-define (cg ftp np if tss) cgmetrics)
  (call-with-transaction
   db
   (lambda ()
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (query-exec
      db
      "update A_SESSION set intensity_factor = ?, training_stress_score = ? where id = ?"
      if tss sid)
     (query-exec
      db
      "update SECTION_SUMMARY set normalized_power = ? where id = ?"
      np ssid))))

(define (maybe-update-session-tss session-id df db [force? #f])
  (let ((effort (get-session-effort session-id db)))
    (cond ((and (eq? #f (effort-np effort)) ; No Normalized Power available
                (get-athlete-ftp)           ; ... but we have an FTP value
                (df-contains? df "pwr"))    ; and we have a power series
           (define metrics
             (cg-metrics df
                         #:ftp (get-athlete-ftp)
                         #:series "pwr"
                         #:weight-series "timer"))
           (put-session-cg-metrics session-id metrics #:database db))
          ((or force? (not (effort-tss effort)))
           (define tss (calculate-session-tss effort df session-id db))
           (when tss
             (query-exec
              db
              "update A_SESSION set training_stress_score = ? where id = ?"
              tss session-id))))))
