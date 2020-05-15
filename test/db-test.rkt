#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         rackunit
         racket/match
         data-frame
         racket/class
         racket/draw
         "../rkt/database.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/dbutil.rkt"
         "../rkt/fit-file/activity-util.rkt"
         "../rkt/session-df/session-df.rkt"
         "../rkt/models/sport-zone.rkt"
         "../rkt/sport-charms.rkt"
         "../rkt/weather.rkt"
         "../rkt/workout-editor/wk-db.rkt"
         "../rkt/workout-editor/wk-fit.rkt"
         "../rkt/workout-editor/wkstep.rkt"
         "../rkt/utilities.rkt"
         "../rkt/time-in-zone.rkt"
         "test-util.rkt"
         "custom-test-runner.rkt")

(set-allow-weather-download #f)        ; don't download weather for unit tests
(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!


;;............................................................ test data ....

(define a1 "test-data/920xt-lap-swim.fit")
(define a2 "test-data/920xt-run.fit")
(define a3 "test-data/920xt-triathlon.fit")
(define a4 "test-data/920xt-bike.fit")
(define a5 "test-data/310xt-ows.fit")
(define a6 "test-data/310xt-bike.fit")
(define a7 "test-data/310xt-run.fit")
(define a8 "test-data/garmin-swim.fit")
(define a9 "test-data/cydynamics-bike.fit")


;;.................................................... common utilities ....

(define (fresh-database? db)
  (= (query-value db "select count(*) from ACTIVITY") 0))

(define (activity-count db)
  (query-value db "select count(*) from ACTIVITY"))

(define (db-import-manual-session db)
  (let* ((duration 3600)
         (distance 1000)
         (avg-speed (if (and duration distance (> duration 0))
                        (/ distance duration) #f))
         (sport (cons 1 #f))
         (name "Manual session")
         (desc "Manual session description")
         (start-time (current-seconds))
         (rpe-scale 2))
    (call-with-transaction
     db
     (lambda ()
       (query-exec
        db
        "insert into SECTION_SUMMARY(total_timer_time, total_elapsed_time, total_distance, avg_speed)
             values(?, ?, ?, ?)"
        (or duration sql-null)
        (or duration sql-null)
        (or distance sql-null)
        (or avg-speed sql-null))
       (let ((ssid (db-get-last-pk "SECTION_SUMMARY" db)))
         (query-exec db "insert into ACTIVITY(start_time) values (?)" start-time)
         (let ((aid (db-get-last-pk "ACTIVITY" db)))
           (query-exec
            db
            "insert into A_SESSION(name, description, activity_id, start_time, sport_id, sub_sport_id, rpe_scale, summary_id)
                 values(?, ?, ?, ?, ?, ?, ?, ?)"
            (or name sql-null)
            (or desc sql-null)
            aid
            start-time
            (or (car sport) sql-null)
            (or (cdr sport) sql-null)
            (if (eqv? rpe-scale 0) sql-null rpe-scale)
            ssid)))
       (let ((sid (db-get-last-pk "A_SESSION" db)))
         sid)))))

(define (db-check-tile-code db)
  (let ((cnt (query-value db "
select count(*)
  from A_TRACKPOINT
 where tile_code is null
   and (position_lat is not null
        or position_long is not null)")))
    (check = 0 cnt "Missing tile codes from A_TRACKPOINT")))

;; Store a new set of sport zones in the database for SPORT/SUB-SPORT and the
;; ZONE-METRIC.
;;
;; ZONES are defined as a list of numbers representing zone boundaries, first
;; item is the minimum value and last item is the maximum value for the metric
;;
;; ZONE-NAMES, when present is a list of strings, one for each zone.  Can be
;; #f, in which case the zones are unnamed and the software will simply call
;; them "Zone 0", "Zone 1", etc.
;;
;; VALID-FROM defines the unix timestamp when these set of sport zones are
;; valid from and they default to current time.  The zones will be valid until
;; another zone definition with a more recent timestamp is defined.
;;
;; DATABASE is the database connection and default to `(current-database)`
;;
;; Returns the zone ID for the newly defined sport zones
;;
;;
;; THIS IS A TEST FUNCTION, use `put-sport-zones` from models/sport-zone.rkt
;; for in-application use.
(define (db-put-sport-zones sport sub-sport zone-metric zones
                            #:zone-names (zone-names #f)
                            #:valid-from (valid-from (current-seconds))
                            #:database (db (current-database)))
  (define z (sz sport sub-sport
                (id->metric zone-metric)
                (list->vector zones)
                (if zone-names
                    (list->vector zone-names)
                    (for/vector ((n (in-range (length zones)))) (format "Zone ~a" n)))
                (for/vector ([n (in-range (length zones))])
                  (make-object color% n n n))
                valid-from
                #f                      ; valid until
                #f))
  (put-sport-zones z #:database db))


(define (fill-sport-zones db #:valid-from (valid-from (current-seconds)))
  (db-put-sport-zones 2 #f 1 '(60 130 140 150 160 170 220)
                   #:valid-from valid-from)
  (db-put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220)
                   #:valid-from valid-from)
  (db-put-sport-zones 2 #f 3 '(-1 0 100 140 180 220 230 250 600)
                   #:valid-from valid-from))


;;.................................... cycling dynamics import and fetch ....

(define (cyd-check-session-values session-id db)
  (check-not-exn
   (lambda ()
     (printf "Checking that CYD values are not null for session~%")
     (let ((row (query-row db
                           "select SS.avg_left_pco, SS.avg_right_pco,
                             SS.avg_left_pp_start, SS.avg_left_pp_end,
                             SS.avg_right_pp_start, SS.avg_right_pp_end,
                             SS.avg_left_ppp_start, SS.avg_left_ppp_end,
                             SS.avg_right_ppp_start, SS.avg_right_ppp_end
                             from A_SESSION S, SECTION_SUMMARY SS
                             where S.summary_id = SS.id and S.id = ?" session-id)))
       (for ((item (in-vector row)))
         (check-pred (lambda (v) (not (sql-null? v)))
                     item
                     (format "Cycling dynamic field should not be null ~a" row)))))))

(define (cyd-check-lap-values session-id db)
  (check-not-exn
   (lambda ()
     (printf "Checking that CYD values are not null for all laps~%")
     (let ((rows (query-rows db
                             "select SS.avg_left_pco, SS.avg_right_pco,
                             SS.avg_left_pp_start, SS.avg_left_pp_end,
                             SS.avg_right_pp_start, SS.avg_right_pp_end,
                             SS.avg_left_ppp_start, SS.avg_left_ppp_end,
                             SS.avg_right_ppp_start, SS.avg_right_ppp_end
                             from A_SESSION S, SECTION_SUMMARY SS, A_LAP L
                             where L.summary_id = SS.id and L.session_id = S.id and S.id = ?"
                             session-id)))
       (for ((row (in-list rows)))
         (for ((item (in-vector row)))
           (check-pred (lambda (v) (not (sql-null? v)))
                       item
                       (format "Cycling dynamic field should not be null ~a" row))))))))

(define (cyd-check-track-values session-id db)
  (check-not-exn
   (lambda ()
     (printf "Checking that CYD values are present in the track~%")
     ;; NOTE: not all trackpoints will have these values so we just look at the counts
     (let ((row (query-row db "
select count(T.id),
       count(T.left_pco),
       count(T.right_pco),
       count(T.left_pp_start),
       count(T.left_pp_end),
       count(T.right_pp_start),
       count(T.right_pp_end),
       count(T.left_ppp_start),
       count(T.left_ppp_end),
       count(T.right_ppp_start),
       count(T.right_ppp_end)
  from A_TRACKPOINT T, A_LENGTH E, A_LAP L, A_SESSION S
 where T.length_id = E.id
   and E.lap_id = L.id
   and L.session_id = S.id
   and S.id = ?" session-id)))
       (for ((item (in-vector row)))
         (check-pred (lambda (v) (and (not (sql-null? v)) (> v 0)))
                     item
                     (format "Cycling dynamic field should not be null or zero ~a" row)))
       (printf "Track stats: ~a~%" row)))))

(define (cyd-check-retrieved-session session)
  (printf "Check that CYD values are retrieved from the database~%")

  ;; The avg CYD fields must be present for the session
  (check-pred number? (session-avg-left-pco session) "avg-left-pco")
  (check-pred number? (session-avg-right-pco session) "avg-right-pco")
  (check-pred number? (session-avg-left-pp-start session) "avg-left-pp-start")
  (check-pred number? (session-avg-left-pp-end session) "avg-left-pp-end")
  (check-pred number? (session-avg-right-pp-start session) "avg-right-pp-start")
  (check-pred number? (session-avg-right-pp-end session) "avg-right-pp-end")
  (check-pred number? (session-avg-left-ppp-start session) "avg-left-ppp-start")
  (check-pred number? (session-avg-left-ppp-end session) "avg-left-ppp-end")
  (check-pred number? (session-avg-right-ppp-start session) "avg-right-ppp-start")
  (check-pred number? (session-avg-right-ppp-end session) "avg-right-ppp-end")

  ;; The avg CYD fields must be present for each lap
  (for ([lap (in-list (session-laps session))])
    (check-pred number? (lap-avg-left-pco lap) "avg-left-pco")
    (check-pred number? (lap-avg-right-pco lap) "avg-right-pco")
    (check-pred number? (lap-avg-left-pp-start lap) "avg-left-pp-start")
    (check-pred number? (lap-avg-left-pp-end lap) "avg-left-pp-end")
    (check-pred number? (lap-avg-right-pp-start lap) "avg-right-pp-start")
    (check-pred number? (lap-avg-right-pp-end lap) "avg-right-pp-end")
    (check-pred number? (lap-avg-left-ppp-start lap) "avg-left-ppp-start")
    (check-pred number? (lap-avg-left-ppp-end lap) "avg-left-ppp-end")
    (check-pred number? (lap-avg-right-ppp-start lap) "avg-right-ppp-start")
    (check-pred number? (lap-avg-right-ppp-end lap) "avg-right-ppp-end"))

  )

(define cyd-tests
  (test-suite
   "Cycling Dynamics"

   (test-case "Cycling dynamics import / export"
     (with-fresh-database
       (lambda (db)
         (db-import-activity-from-file/check a9 db #:basic-checks-only? #t)
         (check = 1 (activity-count db))
         (define session-id 1)
         (cyd-check-session-values session-id db)
         (cyd-check-lap-values session-id db)
         (cyd-check-track-values session-id db)
         (let ((session (db-fetch-session session-id db)))
           (cyd-check-retrieved-session session)))))))

(define db-patch-26-tests
  (test-suite
   "Database Patch 26"
   (test-case "V_SPORT_ZONE_FOR_SESSION and V_CRITICAL_POWER_FOR_SESSION"
     (with-fresh-database
       (lambda (db)
         ;; Put some sport zones and CP values in the database.  We don't care
         ;; about the actual values, only their validity dates
         (query-exec
          db
          "insert into SPORT_ZONE(valid_from, sport_id, sub_sport_id, zone_metric_id)
           values (strftime('%s', '2015-01-01'), 2, null, 3)")
         (query-exec
          db
          "insert into SPORT_ZONE(valid_from, sport_id, sub_sport_id, zone_metric_id)
           values (strftime('%s', '2016-01-01'), 2, null, 3)")
         (query-exec
          db
          "insert into SPORT_ZONE(valid_from, sport_id, sub_sport_id, zone_metric_id)
           values (strftime('%s', '2017-01-01'), 2, null, 3)")
         (query-exec
          db
          "insert into CRITICAL_POWER(valid_from, sport_id, sub_sport_id, cp, wprime, tau)
           values (strftime('%s', '2015-01-01'), 2, null, 100, 100000, 300)")
         (query-exec
          db
          "insert into CRITICAL_POWER(valid_from, sport_id, sub_sport_id, cp, wprime, tau)
           values (strftime('%s', '2016-01-01'), 2, null, 110, 110000, 300)")
         (query-exec
          db
          "insert into CRITICAL_POWER(valid_from, sport_id, sub_sport_id, cp, wprime, tau)
           values (strftime('%s', '2017-01-01'), 2, null, 120, 120000, 300)")

         ;; Put an indoor cycling session in the database "sub_sport_id = 6"
         (query-exec
          db
          "insert into A_SESSION(name, start_time, sport_id, sub_sport_id)
           values ('a1, indoor cycling', strftime('%s', '2016-06-01'), 2, 6)")

         ;; Put a cycling session in the database "sub_sport_id = null"
         (query-exec
          db
          "insert into A_SESSION(name, start_time, sport_id, sub_sport_id)
           values ('a1 cycling', strftime('%s', '2016-06-01'), 2, null)")

         ;; Check that the indoor cycling session has a sport zone and a CP zone assigned
         (check-eq? 1 (length (query-list db "
select S.id
from A_SESSION S, V_SPORT_ZONE_FOR_SESSION SZFS
where S.id = SZFS.session_id
  and S.sport_id = 2
  and S.sub_sport_id = 6")))

         (check-eq? 1 (length (query-list db "
select S.id
from A_SESSION S, V_CRITICAL_POWER_FOR_SESSION CPFS
where S.id = CPFS.session_id
  and S.sport_id = 2
  and S.sub_sport_id = 6")))

         ;; Check that the cycling session has a sport zone and a CP zone assigned
         (check-eq? 1 (length (query-list db "
select S.id
from A_SESSION S, V_SPORT_ZONE_FOR_SESSION SZFS
where S.id = SZFS.session_id
  and S.sport_id = 2
  and S.sub_sport_id is null")))

         (check-eq? 1 (length (query-list db "
select S.id
from A_SESSION S, V_CRITICAL_POWER_FOR_SESSION CPFS
where S.id = CPFS.session_id
  and S.sport_id = 2
  and S.sub_sport_id is null")))

         )))))


;;............................................................. workouts ....

(define (check-steps-match steps1 steps2)
  (check = (length steps1) (length steps2) "check-steps-match: length mismatch")
  (for ([s1 (in-list steps1)] [s2 (in-list steps2)])
    (cond
      ((wkstep? s1)
       (check-pred wkstep? s2 "check-steps-match: not a wkstep")
       (match-define (wkstep type1 duration1 dval1 target1 tlow1 thigh1 ramp1?) s1)
       (match-define (wkstep type2 duration2 dval2 target2 tlow2 thigh2 ramp2?) s1)
       (check eq? type1 type2 "check-steps-match: type mismatch")
       (check eq? duration1 duration2 "check-steps-match: duration type mismatch")
       (check eqv? dval1 dval2 "check-steps-match: duration value mismatch")
       (check eq? target1 target2 "check-steps-match: target type mismatch")
       (if (eq? target1 'open)
           (begin
             (check eqv? dval1 dval2 "check-steps-match: tlow mismatch")
             (check eqv? dval1 dval2 "check-steps-match: thigh mismatch"))
           (begin
             (check < (abs (- tlow1 tlow2)) 0.001 "check-steps-match: tlow A mismatch")
             (check < (abs (- thigh1 thigh2)) 0.001 "check-steps-match: tlow A mismatch"))))
      ((wkrepeat? s1)
       (check-pred wkrepeat? s2 "check-steps-match: not a repeat")
       (match-define (wkrepeat times1 steps1) s1)
       (match-define (wkrepeat times2 steps2) s2)
       (check eqv? times1 times2 "check-steps-match: times mismatch")
       (check-steps-match steps1 steps2)))))

(define sample-wk
  (workout
   "sample workout"
   "sample description"
   'running
   #f                            ; serial number will be created when stored
   #f                            ; timestamp will be created when stored
   (list
    (wkstep 'warmup 'distance 1000 'open #f #f #f)
    (wkrepeat
     5
     (list
      (wkstep 'active 'time 120 'heart-rate 100 160 #f)
      (wkstep 'recover 'open #f 'speed 4.2 4.6 #f)
      (wkstep 'rest 'time 180 'power 200 300 #f)))
    (wkstep 'cooldown 'distance 2000 'power-ftp-pct 10 150 #f))))

(define (check-workout-store-fetch-delete db)

  (define-values (workout-id version-id)
    ;; NOTE: library-id 1 is created by the db-schema.sql
    (store-workout db sample-wk 1))

  (define-values (workout-id1 version-id1)
    ;; another workout will be stored, since we don't have a serial number.
    (store-workout db sample-wk 1))

  ;; two workouts in the database
  (check = 2 (query-value db "select count(*) from WORKOUT") "check #1")

  (define wk2 (fetch-workout db workout-id))

  (match-define (workout name1 desc1 sport1 serial1 timestamp1 steps1) sample-wk)
  (match-define (workout name2 desc2 sport2 serial2 timestamp2 steps2) wk2)

  ;; The workout should have a serial number and a timestamp
  (check-pred number? serial2 "Expecting a workout serial number")
  (check-pred number? timestamp2 "Expecting a workout timestamp")

  (check string=? name1 name2 "Workout names mismatch")
  (check string=? desc1 desc2 "Workout description mismatch")
  (check-steps-match steps1 steps2)

  ;; mark the first version of this workout as exported
  (query-exec
   db
   "update WORKOUT_VERSION set is_exported = 1 where id = ?"
   version-id)

  (define wk3 (struct-copy workout wk2 [timestamp (+ (current-seconds) 10)]))
  ;; will create a new version of the workout
  (store-workout db wk3 1)

  ;; still two workouts in the database
  (check = 2 (query-value db "select count(*) from WORKOUT") "check #2")
  ;; .. but the first workout has two versions
  (let ((nversions (query-value db "select count(*) from WORKOUT_VERSION where workout_id = ?" workout-id)))
    (check = 2 nversions "check #3"))
  ;; second workout still has a version, we did not delete it by mistake...
  (check = 1 (query-value
              db "select count(*) from WORKOUT_VERSION where workout_id = ?"
              workout-id1)
         "check #4")

  ;; Store this as a new workout
  (store-workout db wk3 1 #:may-replace-serial? #t)
  (check = 3 (query-value db "select count(*) from WORKOUT") "check #5")
  ;; wk2 has two versions, the other 2 just one
  (check = 4 (query-value db "select count(*) from WORKOUT_VERSION") "check #6")
  (delete-workout db workout-id)

  ;; two workouts left
  (check = 2 (query-value db "select count(*) from WORKOUT") "check #7")
  ;; ... with one version each
  (check = 2 (query-value db "select count(*) from WORKOUT_VERSION") "check #8")

  (define-values (workout-id2 version-id2)
    ;; a fresh workout will be stored, since we don't have a serial number.
    (store-workout db sample-wk 1))

  ;; Workout is not marked as exported
  (check = 0 (query-value db "select is_exported from WORKOUT_VERSION where id = ?"
                          version-id2))
  ;; Simply fetching the workout will not cause it to become exported...
  (fetch-workout db workout-id2)
  (check = 0 (query-value db "select is_exported from WORKOUT_VERSION where id = ?"
                          version-id2))
  ;; ... unless we explicitly ask for it.
  (fetch-workout db workout-id2 #:for-export? #t)
  (check = 1 (query-value db "select is_exported from WORKOUT_VERSION where id = ?"
                          version-id2))

  )

(define (check-workout-fit-generation)

  (define wk (struct-copy workout sample-wk
                          [serial 100]
                          [timestamp (current-seconds)]))
  (define data (workout->fit wk))
  (define wk1 (fit->workout data))

  (match-define (workout name1 desc1 sport1 serial1 timestamp1 steps1) wk)
  (match-define (workout name2 desc2 sport2 serial2 timestamp2 steps2) wk1)

  (check string=? name1 name2)
  ;; note: description is not stored in the FIT file and as such it is lost.
  (check eq? sport1 sport2)
  (check eqv? serial1 serial2)
  (check eqv? timestamp1 timestamp1)
  (check-steps-match steps1 steps2))

(define workouts-tests-suite
  (test-suite
   "Workouts"
   (test-case "Workout store-fetch-delete"
     (with-fresh-database
       (lambda (db)
         (check-not-exn
          (lambda ()
            (check-workout-store-fetch-delete db))))))
   (test-case "FIT file serialize / deserialize"
     (check-not-exn
      check-workout-fit-generation))))

(define sport-zone-test-suite
  (test-suite
   "Sport Zones"

   (test-case "Get Sport Zones/Empty"
     (with-fresh-database
       (lambda (db)
         (for ((sport (in-list (query-list db "select id from E_SPORT"))))
           (for ((sub-sport (in-list (cons #f (query-list db "select id from E_SUB_SPORT")))))
             (for ((metric (in-list '(heart-rate pace power))))
               (check-false (sport-zones-for-sport sport sub-sport metric))))))))

   (test-case "Get Sport Zones For Sport"
     (with-fresh-database
       (lambda (db)
         (fill-sport-zones db #:valid-from (- (current-seconds) 3600))
         (check-pred sz? (sport-zones-for-sport 1 #f 'heart-rate))
         (check-pred sz? (sport-zones-for-sport 2 #f 'heart-rate))
         (check-pred sz? (sport-zones-for-sport 2 #f 'power)))))

   (test-case "Get Sport Zones For Session"
     (with-fresh-database
       (lambda (db)
         (db-import-activity-from-file/check a2 db #:basic-checks-only? #t)
         ;; No sport zones yet
         (check-false (sport-zones-for-session 1 'heart-rate #:database db))
         ;; Put in sport zones which are after the imported session
         (db-put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220) #:valid-from (current-seconds))
         ;; Still no sport zones
         (check-false (sport-zones-for-session 1 'heart-rate #:database db))
         ;; Put in sport zones which cover the imported session
         (db-put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220) #:valid-from 1)
         ;; We should find sport zones for this activity
         (check-pred sz? (sport-zones-for-session 1 'heart-rate #:database db)))))

   (test-case "Delete Sport Zones By Id"
     (with-fresh-database
       (lambda (db)
         (db-put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220) #:valid-from (current-seconds))
         (define z (sport-zones-for-sport 1 #f 'heart-rate))
         (check-pred sz? z)
         (delete-sport-zones (sz-id z) #:database db)
         ;; They should be no more
         (check-false (sport-zones-for-sport 1 #f 'heart-rate)))))

   (test-case "Delete Sport Zones"
     (with-fresh-database
       (lambda (db)
         (db-put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220) #:valid-from (current-seconds))
         (define z (sport-zones-for-sport 1 #f 'heart-rate))
         (check-pred sz? z)
         (delete-sport-zones z #:database db)
         ;; They should be no more
         (check-false (sport-zones-for-sport 1 #f 'heart-rate)))))

   (test-case "Sport Zones From Threshold"
     (define sample-zones
       '(("One" absolute 23)
         ("Two" percent 0.5)
         ("Three" percent 1.1)))
     (define sz (sport-zones-from-threshold 1 #f 'heart-rate 180 sample-zones #:valid-from 1000))

     (match-define (vector one two three) (sz-boundaries sz))
     (check-= one 23 1.0)
     (check-= two 90 1.0)
     (check-= three 198 1.0)
     (check-equal? (sz-names sz) #("One" "Two" "Three"))
     (check-equal? (sz-valid-from sz) 1000)
     (check-false (sz-valid-until sz)))

   ))


;;.....................................................................  ....

(define db-tests
  (test-suite
   "Database Operations"

   ;; This should catch any problems with db-schema.sql
   (test-case "Create fresh database"
     (check-not-exn
      (lambda () (disconnect (open-activity-log 'memory)))))

   ;; This should catch basic sport zone issues early on
   sport-zone-test-suite

   (test-case "Importing first activity"
     (for ((file (in-list (list a1 a2 a3 a4 a5 a6 a7 a8))))
       (with-fresh-database
         (lambda (db)
           (fill-sport-zones db #:valid-from 1)
           (printf "About to import ~a~%" file)(flush-output)
           (db-import-activity-from-file/check
            file db
            #:extra-df-checks
            (lambda (df)
              (define sid (df-get-property df 'session-id))
              (define ts (query-value
                          db
                          "select start_time from A_SESSION where id = ?"
                          sid))
              ;; Add a new set of time zones, which affect this session, than
              ;; re-calculate the metrics.  `check-time-in-zone' will verify
              ;; that there are no duplicate TIZ information for the same
              ;; metric
              (fill-sport-zones db #:valid-from (- ts 100))
              (update-some-session-metrics sid db)
              (check-time-in-zone df db file)))
           (check = 1 (activity-count db))
           (db-check-tile-code db)))))

   (test-case "Subsequent imports"
     (with-fresh-database
       (lambda (db)
         (fill-sport-zones db #:valid-from 1)
         (for ((file (in-list (list a1 a2 a3 a4 a5 a6 a7 a8))))
           (printf "About to import ~a~%" file)(flush-output)
           (db-import-activity-from-file/check file db))
         (check = 8 (activity-count db)))))

   (test-case "Import manual activity"
     (with-fresh-database
       (lambda (db)
         (check-not-exn
          (lambda ()
            (printf "Checking session-df for manual activity~%")(flush-output)
            (let* ((sid (db-import-manual-session db))
                   (df (session-df db sid)))
              ;; We don't expect much in a session df for a manual session, but
              ;; we should at least be able to read it.
              (check-eqv? (df-row-count df) 0)))))))

   db-patch-26-tests
   cyd-tests
   workouts-tests-suite
   ))

(module+ test
  (run-tests #:package "db-test"
             #:results-file "test-results/db-test.xml"
             db-tests))

;;(test/gui db-tests)
