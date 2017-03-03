#lang racket
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


(require rackunit)
;;(require rackunit/gui)
(require rackunit/text-ui)
(require db)
(require "../rkt/dbapp.rkt")
(require "../rkt/database.rkt")
(require "../rkt/sport-charms.rkt")
(require "../rkt/fit-file.rkt")
(require "../rkt/activity-util.rkt")
(require "../rkt/utilities.rkt")
(require "../rkt/import.rkt")
(require "../rkt/session-df.rkt")
(require "../rkt/intervals.rkt")


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

(define (with-database thunk)
  (let ((db (open-activity-log 'memory)))
    (set-current-database db)
    (clear-session-df-cache)
    ;; NOTE: cannot really catch errors as error trace will loose context
    (thunk db)
    (disconnect db)))

(define (aid->sid aid db)
  ;; NOTE: there might be multiple session ID's for each activity ID
  ;; (multisport sessions)
  (query-list db "select id from A_SESSION where activity_id = ?" aid))

;; Do some basic checks on the session data frame
(define (check-session-df df)
  (check > (send df get-row-count) 0)   ; must have some data
  (define sn (send df get-series-names))
  ;; Remove the common series
  (set! sn (remove "timestamp" sn))
  (set! sn (remove "timer" sn))
  (set! sn (remove "elapsed" sn))
  (set! sn (remove "dst" sn))
  ;; Check that we still got some actual data series left
  (check > (length sn) 0 "no meaningful series in session data frame"))

;; Check that TIME-IN-ZONE data has been stored in the database for this
;; session.
(define (check-time-in-zone df db file)
  ;; Only check TIZ data if sport zones have been added to the database
  (when (> (query-value db "select count(*) from SPORT_ZONE") 0)
    (let ((sport (send df get-property 'sport))
          (sid (send df get-property 'session-id)))
      (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
        ;; Check that we have some time-in-zone data from the session
        (let ((nitems (query-value db "select count(*) from TIME_IN_ZONE where session_id = ?" sid)))
          (check > nitems 0
                 (format "TIZ not present for ~a ~a" sport file)))))))

;; Check that we can obtain intervals from a data frame.  For now, we only
;; check if the code runs without throwing any exceptions.
(define (check-intervals df)
  (let ((sport (send df get-property 'sport)))
    (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
      (make-split-intervals df "elapsed" (* 5 60)) ; 5 min splits
      (make-split-intervals df "dst" 1600)         ; 1 mile splits
      (make-climb-intervals df)
      (if (eq? (vector-ref sport 0) 1)
          (make-best-pace-intervals df)
          (make-best-power-intervals df)))))
  
(define (db-import-activity-from-file/check file db (basic-checks-only #f))
  (check-not-exn
   (lambda ()
     (let ((result (db-import-activity-from-file file db)))
       ;; (lambda (msg) (printf "post import: ~a~%" msg)))
       (check-pred cons? result "Bad import result format")
       (check-eq? (car result) 'ok (format "~a" (cdr result)))
       (unless basic-checks-only
         ;; Do some extra checks on this imported file
         (do-post-import-tasks db)
         (for ((sid (aid->sid (cdr result) db)))
           (let ((df (session-df db sid)))
             (check-session-df df)
             (check-intervals df)
             (check-time-in-zone df db file))))))))

(define (db-check-tile-code db)
  (let ((cnt (query-value db "
select count(*)
  from A_TRACKPOINT
 where tile_code is null
   and (position_lat is not null
        or position_long is not null)")))
    (check = 0 cnt "Missing tile codes from A_TRACKPOINT")))

(define (fill-sport-zones db)
  (put-sport-zones 2 #f 1 '(60 130 140 150 160 170 220))
  (put-sport-zones 1 #f 1 '(60 130 140 150 160 170 220))
  (put-sport-zones 2 #f 3 '(-1 0 100 140 180 220 230 250 600))
  ;; put-sport-zones will setup sport zones to be valid from the time or their
  ;; call.  Make all sport zones valid from the "beginning of time"
  (query-exec db "update sport_zone set valid_from = 0"))



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
     (with-database
       (lambda (db)
         (db-import-activity-from-file/check a9 db #t) ; only basic checks here
         (check = 1 (activity-count db))
         (define session-id 1)
         (cyd-check-session-values session-id db)
         (cyd-check-lap-values session-id db)
         (cyd-check-track-values session-id db)
         (let ((session (db-fetch-session session-id db)))
           (cyd-check-retrieved-session session)))))))


;;.....................................................................  ....

(define db-tests
  (test-suite
   "Basic database tests"

   (test-case
    "Create fresh database"
    ;; This should catch any problems with db-schema.sql
    (check-not-exn
     (lambda () (disconnect (open-activity-log 'memory)))))

   (test-case "Importing first activity"
     (for ((file (in-list (list a1 a2 a3 a4 a5 a6 a7 a8))))
       (with-database
         (lambda (db)
           (fill-sport-zones db)
           (printf "About to import ~a~%" file)
           (db-import-activity-from-file/check file db)
           (check = 1 (activity-count db))
           (db-check-tile-code db)))))

   (test-case "Subsequent imports"
     (with-database
       (lambda (db)
         (fill-sport-zones db)
         (for ((file (in-list (list a1 a2 a3 a4 a5 a6 a7 a8))))
           (printf "About to import ~a~%" file)
           (db-import-activity-from-file/check file db))
         (check = 8 (activity-count db)))))

   (test-case "Get Sport Zones"
     (with-database
       (lambda (db)
         (for ((sport (in-list (query-list db "select id from E_SPORT"))))
           (for ((sub-sport (in-list (cons #f (query-list db "select id from E_SUB_SPORT")))))
             ;; No Sport zones are defined
             (check-false (get-sport-zones sport sub-sport 1)))))))

   cyd-tests
   ))

(module+ test
  (run-tests db-tests))

;; (test/gui db-tests)
