#lang racket/base

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018, 2019, 2021, 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/list
         racket/class
         rackunit
         tzgeolookup
         data-frame
         "../rkt/database.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/dbutil.rkt"
         "../rkt/import.rkt"
         "../rkt/intervals.rkt"
         "../rkt/session-df/session-df.rkt"
         "../rkt/session-df/series-metadata.rkt"
         "../rkt/sport-charms.rkt"
         ;; Even though we don't need this, native series are not registered
         ;; unless this module is required somewhere...
         "../rkt/session-df/native-series.rkt"
         "../rkt/fit-file/activity-util.rkt")

(define (with-fresh-database thunk)
  (let ((db (open-activity-log 'memory)))
    (set-current-database db)
    (dynamic-wind
      (lambda () (void))
      ;; NOTE: cannot really catch errors as error trace will loose context
      (lambda () (thunk db))
      (lambda () (disconnect db)))))

(define (with-database path thunk)
  (let ((db (open-activity-log path)))
    (set-current-database db)
    (dynamic-wind
      (lambda () (void))
      ;; NOTE: cannot really catch errors as error trace will loose context
      (lambda () (thunk db))
      (lambda () (disconnect db)))))

(define (aid->sid aid db)
  ;; NOTE: there might be multiple session ID's for each activity ID
  ;; (multisport sessions)
  (query-list db "select id from A_SESSION where activity_id = ?" aid))

;; Do some basic checks on the session data frame
(define (check-session-df df
                          #:expected-row-count (rc #f)
                          #:expected-series-count (sc #f))
  (define nitems (df-row-count df))
  (if rc
      (check = nitems rc)
      (check > nitems 0))               ; must have some data
  (define sn (df-series-names df))
  (when sc
    (check = (length sn) sc))
  ;; All data series must contain at least one data point of valid data
  ;; (make-session-data-frame is supposed to remove empty series, like gct for
  ;; a cycling session)
  ;;
  ;; Skip series that are lazily created and thus might still be empty, like
  ;; stride and pace.
  (for ([s (in-list sn)] #:unless (member s '("stride" "pace" "torque")))
    (check-true (df-has-non-na? df s) (format "empty series found: ~a" s)))
  ;; Check that metadata objects exist for all series (except a select few).
  ;; This is especially important since metadata objects for XDATA series are
  ;; created when the first such series is read in.
  (define lap-swim? (df-get-property df 'is-lap-swim?))
  (for ([s (in-list sn)]
        #:unless (member s (if lap-swim?
                               '("timestamp" "timer" "duration" "swim_stroke" "speed"
                                             "spd" "dst" "tempe" "active")
                               '("timestamp" "lat" "lon" "dst" "spd" "gaspd" "tempe"))))
    (define metadata (find-series-metadata s lap-swim?))
    (check-true (is-a? metadata series-metadata%) (format "missing metadata for ~a" sn)))
  ;; Remove the common series
  (set! sn (remove "timestamp" sn))
  (set! sn (remove "timer" sn))
  (set! sn (remove "elapsed" sn))
  (set! sn (remove "dst" sn))
  ;; Check that we still got some actual data series left
  (check > (length sn) 0 "no meaningful series in session data frame"))

(define tiz-query
  "select distinct SZ.id, SZ.zone_metric_id
     from TIME_IN_ZONE TIZ,
          SPORT_ZONE SZ
    where TIZ.sport_zone_id = SZ.id
      and TIZ.session_id = ?
  group by SZ.id, SZ.zone_metric_id")

;; Check that TIME-IN-ZONE data has been stored in the database for this
;; session.
(define (check-time-in-zone df db file)
  ;; Only check TIZ data if sport zones have been added to the database
  (when (> (query-value db "select count(*) from SPORT_ZONE") 0)
    (let ((sport (df-get-property df 'sport))
          (sid (df-get-property df 'session-id)))
      (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
        ;; Check that we have some time-in-zone data from the session

        (let ((zones (query-rows db tiz-query sid)))
          (check > (length zones) 0
                 (format "TIZ not present for ~a ~a" sport file))
          (define zmetrics (for/list ([r (in-list zones)]) (vector-ref r 1)))
          ;; Check that there is a single set of TIZ data for each metric, we
          ;; had a bug which created duplicate entries when updating zones.
          (check equal? zmetrics (remove-duplicates zmetrics)
                 (format "Duplicate ZONE metrics: ~a for sid ~a of ~a"
                         zones sid file)))))))

;; Check that we can obtain intervals from a data frame.  For now, we only
;; check if the code runs without throwing any exceptions.
(define (check-intervals df ftp)
  (let ((sport (df-get-property df 'sport)))
    (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
      (make-split-intervals df "elapsed" (* 5 60) #:ftp ftp) ; 5 min splits
      ;; (printf "make-split-intervals elapsed done~%")(flush-output)
      (make-split-intervals df "dst" 1600 #:ftp ftp)         ; 1 mile splits
      ;; (printf "make-split-intervals dst done~%")(flush-output)
      (make-climb-intervals df #:ftp ftp)
      ;; (printf "make-climb-intervals done~%")(flush-output)
      ;; (printf "sport: ~a~%" sport)(flush-output)
      (if (eq? (vector-ref sport 0) 1)
          (begin
            (make-best-pace-intervals df #:ftp ftp)
            ;; (printf "make-best-pace-intervals done~%")(flush-output))
            )
          (begin
            (make-best-power-intervals df #:ftp ftp)
            ;; (printf "make-best-power-intervals done~%")(flush-output))
            )))))

;; Check that the time zone that was stored on import matches the time zone we
;; determine manually for this data frame -- we only check that the import
;; task determined the time zone and stored it into the database -- the
;; tzgeolookup package has unit tests for the correctness of the lookup
;; itself.
(define (check-time-zone df db)
  (define sid (df-get-property df 'session-id))
  (define db-tz
    (query-maybe-value db "
select ETZ.name
from E_TIME_ZONE ETZ, A_SESSION S
where S.time_zone_id = ETZ.id
  and S.id = ?" sid))
  (define df-tz
    (and (df-contains? df "lat" "lon")
         (for/first ([(lat lon) (in-data-frame df "lat" "lon")] #:when (and lat lon))
           (lookup-timezone lat lon))))
  (define s-tz
    (session-time-zone (db-fetch-session sid db)))
  #;(printf "timezones df = ~a, db = ~a~%, session = ~a" df-tz db-tz s-tz)
  (check-equal? db-tz df-tz "Failed to import time zone (db)")
  (check-equal? s-tz df-tz "Failed to import time zone (session read)"))

;; NOTE: this query will need to be updated when new tables reference the
;; SECTION_SUMMARY table...
(define (leaked-section-summaries db)
  (query-list
   db
"select SS.id
  from SECTION_SUMMARY SS
 where SS.id not in (select summary_id from A_SESSION)
   and SS.id not in (select summary_id from A_LAP)
   and SS.id not in (select summary_id from A_LENGTH)
   and SS.id not in (select summary_id from XDATA_SUMMARY_VALUE)
   and SS.id not in (select summary_id from GPS_SEGMENT_MATCH)"))

(define (db-import-activity-from-file/check file db
                                            #:basic-checks-only? (bc #f)
                                            #:expected-row-count (rc #f)
                                            #:expected-series-count (sc #f)
                                            #:expected-session-count (nsessions #f)
                                            #:extra-db-checks (db-check #f)
                                            #:extra-df-checks (df-check #f)
                                            #:delete-sessions? (delete? #f))
  (check-pred null? (leaked-section-summaries db)
              "Having leaked SECTION_SUMMARY entries before import")
  (define sport-charms (new sport-charms% [dbc db]))
  (let ((result (db-import-activity-from-file file db)))
    (check-pred cons? result "Bad import result format")
    (check-eq? (car result) 'ok (format "~a" (cdr result)))
    (unless bc
      ;; Do some extra checks on this imported file
      (do-post-import-tasks db sport-charms #;(lambda (msg) (printf "~a~%" msg) (flush-output)))
      ;; (printf "... done with the post import tasks~%")(flush-output)
      (when db-check
        (db-check db))
      (define sids (aid->sid (cdr result) db))
      (when nsessions
        (check = (length sids) nsessions))
      (for ((sid (in-list sids))
            (expected-row-count (if (list? rc) (in-list rc) (in-cycle (in-value rc))))
            (expected-series-count (if (list? sc) (in-list sc) (in-cycle (in-value sc)))))
        (let ((df (session-df db sid)))
          (check-session-df df
                            #:expected-row-count expected-row-count
                            #:expected-series-count expected-series-count)
          (check-intervals df (send sport-charms get-athlete-ftp))
          (check-time-in-zone df db file)
          (check-time-zone df db)
          (when df-check
            (df-check df))
          ))
      (when delete?
        (for ([sid (in-list sids)])
          (check-not-exn (lambda () (db-delete-session sid db)))
          (check-false (query-maybe-value db "select id from A_SESSION where id = ?" sid))
          (check-pred null? (leaked-section-summaries db)
                      "Leaking SECTION_SUMMARY entries after deleting session"))))))

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
       (define ssid
         (db-insert
          db
          "insert into SECTION_SUMMARY(total_timer_time, total_elapsed_time, total_distance, avg_speed)
             values(?, ?, ?, ?)"
          (or duration sql-null)
          (or duration sql-null)
          (or distance sql-null)
          (or avg-speed sql-null)))
       (define aid (db-insert db "insert into ACTIVITY(start_time) values (?)" start-time))
       (db-insert
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
        ssid)))))

(provide with-fresh-database
         with-database
         db-import-activity-from-file/check
         check-time-in-zone
         leaked-section-summaries
         db-import-manual-session)
