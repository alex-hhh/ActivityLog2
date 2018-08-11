#lang racket/base

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         rackunit
         "../rkt/data-frame/df.rkt"
         "../rkt/database.rkt"
         "../rkt/dbapp.rkt"
         "../rkt/import.rkt"
         "../rkt/intervals.rkt"
         "../rkt/session-df.rkt")

(define (with-database thunk)
  (let ((db (open-activity-log 'memory)))
    (set-current-database db)
    (clear-session-df-cache)
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
  (for ([s (in-list sn)] #:unless (member s '("stride" "pace")))
    (check-true (df-has-non-na? df s) (format "empty series found: ~a" s)))
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
    (let ((sport (df-get-property df 'sport))
          (sid (df-get-property df 'session-id)))
      (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
        ;; Check that we have some time-in-zone data from the session
        (let ((nitems (query-value db "select count(*) from TIME_IN_ZONE where session_id = ?" sid)))
          (check > nitems 0
                 (format "TIZ not present for ~a ~a" sport file)))))))

;; Check that we can obtain intervals from a data frame.  For now, we only
;; check if the code runs without throwing any exceptions.
(define (check-intervals df)
  (let ((sport (df-get-property df 'sport)))
    (when (or (eq? (vector-ref sport 0) 1) (eq? (vector-ref sport 0) 2))
      (make-split-intervals df "elapsed" (* 5 60)) ; 5 min splits
      ;; (printf "make-split-intervals elapsed done~%")(flush-output)
      (make-split-intervals df "dst" 1600)         ; 1 mile splits
      ;; (printf "make-split-intervals dst done~%")(flush-output)
      (make-climb-intervals df)
      ;; (printf "make-climb-intervals done~%")(flush-output)
      ;; (printf "sport: ~a~%" sport)(flush-output)
      (if (eq? (vector-ref sport 0) 1)
          (begin
            (make-best-pace-intervals df)
            ;; (printf "make-best-pace-intervals done~%")(flush-output))
            )
          (begin
            (make-best-power-intervals df)
            ;; (printf "make-best-power-intervals done~%")(flush-output))
            )))))

(define (db-import-activity-from-file/check file db
                                            #:basic-checks-only? (bc #f)
                                            #:expected-row-count (rc #f)
                                            #:expected-series-count (sc #f)
                                            #:extra-df-checks (df-check #f))
  (check-not-exn
   (lambda ()
     (let ((result (db-import-activity-from-file file db)))
       (check-pred cons? result "Bad import result format")
       (check-eq? (car result) 'ok (format "~a" (cdr result)))
       (unless bc
         ;; Do some extra checks on this imported file
         (do-post-import-tasks db #;(lambda (msg) (printf "~a~%" msg) (flush-output)))
         ;; (printf "... done with the post import tasks~%")(flush-output)
         (for ((sid (aid->sid (cdr result) db)))
           (let ((df (session-df db sid)))
             ;; (printf "got the session~%")(flush-output)
             (check-session-df df
                               #:expected-row-count rc
                               #:expected-series-count sc)
             ;; (printf "check session done~%")(flush-output)
             (check-intervals df)
             ;; (printf "check intervals done~%")(flush-output)
             (check-time-in-zone df db file)
             (when df-check
               (df-check df))
             ;;(printf "check time-in-zone done~%")
             )))))))

(provide with-database db-import-activity-from-file/check)
