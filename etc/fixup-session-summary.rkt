#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; fixup-session-summary.rkt -- rebuild session summary data from trackpoint data
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;; Commentary

;; this file contains routines to rebuild session and lap summary data (total
;; distance, average speed, etc) from individual track point recordings.  It
;; can be used to fixup an activity where the device recorded correctly, but
;; something went wrong while calculating summary values and finalizing the
;; FIT file.  The function to use is `fixup-session-summary-from-data`
;; (put-pref 'activity-log:database-file "path-to-database")

(require racket/match
         "../rkt/database.rkt"
         "../rkt/intervals.rkt"
         "../rkt/models/coggan.rkt"
         "al-interactive.rkt")


(define (fixup-start-time db sid df)
  (define start-time
    (for/first ([(timestamp) (in-data-frame df "timestamp")]
                #:when (rational? timestamp))
      timestamp))
  (if start-time
      (begin
        (query-exec
         db
         "update A_SESSION
             set start_time = ?
           where id = ?"
         start-time sid)
        (query-exec
         db
         "update ACTIVITY
             set start_time = ?
           where id in (select activity_id from A_SESSION where id = ?)"
         start-time sid))
      (printf "No start-time found in data-frame")))

(define (fixup-tss-and-if db sid df #:ftp ftp)
  (define cg (cg-metrics df #:ftp ftp))
  (query-exec
   db
   "update A_SESSION
       set training_stress_score = ?, intensity_factor = ?
     where id = ?"
   (cg-tss cg) (cg-if cg) sid))

(define (fixup-session-summary db sid df #:ftp ftp)
  (define summary (make-interval-summary df 0 (df-row-count df) #:ftp ftp))
  (define ssid (db-insert-section-summary summary db))
  (query-exec
   db
   "delete from SECTION_SUMMARY
     where id in (select summary_id
                   from A_SESSION
                  where id = ?)"
   sid)
  (query-exec
   db
   "update A_SESSION
       set summary_id = ?
     where id = ?"
   ssid
   sid))

(define (fixup-lap-summary db sid df #:ftp ftp)
  (define laps
    (query-rows
     db
     "select id,
             (select min(T.timestamp)
                from A_TRACKPOINT T, A_LENGTH L
               where T.length_id = L.id
                 and L.lap_id = P.id) as start,
             (select max(T.timestamp)
                from A_TRACKPOINT T, A_LENGTH L
               where T.length_id = L.id
                 and L.lap_id = P.id)  as end
       from A_LAP P
      where P.session_id = ?"
    sid))
  (for ([lap (in-list laps)])
    (match-define (vector lid start-timestamp end-timestamp) lap)
    (match-define (list start-index end-index)
      (df-index-of* df "timestamp" start-timestamp end-timestamp))
    (define summary
      (make-interval-summary df start-index (add1 end-index) #:ftp ftp))
    (define ssid (db-insert-section-summary summary db))
    (query-exec
     db
     "delete from SECTION_SUMMARY
     where id in (select summary_id
                   from A_LAP
                  where id = ?)"
     lid)
    (query-exec
     db
     "update A_LAP
       set summary_id = ?
     where id = ?"
     ssid
     lid)))

(define (fixup-session-summary-from-data sid #:ftp ftp)
  (define df (sid->df sid))
  (unless df
    (error (format "No such session id: ~a" sid)))
  (define db (current-database))
  (call-with-transaction
   db
   (lambda ()
     (fixup-start-time db sid df)
     (when (df-contains? df "pwr")
       (fixup-tss-and-if db sid df #:ftp ftp))
     (fixup-session-summary db sid df #:ftp ftp)
     (fixup-lap-summary db sid df #:ftp ftp))))
