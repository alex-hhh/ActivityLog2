#lang racket/base

;; power-spikes.rkt -- clear power spikes and re-calculate power related
;; metrics.
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "al-interactive.rkt"
         "../rkt/sport-charms.rkt"
         "../rkt/models/coggan.rkt"
         "../rkt/time-in-zone.rkt"
         math/statistics
         racket/match
         data/queue)

;; Return a mapping of timestamp to trackpoint ID for every trackpoint in the
;; session SID.
(define (get-timestamp-mapping sid)
  (define trackpoint-id-sql
    "select T.id as id,
            T.timestamp as timestamp
       from A_TRACKPOINT T, A_LENGTH L, A_LAP P
      where T.length_id = L.id
        and L.lap_id = P.id
        and P.session_id = ?")

  (for/hash (([id timestamp] (in-query (current-database) trackpoint-id-sql sid)))
    (values timestamp id)))

;; Return a mapping of timestamp to lap section summary ID for each lap in the
;; session SID.
;;
;; NOTE: WE DON'T RETURN LAP IDS!!!!!
(define (get-lap-mapping sid)
  (define lap-ssid-sql
    "select P.summary_id as id,
            P.start_time as timestamp
       from A_LAP P
      where P.session_id = ?")

  (for/hash (([id timestamp] (in-query (current-database) lap-ssid-sql sid)))
    (values timestamp id)))

;; Clear out the power and cycling dynamics values for all track points which
;; are above the CUTOFF power inside the dataframe DF.  This code operates
;; directly on the database, and after running it the data-frame will contain
;; outdated data.
(define (clear-power-spikes df cutoff #:database (db (current-database)))
  (define clear-power-data-sql
    "update A_TRACKPOINT
        set power = null,
            accumulated_power = null,
            left_right_balance = null,
            left_torque_effectiveness = null,
            right_torque_effectiveness = null,
            left_pedal_smoothness = null,
            right_pedal_smoothness = null,
            left_pco = null,
            right_pco = null,
            left_pp_start = null,
            left_pp_end = null,
            right_pp_start = null,
            right_pp_end = null,
            left_ppp_start = null,
            left_ppp_end = null,
            right_ppp_start = null,
            right_ppp_end = null
      where id = ?")
  (define sid (df-get-property df 'session-id))
  (define mapping (get-timestamp-mapping sid))
  (call-with-transaction
   db
   (lambda ()
     (for (([timestamp power] (in-data-frame df "timestamp" "pwr")))
       (when (and power (> power cutoff))
         (query-exec db clear-power-data-sql (hash-ref mapping timestamp))))))
  (log-event 'session-updated-data sid))

;; Store a value in the SECTION_SUMMARY table for the SSID row.  FIELD-NAME is
;; updated to VALUE.
(define (put-section-summary-value db ssid field-name value)
  (query-exec
   db
   (format "update SECTION_SUMMARY set ~a = ? where id = ?" field-name)
   value ssid))

;; Store/Update the Coggan metrics CGMETRICS for session SID.
(define (put-session-cg-metrics sid cgmetrics #:database (db (current-database)))
  (match-define (cg ftp np if tss) cgmetrics)
  (call-with-transaction
   db
   (lambda ()
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (query-exec
      db
      "update A_SESSION set intensity_factor = ?, training_stress_score = ? where id = ?"
      if tss sid)
     (put-section-summary-value db ssid "normalized_power" np))))

;; Update the section summary SSID based on the data-frame averages from START
;; to STOP.  Updates average and maximum power plus all the average cycling
;; dynamics values.
(define (put-section-summary-stats db ssid df #:start (start 0) #:stop (stop (df-row-count df)))

  (when (df-contains? df "pwr")
    (let ((stats (df-statistics df "pwr" #:start start #:stop stop)))
      (put-section-summary-value db ssid "avg_power" (statistics-mean stats))
      (put-section-summary-value db ssid "max_power" (statistics-max stats))))

  (define (put-avg series dbcol)
    (when (df-contains? df series)
      (let ((stats (df-statistics df series #:start start #:stop stop)))
        (put-section-summary-value db ssid dbcol (statistics-mean stats)))))

  (put-avg "lrbal" "left_right_balance")
  (put-avg "lteff" "avg_left_torque_effectiveness")
  (put-avg "rteff" "avg_right_torque_effectiveness")
  (put-avg "lpsmth" "avg_left_pedal_smoothness")
  (put-avg "rpsmth" "avg_right_pedal_smoothness")
  (put-avg "lpco" "avg_left_pco")
  (put-avg "rpco" "avg_right_pco")
  (put-avg "lpps" "avg_left_pp_start")
  (put-avg "lppe" "avg_left_pp_end")
  (put-avg "rpps" "avg_right_pp_start")
  (put-avg "rppe"  "avg_right_pp_end")

  (put-avg "lppps" "avg_left_ppp_start")
  (put-avg "lpppe" "avg_left_ppp_end")
  (put-avg "rppps" "avg_right_ppp_start")
  (put-avg "rpppe"  "avg_right_ppp_end"))


;; Fix power spikes: power data values above CUTOFF are cleared out and Coggan
;; metrics + averages are recalculated.
(define (do-fixups df cutoff #:database (db (current-database)) #:ftp (ftp (get-athlete-ftp)))
  (call-with-transaction
   db
   (lambda ()
     (define sid (df-get-property df 'session-id))
     (define ssid (query-value db "select summary_id from A_SESSION where id = ?" sid))
     (clear-power-spikes df cutoff #:database db)
     (define ndf (session-df db sid))      ; read it back again
     (define scgm (cg-metrics ndf #:ftp ftp))
     (put-section-summary-stats db ssid ndf)
     (put-session-cg-metrics sid scgm #:database db)
     (define lmapping (get-lap-mapping sid))
     (define laps (df-get-property df 'laps))
     (for ([start (in-vector laps)]
           [end (in-sequences (in-vector laps 1) (in-value #f))])
       (match-define (list sindex eindex)
         (if end
             (df-index-of* df "timestamp" start end)
             (list
              (df-index-of df "timestamp" start)
              (df-row-count df))))
       (define lcgm (cg-metrics ndf #:ftp ftp #:start sindex #:stop eindex))
       (define ssid (hash-ref lmapping start))
       (put-section-summary-value db ssid "normalized_power" (cg-np lcgm))
       (put-section-summary-stats db ssid ndf #:start sindex #:stop eindex)
       (query-exec db "delete from BAVG_CACHE where session_id = ?" sid)
       (query-exec db "delete from HIST_CACHE where session_id = ?" sid)
       (query-exec db "delete from SCATTER_CACHE where session_id = ?" sid)
       (update-time-in-zone-data sid db)))))

;; Determine the power which contains (1 - q) percent of the values.  I.e. if
;; q = 0.005, returns the power value where only 0.5% of the samples are
;; higher than that -- the returned value can be used as a spike cutoff point.
(define (cutoff-power df [q 0.005])
  (car (df-quantile df "pwr" q #:weight-series "timer" #:less-than >)))

;; Usage notes:
;;
;; Find a cutoff power for a session id: (cutoff-power (sid->df) 0.005)
;;
;; call do-fixups with the desired cutoff power.
