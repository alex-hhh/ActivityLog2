#lang racket/base
;; view-reports.rkt -- provide reporting on activities in the database.
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

;;; Comentary:
;;
;; Several reports are provided and they can be customized using various
;; filters.  Data can also be exported as CSV (thans to the qresults-list%
;; widget).

(require db/base
         (rename-in srfi/48 (format format-48))
         racket/class
         racket/date
         racket/gui/base
         racket/list
         racket/math
         "al-widgets.rkt"
         "database.rkt"
         "fmt-util.rkt"
         "sport-charms.rkt"
         "dbutil.rkt"
         "widgets/main.rkt"
         "utilities.rkt")

(provide view-reports%)



;;...................................................... training-volume ....

;; Report on training volume grouped by week, month and year, filtered by
;; sport and date range

;; Template for the training volume.  Needs to have filters added.  See
;; `make-tvol-query'
(define tvol-query
  "select TMP.the_period as period,
       ifnull(TMP.count, 0) as count,
       ifnull(TMP.distance, 0) as distance,
       ifnull(TMP.time, 0) as time,
       ifnull(TMP.distance / TMP.time, 0) as avg_speed,
       ifnull(round(TMP.total_hr / TMP.total_hr_time), 0) as avg_hr,
       ifnull(round(TMP.total_cadence / TMP.total_cadence_time), 0) as avg_cadence,
       ifnull(TMP.calories, 0) as calories,
       ifnull(TMP.elevation_gain, 0) as total_elevation_gain,
       ifnull(TMP.avg_elevation_gain, 0) as avg_elevation_gain,
       ifnull((TMP.total_power / TMP.total_power_time), 0) as avg_power,
       ifnull((TMP.total_np / TMP.total_np_time), 0) as avg_np,
       ifnull(TMP.avg_tss, 0) as avg_tss,
       ifnull(TMP.total_tss, 0) as total_tss
  from (select ~a as the_period,
               count(S.id) as count,
               sum(SS.total_distance) as distance,
               sum(SS.total_timer_time) as time,
               sum(ifnull(SS.total_corrected_ascent, SS.total_ascent)) as elevation_gain,
               sum(case ifnull(SS.avg_heart_rate, -1)
                   when -1 then 0
                   else SS.total_timer_time end) as total_hr_time,
               sum(SS.avg_heart_rate * SS.total_timer_time) as total_hr,
               sum(case ifnull(SS.avg_cadence, -1)
                   when -1 then 0
                   else SS.total_timer_time end) as total_cadence_time,
               sum(SS.avg_cadence * SS.total_timer_time) as total_cadence,
               sum(SS.total_calories) as calories,
               avg(ifnull(SS.total_corrected_ascent, SS.total_ascent)) as avg_elevation_gain,
               sum(SS.avg_power * SS.total_timer_time) as total_power,
               sum(case ifnull(SS.avg_power, -1)
                   when -1 then 0
                   else SS.total_timer_time end) as total_power_time,
               sum(SS.normalized_power * SS.total_timer_time) as total_np,
               sum(case ifnull(SS.normalized_power, -1)
                   when -1 then 0
                   else SS.total_timer_time end) as total_np_time,
               avg(S.training_stress_score) as avg_tss,
               sum(S.training_stress_score) as total_tss
          from A_SESSION S, SECTION_SUMMARY SS
         where S.summary_id = SS.id
           and ~a
           and ~a
           and ~a
         group by the_period) as TMP
 order by TMP.the_period desc")

;; Construct a SQL query string that groups results by PERIOD and has filters
;; for time (if HAVE-TIME-RANGE? is #t) and sport (if HAVE-SPORT?,
;; HAVE-SUB-SPORT?  are #t)
(define (make-tvol-query period have-time-range? have-sport? have-sub-sport?)

  (define group-by-month
    "date(S.start_time, 'unixepoch', 'localtime', 'start of month')")
  (define group-by-year
    "date(S.start_time, 'unixepoch', 'localtime', 'start of year')")
  (define group-by-week
    "date(S.start_time, 'unixepoch', 'localtime', '-6 days', 'weekday 1')")

  (let ((time-range (if have-time-range? "S.start_time >= ? and S.start_time < ?" "1 = 1"))
        (sport (if have-sport? "S.sport_id = ?" "1 = 1"))
        (sub-sport (if have-sub-sport? "S.sub_sport_id = ?" "1 = 1"))
        (group (cond ((eq? period 'week) group-by-week)
                     ((eq? period 'month) group-by-month)
                     ((eq? period 'year) group-by-year))))
    (format tvol-query group time-range sport sub-sport)))

;; Return training volume data rows from the database
(define (get-training-volume db group-by date-range sport sub-sport)
  (let* ((query (make-tvol-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (apply query-rows db query args)))

;; Return the SQL query for the training volume.  This is only used for
;; exporting it, as it will not run directly.
(define (get-training-volume-sql group-by date-range sport sub-sport)
  (let* ((query (make-tvol-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (format "~a~%-- Parameters ~a~%" query args)))

;; Generic training volume columns in the report.  Note that the time period
;; is added by the get-training-volume-columns (depending on the grouping) and
;; that there are specific columns for running and swimming
(define tvol-columns
  (list
   (let ((index 1))
     (qcolumn "Count"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 2))
     (qcolumn "Distance (km)"
              (lambda (row) (distance->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 3))
     (qcolumn "Time"
              (lambda (row) (duration->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 4))
     (qcolumn "Avg Speed (km/h)"
              (lambda (row) (speed->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Avg HR"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Avg Cadence"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 7))
     (qcolumn "Calories"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 8))
     (qcolumn "Total Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 9))
     (qcolumn "Avg Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 12))
     (qcolumn "Avg Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 13))
     (qcolumn "Total Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   ))

;; Specific training volume columns when the sport is running
(define tvol-columns-running
  (list
   (let ((index 1))
     (qcolumn "Count"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 2))
     (qcolumn "Distance (km)"
              (lambda (row) (distance->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 3))
     (qcolumn "Time"
              (lambda (row) (duration->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 4))
     (qcolumn "Avg Speed (min/km)"
              (lambda (row) (pace->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Avg HR"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Avg Cadence"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 7))
     (qcolumn "Calories"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 8))
     (qcolumn "Total Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 9))
     (qcolumn "Avg Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((get-stride (lambda (row)
                       (let ((distance (vector-ref row 2))
                             (time (vector-ref row 3))
                             (avg-cadence (vector-ref row 6)))
                         (if (and (> time 0) (> avg-cadence 0))
                             (/ distance (* (/ time 60.0) (* 2 avg-cadence)))
                             0)))))
     (qcolumn "Avg Stride"
              (lambda (row)
                (stride->string (get-stride row)))
              get-stride))

   (let ((index 12))
     (qcolumn "Avg Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 13))
     (qcolumn "Total Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))


   ))

;; Specific training volume columns when the sport is swimming
(define tvol-columns-cycling
  (list
   (let ((index 1))
     (qcolumn "Count"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 2))
     (qcolumn "Distance (km)"
              (lambda (row) (distance->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 3))
     (qcolumn "Time"
              (lambda (row) (duration->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 4))
     (qcolumn "Avg Speed (km/h)"
              (lambda (row) (speed->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Avg HR"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Avg Cadence"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 7))
     (qcolumn "Calories"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 8))
     (qcolumn "Total Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 9))
     (qcolumn "Avg Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 10))
     (qcolumn "Avg Power"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 11))
     (qcolumn "Avg Weighted Power"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 12))
     (qcolumn "Avg Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 13))
     (qcolumn "Total Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   ))

;; Specific training volume columns when the sport is swimming
(define tvol-columns-swimming
  (list
   (let ((index 1))
     (qcolumn "Count"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 2))
     (qcolumn "Distance (km)"
              (lambda (row) (distance->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 3))
     (qcolumn "Time"
              (lambda (row) (duration->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 4))
     (qcolumn "Avg Speed (min/100m)"
              (lambda (row) (swim-pace->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Avg HR"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Avg Cadence"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 7))
     (qcolumn "Calories"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 12))
     (qcolumn "Avg Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   (let ((index 13))
     (qcolumn "Total Effort"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))

   ))


(define tvol-colums-by-sport
  `((1 . ,tvol-columns-running)
    (2 . ,tvol-columns-cycling)
    (5 . ,tvol-columns-swimming)))

;; Return the training volume columns to use depending on SPORT and
;; TIME-PERIOD
(define (get-training-volume-columns sport time-period)

  (define (fmt-week s) s)

  (define (fmt-month s)

    (define month (vector "XXX" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a ~a" (vector-ref month m) y))
          #f)))

  (define (fmt-year s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a" y))
          #f)))

  ;; Convert a date received from SQLite (e.g 2014-12-01) to a unix timestamp
  (define (str->date s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (begin
            (let ((year (string->number (list-ref m 1)))
                  (month (string->number (list-ref m 2)))
                  (day (string->number (list-ref m 3))))
              (date->seconds (date 0 0 0 day month year 0 0 #f 0))
              ))
          #f)))

  (let ((time-period-column
         (let ((index 0))
           (qcolumn "Time Period"
                    (lambda (row)
                      (let ((date-string (vector-ref row index)))
                        (cond ((eq? time-period 'month)
                               (fmt-month date-string))
                              ((eq? time-period 'year)
                               (fmt-year date-string))
                              (#t
                               (fmt-week date-string)))))
                    (lambda (row) (str->date (vector-ref row index)))))))
    (cons time-period-column
          (cond ((assoc sport tvol-colums-by-sport) => cdr)
                (#t tvol-columns)))))


;............................................ triathlon training volume ....

;; Report on the amount of swim, bike, run and strength traning activities per
;; week, month or year.

(define tri-tvol-query
  "select ~a as period,
         total(T.run_count) as run_count,
         total(T.run_distance) as run_distance,
         total(T.run_time) as run_time,
         total(T.bike_count) as bike_count,
         total(T.bike_distance) as bike_distance,
         total(T.bike_time) as bike_time,
         total(T.swim_count) as swim_count,
         total(T.swim_distance) as swim_distance,
         total(T.swim_time) as swim_time,
         total(T.strength_count) as strength_count,
         total(T.strength_distance) as strength_distance,
         total(T.strength_time) as strength_time,
         total(T.sport_count) as total_count,
         total(T.sport_distance) as total_distance,
         total(T.sport_time) as total_time,
         total(T.run_effort) as run_effort,
         total(T.bike_effort) as bike_effort,
         total(T.swim_effort) as swim_effort,
         total(T.strength_effort) as strength_effort,
         total(T.effort) as total_effort
    from V_TRIATHLON_SESSIONS T
   where ~a and ~a and ~a
   group by period
   order by period desc")

(define (make-tri-tvol-query period have-time-range? have-sport? have-sub-sport?)

  (define group-by-month
    "date(T.start_time, 'unixepoch', 'localtime', 'start of month')")
  (define group-by-year
    "date(T.start_time, 'unixepoch', 'localtime', 'start of year')")
  (define group-by-week
    "date(T.start_time, 'unixepoch', 'localtime', '-6 days', 'weekday 1')")

  (let ((time-range (if have-time-range? "T.start_time >= ? and T.start_time < ?" "1 = 1"))
        (sport (if have-sport? "T.sport_id = ?" "1 = 1"))
        (sub-sport (if have-sub-sport? "T.sub_sport_id = ?" "1 = 1"))
        (group (cond ((eq? period 'week) group-by-week)
                     ((eq? period 'month) group-by-month)
                     ((eq? period 'year) group-by-year))))
    (format tri-tvol-query group time-range sport sub-sport)))

(define (get-tri-training-volume db group-by date-range sport sub-sport)
  (let* ((query (make-tri-tvol-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (apply query-rows db query args)))

(define (get-tri-training-volume-sql group-by date-range sport sub-sport)
  (let* ((query (make-tri-tvol-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (format "~a~%-- Parameters: ~a~%" query args)))

;; Triatholn training volume columns in the report.  Note that the time period
;; is added by the get-tri-training-volume-columns (depending on the grouping)
(define tri-tvol-columns
  (list
   (let ((index 1))
     (qcolumn "Run Count"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (number->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 2))
     (qcolumn "Run Distance (km)"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (distance->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 3))
     (qcolumn "Run Time"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (duration->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((extract-pct (lambda (row)
                        (let ((total-time (vector-ref row 15))
                              (run-time (vector-ref row 3)))
                          (if (> total-time 0) (/ run-time total-time) 0)))))
     (qcolumn "Run Pct"
              (lambda (row)
                (let ((v (/ (round (* (extract-pct row) 10000)) 100)))
                  (if (> v 0) (format-48 "~2,2F %" v) "")))
              extract-pct))
   (let ((index 16))
     (qcolumn "Run Effort"
              (lambda (row)
                (let ((v (exact-round (vector-ref row index))))
                  (if (> v 0) (format "~a" v) "")))
              (lambda (row) (vector-ref row index))))

   (let ((index 4))
     (qcolumn "Bike Count"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (number->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Bike Distance (km)"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (distance->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Bike Time"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (duration->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((extract-pct (lambda (row)
                        (let ((total-time (vector-ref row 15))
                              (run-time (vector-ref row 6)))
                          (if (> total-time 0) (/ run-time total-time) 0)))))
     (qcolumn "Bike Pct"
              (lambda (row)
                (let ((v (/ (round (* (extract-pct row) 10000)) 100)))
                  (if (> v 0) (format-48 "~2,2F %" v) "")))
              extract-pct))
   (let ((index 17))
     (qcolumn "Bike Effort"
              (lambda (row)
                (let ((v (exact-round (vector-ref row index))))
                  (if (> v 0) (format "~a" v) "")))
              (lambda (row) (vector-ref row index))))

   (let ((index 7))
     (qcolumn "Swim Count"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (number->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 8))
     (qcolumn "Swim Distance (km)"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (distance->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 9))
     (qcolumn "Swim Time"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (duration->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((extract-pct (lambda (row)
                        (let ((total-time (vector-ref row 15))
                              (run-time (vector-ref row 9)))
                          (if (> total-time 0) (/ run-time total-time) 0)))))
     (qcolumn "Swim Pct"
              (lambda (row)
                (let ((v (/ (round (* (extract-pct row) 10000)) 100)))
                  (if (> v 0) (format-48 "~2,2F %" v) "")))
              extract-pct))
   (let ((index 18))
     (qcolumn "Swim Effort"
              (lambda (row)
                (let ((v (exact-round (vector-ref row index))))
                  (if (> v 0) (format "~a" v) "")))
              (lambda (row) (vector-ref row index))))

   (let ((index 10))
     (qcolumn "Strength Count"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (number->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 12))
     (qcolumn "Strength Time"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (duration->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((extract-pct (lambda (row)
                        (let ((total-time (vector-ref row 15))
                              (run-time (vector-ref row 12)))
                          (if (> total-time 0) (/ run-time total-time) 0)))))
     (qcolumn "Strength Pct"
              (lambda (row)
                (let ((v (/ (round (* (extract-pct row) 10000)) 100)))
                  (if (> v 0) (format-48 "~2,2F %" v) "")))
              extract-pct))
   (let ((index 19))
     (qcolumn "Strength Effort"
              (lambda (row)
                (let ((v (exact-round (vector-ref row index))))
                  (if (> v 0) (format "~a" v) "")))
              (lambda (row) (vector-ref row index))))

   (let ((index 13))
     (qcolumn "Total Count"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (number->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 14))
     (qcolumn "Total Distance (km)"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (distance->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 15))
     (qcolumn "Total Time"
              (lambda (row)
                (let ((v (vector-ref row index)))
                  (if (> v 0) (duration->string v) "")))
              (lambda (row) (vector-ref row index))))
   (let ((index 20))
     (qcolumn "Total Effort"
              (lambda (row)
                (let ((v (exact-round (vector-ref row index))))
                  (if (> v 0) (format "~a" v) "")))
              (lambda (row) (vector-ref row index))))
   ))


(define (get-tri-training-volume-columns time-period)

  (define (fmt-week s) s)

  (define (fmt-month s)

    (define month (vector "XXX" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a ~a" (vector-ref month m) y))
          #f)))

  (define (fmt-year s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a" y))
          #f)))

  ;; Convert a date received from SQLite (e.g 2014-12-01) to a unix timestamp
  (define (str->date s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (begin
            (let ((year (string->number (list-ref m 1)))
                  (month (string->number (list-ref m 2)))
                  (day (string->number (list-ref m 3))))
              (date->seconds (date 0 0 0 day month year 0 0 #f 0))
              ))
          #f)))

  (let ((time-period-column
         (let ((index 0))
           (qcolumn "Time Period"
                    (lambda (row)
                      (let ((date-string (vector-ref row index)))
                        (cond ((eq? time-period 'month)
                               (fmt-month date-string))
                              ((eq? time-period 'year)
                               (fmt-year date-string))
                              (#t
                               (fmt-week date-string)))))
                    (lambda (row) (str->date (vector-ref row index)))))))
    (cons time-period-column tri-tvol-columns)))





;................................................ activity distribution ....

;; Report on activity distribution: how much time, distance and how many
;; activities were done for each sport (filtered by sport and time).  There
;; are two reports: one groups sports (e.g. Swimming contains both Lap and
;; Open Water) and one that has each sport individually.

;; NOTE: the activtiy_type and sub_sport_id fields are incorrect if this query
;; is grouped only by sport.  We have code to specially ignore them when
;; displaying the results of this query for the "Activity Distribution" report
;; and only consider them in the "Activity Distribution (detailed)" report.

(define adist-query
  "select ifnull(TMP.activity_sub_type, TMP.activity_type) as activity_type,
               TMP.sport_id as sport_id,
               TMP.sub_sport_id as sub_sport_id,
               ifnull(TMP.count, 0) as count,
               ifnull(TMP.distance, 0) as distance,
               ifnull(TMP.time, 0) as time,
               ifnull(TMP.distance / TMP.time, 0) as avg_speed,
               ifnull(round(TMP.total_hr / TMP.total_hr_time), 0) as avg_hr,
               ifnull(round(TMP.total_cadence / TMP.total_cadence_time), 0) as avg_cadence,
               ifnull(TMP.calories, 0) as calories,
               ifnull(TMP.elevation_gain, 0) as total_elevation_gain,
               ifnull(TMP.avg_elevation_gain, 0) as avg_elevation_gain
        from (select ES.name as activity_type,
                     (select name from E_SUB_SPORT where id = S.sub_sport_id) as activity_sub_type,
                     S.sport_id,
                     S.sub_sport_id,
                     count(S.id) as count,
                     sum(SS.total_distance) as distance,
                     sum(SS.total_timer_time) as time,
                     sum(ifnull(SS.total_corrected_ascent, SS.total_ascent)) as elevation_gain,
                     sum(case ifnull(SS.avg_heart_rate, -1)
                         when -1 then 0
                         else SS.total_timer_time end) as total_hr_time,
                     sum(SS.avg_heart_rate * SS.total_timer_time) as total_hr,
                     sum(case ifnull(SS.avg_cadence, -1)
                         when -1 then 0
                         else SS.total_timer_time end) as total_cadence_time,
                     sum(SS.avg_cadence * SS.total_timer_time) as total_cadence,
                     sum(SS.total_calories) as calories,
                     avg(ifnull(SS.total_corrected_ascent, SS.total_ascent)) as avg_elevation_gain
                from A_SESSION S, SECTION_SUMMARY SS, E_SPORT ES
               where S.sport_id = ES.id
                 and S.summary_id = SS.id
                 and ~a
                 and ~a
                 and ~a
               group by ~a) as TMP")

(define (make-adist-query group have-time-range? have-sport? have-sub-sport?)
  (let ((time-range (if have-time-range? "S.start_time >= ? and S.start_time < ?" "1 = 1"))
        (sport (if have-sport? "S.sport_id = ?" "1 = 1"))
        (sub-sport (if have-sub-sport? "S.sub_sport_id = ?" "1 = 1"))
        (group (cond ((eq? group 'normal) "S.sport_id")
                     ((eq? group 'detailed) "S.sport_id, S.sub_sport_id"))))
    (format adist-query time-range sport sub-sport group)))

(define (get-activity-distribution db group-by date-range sport sub-sport)
  (let* ((query (make-adist-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (apply query-rows db query args)))

(define (get-activity-distribution-sql group-by date-range sport sub-sport)
  (let* ((query (make-adist-query group-by date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (format "~a~%-- Parameters: ~a~%" query args)))

(define adist-columns
  (list
   (let ((index 3))
     (qcolumn "Count"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 4))
     (qcolumn "Distance (km)"
              (lambda (row) (distance->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 5))
     (qcolumn "Time"
              (lambda (row) (duration->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 6))
     (qcolumn "Avg Speed (km/h)"
              (lambda (row) (speed->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 7))
     (qcolumn "Avg HR"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 8))
     (qcolumn "Avg Cadence"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 9))
     (qcolumn "Calories"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 10))
     (qcolumn "Total Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((index 11))
     (qcolumn "Avg Elevation Gain"
              (lambda (row) (n->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))))

(define (get-activity-distribution-columns sport type)
  (cons
   (let ((fn (lambda (row)
               (let ((sport (vector-ref row 1))
                     (sub-sport (vector-ref row 2)))
                 (when (sql-null? sub-sport)
                   (set! sub-sport #f))
                 (get-sport-name sport (if (eq? type 'detailed) sub-sport #f))))))
     (qcolumn "Sport" fn fn))
   adist-columns))


;;........................................................ equipment use ....

;; Report on equipment usage, filtered by sport and date range

;; NOTE: we use the SQLite specific total() aggregate function, which is
;; equivalent to ifnull(sum(...), 0), but shorter.

(define equse-query
  "select EQ.id,
       ifnull(EQ.name, ''),
       ifnull(EQ.device_name, '') as devname,
       ifnull(EQ.serial_number, '') as serial,
       count(EU.session_id) as use_count,
       ifnull(sum(SS.total_timer_time), 0) as hours_used,
       ifnull(sum(SS.total_distance), 0) as kms_used
  from EQUIPMENT EQ left join EQUIPMENT_USE EU on EQ.id = EU.equipment_id,
       A_SESSION S, SECTION_SUMMARY SS
 where EU.session_id = S.id
   and S.summary_id = SS.id
   and ~a
   and ~a
   and ~a
 group by EQ.id")

;; This query also selects equipment which has no associated sessions.  It is
;; used by `make-equse-query' when no filtering is requested.
(define equse-query-1
  "select EQ.id,
       ifnull(EQ.name, ''),
       ifnull(EQ.device_name, '') as devname,
       ifnull(EQ.serial_number, '') as serial,
       (select count(EU.session_id)
          from EQUIPMENT_USE EU
         where EU.equipment_id = EQ.id) as use_count,
       (select total(SS.total_timer_time)
          from A_SESSION S, SECTION_SUMMARY SS, EQUIPMENT_USE EU
         where EU.equipment_id = EQ.id
           and EU.session_id = S.id
           and S.summary_id = SS.id) as hours_used,
       (select total(SS.total_distance)
          from A_SESSION S, SECTION_SUMMARY SS, EQUIPMENT_USE EU
         where EU.equipment_id = EQ.id
           and EU.session_id = S.id
           and S.summary_id = SS.id) as km_used
  from EQUIPMENT EQ")

(define (make-equse-query have-time-range? have-sport? have-sub-sport?)
  (if (or have-time-range? have-sport? have-sub-sport?)
      (let ((time-range (if have-time-range? "S.start_time >= ? and S.start_time < ?" "1 = 1"))
            (sport (if have-sport? "S.sport_id = ?" "1 = 1"))
            (sub-sport (if have-sub-sport? "S.sub_sport_id = ?" "1 = 1")))
        (format equse-query time-range sport sub-sport))
      equse-query-1))

(define (get-equipment-use db date-range sport sub-sport)
  (let* ((query (make-equse-query date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (apply query-rows db query args)))

(define (get-equipment-use-sql date-range sport sub-sport)
  (let* ((query (make-equse-query date-range sport sub-sport))
         (args (append
                (if date-range (list (car date-range) (cdr date-range)) '())
                (if sport (list sport) '())
                (if sub-sport (list sub-sport) '()))))
    (format "~a~%-- Parameters: ~a~%" query args)))

(define equse-columns
  (list
   (let ((fn (lambda (row) (vector-ref row 1))))
     (qcolumn "Name" fn fn))
   (let ((fn (lambda (row) (vector-ref row 2))))
     (qcolumn "Device Name" fn fn))
   (let ((fn (lambda (row)
               (let ((v (vector-ref row 3)))
                 (if (sql-null? v) 0 v)))))
     (qcolumn "Serial Number"
              (lambda (row)
                ;; TODO: not sure why v is sometimes an empty string.  It
                ;; should be a number.
                (let ((v (fn row)))
                  (if (string? v)
                      v
                      (number->string v))))
              fn))
   (qcolumn "Use Count"
            (lambda (row) (number->string (vector-ref row 4)))
            (lambda (row) (vector-ref row 4)))
   (qcolumn "Hours Used"
            (lambda (row) (duration->string (vector-ref row 5)))
            (lambda (row) (vector-ref row 5)))
   (qcolumn "Total Distance"
            (lambda (row) (distance->string (vector-ref row 6)))
            (lambda (row) (vector-ref row 6)))))

(define (get-equipment-use-columns)
  equse-columns)


;;...................................................... athlete metrics ....

(define am-query-template
  "select ~a as period,
       count(AM.id) as nsamples,
       avg(AM.body_weight) as body_weight,
       avg(AM.sleep_time) as sleep_time,
       round(avg(AM.sleep_quality)) as sleep_quality,
       round(avg(AM.overall_feeling)) as overall_feeling
  from ATHLETE_METRICS AM
 where ~a
 group by period
 order by period desc")

;; Construct a SQL query string that groups results by PERIOD and has filters
;; for time (if HAVE-TIME-RANGE? is #t)
(define (make-am-query period have-time-range?)

  (define group-by-month
    "date(AM.timestamp, 'unixepoch', 'localtime', 'start of month')")
  (define group-by-year
    "date(AM.timestamp, 'unixepoch', 'localtime', 'start of year')")
  (define group-by-week
    "date(AM.timestamp, 'unixepoch', 'localtime', '-6 days', 'weekday 1')")

  (let ((time-range (if have-time-range? "AM.timestamp >= ? and AM.timestamp < ?" "1 = 1"))
        (group (cond ((eq? period 'week) group-by-week)
                     ((eq? period 'month) group-by-month)
                     ((eq? period 'year) group-by-year))))
    (format am-query-template group time-range)))

;; Return athlete metrics data rows from the database
(define (get-athlete-metrics db group-by date-range)
  (let* ((query (make-am-query group-by date-range))
         (args (if date-range (list (car date-range) (cdr date-range)) '())))
    (apply query-rows db query args)))

;; Return the SQL query for the athlete metrics.  This is only used for
;; exporting it, as it will not run directly.
(define (get-athlete-metrics-sql group-by date-range)
  (let* ((query (make-am-query group-by date-range))
         (args (if date-range (list (car date-range) (cdr date-range)) '())))
    (format "~a~%-- Parameters ~a~%" query args)))

(define am-columns
  (list
   (let ((index 1))
     (qcolumn "Samples"
              (lambda (row) (number->string (vector-ref row index)))
              (lambda (row) (vector-ref row index))))
   (let ((fn (lambda (row) (sql-column-ref row 2 #f))))
     (qcolumn "Body weight (avg)"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (weight->string v) "")))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 3 #f))))
     (qcolumn "Sleep Time (avg)"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (duration->string v) "")))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 4 #f))))
     (qcolumn "Sleep Quality (avg)"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (number->string v) "")))
              fn))
   (let ((fn (lambda (row) (sql-column-ref row 5 #f))))
     (qcolumn "Overall Feeling (avg)"
              (lambda (row)
                (let ((v (fn row)))
                  (if v (number->string v) "")))
              fn))
   ))

(define (get-am-columns time-period)

  (define (fmt-week s) s)

  (define (fmt-month s)

    (define month (vector "XXX" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a ~a" (vector-ref month m) y))
          #f)))

  (define (fmt-year s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (let ((y (string->number (list-ref m 1)))
                (m (string->number (list-ref m 2)))
                (d (string->number (list-ref m 3))))
            (format "~a" y))
          #f)))

  ;; Convert a date received from SQLite (e.g 2014-12-01) to a unix timestamp
  (define (str->date s)
    (let ((m (regexp-match "^([0-9]+)-([0-9]+)-([0-9]+)$" s)))
      (if m
          (begin
            (let ((year (string->number (list-ref m 1)))
                  (month (string->number (list-ref m 2)))
                  (day (string->number (list-ref m 3))))
              (date->seconds (date 0 0 0 day month year 0 0 #f 0))
              ))
          #f)))

  (let ((time-period-column
         (let ((index 0))
           (qcolumn "Time Period"
                    (lambda (row)
                      (let ((date-string (vector-ref row index)))
                        (cond ((eq? time-period 'month)
                               (fmt-month date-string))
                              ((eq? time-period 'year)
                               (fmt-year date-string))
                              (#t
                               (fmt-week date-string)))))
                    (lambda (row) (str->date (vector-ref row index)))))))
    (cons time-period-column am-columns)))


;;........................................................ view-reports% ....

(define view-reports%
  (class object%
    (init parent database)
    (super-new)

    (define the-database database)

    (define sport-filter (cons #f #f))
    (define date-range-filter #f)
    (define selected-report 0)

    (define date-range-field #f)

    (define change-notification-source (make-log-event-source))

    (define the-reports
      (list
       (list "Volume by Week"
             (lambda ()
               (get-training-volume
                the-database 'week
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-training-volume-columns sport 'week))
             (lambda ()
               (get-training-volume-sql
                'week
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Volume by Month"
             (lambda ()
               (get-training-volume
                the-database 'month
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-training-volume-columns sport 'month))
             (lambda ()
               (get-training-volume-sql
                'month
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Volume by Year"
             (lambda ()
               (get-training-volume
                the-database 'year
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-training-volume-columns sport 'year))
             (lambda ()
               (get-training-volume-sql
                'year
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Activity Distribution"
             (lambda ()
               (get-activity-distribution
                the-database 'normal
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-activity-distribution-columns sport 'normal))
             (lambda ()
               (get-activity-distribution-sql
                'normal
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Activity Distribution (detailed)"
             (lambda ()
               (get-activity-distribution
                the-database 'detailed
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-activity-distribution-columns sport 'detailed))
             (lambda ()
               (get-activity-distribution-sql
                'detailed
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Equipment Use"
             (lambda ()
               (get-equipment-use
                the-database
                date-range-filter
                (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-equipment-use-columns))
             (lambda ()
               (get-equipment-use-sql
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Triathlon Volume by Week"
             (lambda ()
               (get-tri-training-volume
                the-database 'week
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-tri-training-volume-columns 'week))
             (lambda ()
               (get-tri-training-volume-sql
                'week
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Triathlon Volume by Month"
             (lambda ()
               (get-tri-training-volume
                the-database 'month
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-tri-training-volume-columns 'month))
             (lambda ()
               (get-tri-training-volume-sql
                'month
                date-range-filter (car sport-filter) (cdr sport-filter))))
       (list "Triathlon Volume by Year"
             (lambda ()
               (get-tri-training-volume
                the-database 'year
                date-range-filter (car sport-filter) (cdr sport-filter)))
             (lambda (sport)
               (get-tri-training-volume-columns 'year))
             (lambda ()
               (get-tri-training-volume-sql
                'year
                date-range-filter (car sport-filter) (cdr sport-filter))))

       (list "Athlete Metrics by Week"
             (lambda ()
               (get-athlete-metrics
                the-database 'week date-range-filter))
             (lambda (sport)
               (get-am-columns 'week))
             (lambda ()
               (get-athlete-metrics-sql 'week date-range-filter)))

       (list "Athlete Metrics by Month"
             (lambda ()
               (get-athlete-metrics
                the-database 'month date-range-filter))
             (lambda (sport)
               (get-am-columns 'month))
             (lambda ()
               (get-athlete-metrics-sql 'month date-range-filter)))

       (list "Athlete Metrics by Year"
             (lambda ()
               (get-athlete-metrics
                the-database 'year date-range-filter))
             (lambda (sport)
               (get-am-columns 'year))
             (lambda ()
               (get-athlete-metrics-sql 'year date-range-filter)))

       ))

    (define the-pane (new (class vertical-pane%
                            (init)(super-new)
                            (define/public (interactive-export-sql-query)
                              (on-interactive-export-sql-query)))
                          [parent parent]
                          [alignment '(left center)]))

    (let ((sel-pane (new horizontal-pane% [parent the-pane]
                         [spacing 20]
                         [border 0]
                         [stretchable-height #f]
                         [stretchable-width #t]
                         [alignment '(left center)])))
      (make-spacer sel-pane)
      (new message%
           [parent sel-pane]
           [label (reports-icon)])

      (let ((filter-pane (new horizontal-pane% [parent sel-pane]
                              [spacing 20]
                              [alignment '(left top)])))

        (new choice% [parent filter-pane]
             [label "Report "]
             [choices (map first the-reports)]
             [callback (lambda (c event)
                         (set! selected-report (send c get-selection))
                         (on-filter-changed))])

        (new sport-selector% [parent filter-pane]
             [callback (lambda (s)
                         (set! sport-filter s)
                         (on-filter-changed))])

        (let ((drs (new date-range-selector% [parent filter-pane]
                        [initial-selection 'last-30-days]
                        [callback (lambda (s)
                                    (set! date-range-filter s)
                                    (on-filter-changed))])))
          (send drs set-seasons (db-get-seasons the-database))
          ;; Setup the date-range-filter to the selector's initial value
          (set! date-range-filter (send drs get-selection))
          (set! date-range-field drs)))

      (make-spacer sel-pane))
    

    (define the-list-box (new qresults-list% [parent the-pane]
                              [pref-tag 'activity-log:reports-view]))

    (define (on-filter-changed)
      (when selected-report
        (let ((report-info (list-ref the-reports selected-report)))
          (let ((report-fn (second report-info))
                (columns ((third report-info) (car sport-filter))))
            (send the-list-box set-default-export-file-name
                  (format "~a.csv" (first report-info)))
            (send the-list-box setup-column-defs columns)
            (send the-list-box set-data (report-fn))))))

    (define/public (on-interactive-export-sql-query)
      (when selected-report
        (let ((report-info (list-ref the-reports selected-report)))
          (let ((query ((fourth report-info))))
            (send (get-sql-export-dialog)
                  show-dialog (send the-pane get-top-level-window) query)))))

    (define first-time? #t)

    (define/public (activated)
      ;; Get the full list of events, but we will discard them if the view is
      ;; activated the first time and has to do a full refresh anyway
      (define events (collect-events change-notification-source))
      (when (or first-time?
                ;; Check if anything changed that potentially affects the
                ;; reports.  Currently sessions and athlete-metrics changes
                ;; are in this list.  We don't check if a session actually
                ;; affects a report, so we might refresh more than necessary.
                (hash-ref events 'session-updated #f)
                (hash-ref events 'session-created #f)
                (hash-ref events 'session-deleted #f)
                (hash-ref events 'athlete-metrics-updated #f)
                (hash-ref events 'athlete-metrics-created #f)
                (hash-ref events 'athlete-metrics-deleted #f))
        (refresh)
        (set! first-time? #f)))

    (define/public (refresh)
      (send date-range-field set-seasons (db-get-seasons the-database))
      ;; Refresh the date range (in case the selection is something like "last
      ;; 30 days" and the date has changed.
      (set! date-range-filter (send date-range-field get-selection))
      (on-filter-changed))

    (define/public (save-visual-layout)
      (send the-list-box save-visual-layout))

    ))
