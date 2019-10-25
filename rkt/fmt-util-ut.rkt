#lang racket/base
;; fmt-util-ut.rkt -- format utilities plain Racket code
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/match
         racket/format
         racket/string
         racket/contract
         tzinfo)

(provide/contract
 (calendar-date->string (->* ((or/c date? integer?))
                             (#:time-zone (or/c string? #f))
                             string?))
 (time-of-day->string (->* ((or/c date? integer?))
                           (#:include-seconds? boolean? #:time-zone (or/c string? #f))
                           string?))
 (date-time->string (->* ((or/c date? integer?))
                         (#:include-seconds? boolean? #:time-zone (or/c string? #f) #:include-time-zone? boolean?)
                         string?))
 (format-date (-> integer? (or/c string? #f) string?))
 (->date (->* ((or/c date? integer?)) ((or/c string? #f))
              (values date? (or/c string? #f)))))

(define months #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define months-long #("January" "February" "March" "April"
                                "May" "June" "July" "August"
                                "September" "October" "November" "December"))
(define weekdays #("Sunday" "Monday" "Tuesday" "Wednesday"
                            "Thursday" "Friday" "Saturday"))

;; Convert the value D into a date struct and return it together with a
;; possible abbreviated time zone name (two values are returned).  This is a
;; "do what I mean" function, but the usual case is for D to be a unix time
;; stamp and TIME-ZONE a time zone name, the resulting date struct contains
;; the date and time at the specified time zone.
(define (->date d [time-zone #f])
  (cond ((date? d) (values d #f))       ; ignore time zone
        (time-zone                      ; we have a time zone
         (match-define (tzoffset delta dst? tzabbrev)
           (utc-seconds->tzoffset time-zone d))
         (values (seconds->date (+ d delta) #f) tzabbrev))
        (#t
         (values (seconds->date d #t) #f))))

;; Convert the value D (a unix time stamp) into a string representing the date
;; only.  The date is calculated for the time zone TZ, or for the current
;; local time if TZ is #f.
(define (calendar-date->string d #:time-zone [tz #f])
  (define-values (d1 tzabbrev) (->date d tz))
  (string-append (~a (date-day d1))
                 " "
                 (vector-ref months (sub1 (date-month d1)))
                 " "
                 (~a (date-year d1))))

;; Convert the value D (a unix time stamp) into a string representing the time
;; of day for the time zone TZ, or for the local time if TZ is #f.  If
;; INCLUDE-SECONDS? is #t the string will include seconds as well, otherwise
;; only hour and minute will be shown.
(define (time-of-day->string d
                             #:include-seconds? [include-seconds? #t]
                             #:time-zone [tz #f])
  (define-values (d1 tzabbrev) (->date d tz))
  (if include-seconds?
      (string-append
       (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-hour d1))
       ":"
       (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-minute d1))
       ":"
       (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-second d1)))
      (string-append
       (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-hour d1))
       ":"
       (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-minute d1)))))

;; Convert the value D (a unix time stamp) into a string representing the date
;; and time for the time zone TZ (or local time if the time zone is #f).  If
;; INCLUDE-SECONDS? is #t, the time will also include seconds, otherwise only
;; hour and minutes are shown.  If INCLUDE-TIMEZONE? is #t, the time zone
;; abbreviation is also added to the string.
(define (date-time->string d
                           #:include-seconds? [include-seconds? #f]
                           #:time-zone [tz #f]
                           #:include-time-zone? [include-timezone? #f])
  (define-values (d1 tzabbrev) (->date d tz))
  (string-append (calendar-date->string d1)
                 " "
                 (time-of-day->string d1 #:include-seconds? include-seconds?)
                 (if (and tzabbrev include-timezone?) (format " (~a)" tzabbrev) "")))

;; A "do what I mean" date formatting function, takes a UNIX time in UTC and
;; an optional time zone, and produces a string representing the date, time
;; and time zone for it, in local time at that time zone.
(define (format-date utc time-zone)
  (define-values (d1 tzabbrev) (->date utc time-zone))
  (string-append
   (vector-ref weekdays (date-week-day d1))
   ", "
   (vector-ref months-long (sub1 (date-month d1)))
   " "
   (format "~a~a" (date-day d1) (case (date-day d1) ((1) "st") ((2) "nd") ((3) "rd") (else "th")))
   " "
   (~a (date-year d1))
   ", "
   (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-hour d1))
   ":"
   (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-minute d1))
   ":"
   (~a #:min-width 2 #:align 'right #:left-pad-string "0" (date-second d1))
   (if tzabbrev (format " ~a" tzabbrev) "")))
