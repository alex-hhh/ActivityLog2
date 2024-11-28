#lang racket/base

;; shifting.rkt -- handle shifting (gear change) information in cycling
;; sessions
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame
         racket/contract
         racket/match
         racket/class
         db/base
         "series-metadata.rkt")

(provide/contract
 (maybe-add-gear-change-series! (-> data-frame? connection? any/c))
 (update-gear-rations-for-session! (-> connection? exact-integer?
                                       (vectorof exact-integer?)
                                       (vectorof exact-integer?)
                                       any/c)))
(provide
 axis-gear-ratio
 axis-rear-gear
 axis-front-gear
 axis-rear-gear-index
 axis-front-gear-index)

;; Update in the database DB the gear teeth in the GEAR_CHANGE table for a
;; specified session, SID. FRONT-GEARS is a vector of front gear teeth counts
;; (innermost gear first), REAR-GEARS is a vector of rear gear teeth counts,
;; innermost first.
(define (update-gear-rations-for-session! db sid front-gears rear-gears)
  (define gcdf (df-read/sql db "
select id, front_gear_index, rear_gear_index
 from GEAR_CHANGE
where session_id = ?" sid))

  (define max-front-gears (vector-length front-gears))
  (define max-rear-gears (vector-length rear-gears))

  (when (> (df-row-count gcdf) 0)       ; only if we have any gear change data
    (call-with-transaction
     db
     (lambda ()
       (for ([(id fgi rgi)
              (in-data-frame gcdf "id" "front_gear_index" "rear_gear_index")]
             #:when (exact-integer? id))
         (define front-teeth
           (and (exact-integer? fgi)
                (>= fgi 1)
                (<= fgi max-front-gears)
                (vector-ref front-gears (sub1 fgi))))
         (define rear-teeth
           (and (exact-integer? rgi)
                (>= rgi 1)
                (<= rgi max-rear-gears)
                (vector-ref rear-gears (sub1 rgi))))
         (query-exec
          db
          "update GEAR_CHANGE set front_gear_teeth = ?, rear_gear_teeth = ?
where id = ?"
          (or front-teeth sql-null)
          (or rear-teeth sql-null)
          id))))))

;; Read gear change data from the database and add the respective series to
;; the data frame DF.
(define (maybe-add-gear-change-series! df db)
  (define sid (df-get-property df 'session-id))
  (when (exact-integer? sid)
    (define gcdf (df-read/sql db "
select timestamp, front_gear_index, front_gear_teeth, rear_gear_index, rear_gear_teeth
 from GEAR_CHANGE
where session_id = ?
order by timestamp" sid))

    (when (> (df-row-count gcdf) 0)       ; only if we have any gear change data
      (df-set-sorted! gcdf "timestamp" <)

      (df-add-lazy!
       df
       "fgi"                                ; front-gear-index
       '("timestamp")
       (lambda (v)
         (define timestamp (list-ref v 0))
         (df-lookup gcdf "timestamp" "front_gear_index" timestamp)))

      (df-add-lazy!
       df
       "rgi"                                ; rear-gear-index
       '("timestamp")
       (lambda (v)
         (define timestamp (list-ref v 0))
         (df-lookup gcdf "timestamp" "rear_gear_index" timestamp)))

      (df-add-lazy!
       df
       "fg"                                ; front-gear-index
       '("timestamp")
       (lambda (v)
         (define timestamp (list-ref v 0))
         (df-lookup gcdf "timestamp" "front_gear_teeth" timestamp)))

      (df-add-lazy!
       df
       "rg"                                ; rear-gear-index
       '("timestamp")
       (lambda (v)
         (define timestamp (list-ref v 0))
         (df-lookup gcdf "timestamp" "rear_gear_teeth" timestamp)))

      (df-add-lazy!
       df
       "gr"
       '("fg" "rg")
       (lambda (v)
         (match-define (list front rear) v)
         (and (rational? front)
              (rational? rear)
              (> rear 0)
              (/ front rear)))))))


;;................................................... gear change series ....

(define axis-front-gear-index
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Front Gear Index")
         (define/override (axis-label) "Front Gear Index")
         (define/override (should-filter?) #f)
         (define/override (plot-label) "Front Gear Index")
         (define/override (name) "Front Gear Index")
         (define/override (series-name) "fgi"))))
(register-series-metadata axis-front-gear-index)


(define axis-rear-gear-index
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Rear Gear Index")
         (define/override (axis-label) "Rear Gear Index")
         (define/override (should-filter?) #f)
         (define/override (plot-label) "Rear Gear Index")
         (define/override (name) "Rear Gear Index")
         (define/override (series-name) "rgi"))))
(register-series-metadata axis-rear-gear-index)

(define axis-front-gear
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Front Gear")
         (define/override (axis-label) "Front Gear")
         (define/override (should-filter?) #f)
         (define/override (plot-label) "Front Gear")
         (define/override (name) "Front Gear")
         (define/override (series-name) "fg"))))
(register-series-metadata axis-front-gear)

(define axis-rear-gear
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Rear Gear")
         (define/override (axis-label) "Rear Gear")
         (define/override (should-filter?) #f)
         (define/override (plot-label) "Rear Gear")
         (define/override (name) "Rear Gear")
         (define/override (series-name) "rg"))))
(register-series-metadata axis-rear-gear)

(define axis-gear-ratio
  (new (class series-metadata% (init) (super-new)
         (define/override (headline) "Gear Ratio")
         (define/override (axis-label) "Gear Ratio")
         (define/override (should-filter?) #f)
         (define/override (fractional-digits) 1)
         (define/override (histogram-bucket-slot) 0.1)
         (define/override (plot-label) "Gear Ratio")
         (define/override (name) "Gear Ratio")
         (define/override (series-name) "gr"))))
(register-series-metadata axis-gear-ratio)
