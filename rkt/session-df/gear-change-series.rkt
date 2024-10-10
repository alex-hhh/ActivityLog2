#lang racket/base

;; gear-change-series.rkt --
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2018-2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base)

(provide/contract
 (read-gear-change-series (-> data-frame? connection? any/c)))

;; Read gear change data from the database and add the respective series to
;; the data frame DF.
(define (read-gear-change-series df db)
  (define sid (df-get-property df 'session-id))
  (define gcdf (df-read/sql db "
select timestamp, front_gear_index, front_gear_teeth, rear_gear_index, rear_gear_teeth
 from GEAR_CHANGE
where session_id = ?
order by timestamp" sid))
  (df-set-sorted! gcdf "timestamp" <)

  (when (> (df-row-count gcdf 0))       ; only if we have any gear change data

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

    ;; TODO: these need to be user configurable.
    (define front-gears (vector 33 46))
    (define max-front-gears (vector-length front-gears))
    (define rear-gears (vector 36 32 28 24 21 19 17 15 13 12 11 10))
    (define max-rear-gears (vector-length rear-gears))

    (df-add-lazy!
     df
     "fg"
     '("timestamp")
     (lambda (v)
       (define timestamp (list-ref v 0))
       (define index (df-lookup gcdf "timestamp" "front_gear_index" timestamp))
       ;; NOTE gears are indexed at 1.
       (and (rational? index)
            (>= index 1)
            (<= index max-front-gears)
            (vector-ref front-gears (sub1 index)))))

    (df-add-lazy!
     df
     "rg"
     '("timestamp")
     (lambda (v)
       (define timestamp (list-ref v 0))
       (define index (df-lookup gcdf "timestamp" "rear_gear_index" timestamp))
       ;; NOTE gears are indexed at 1.
       (and (rational? index)
            (>= index 1)
            (<= index max-rear-gears)
            (vector-ref rear-gears (sub1 index)))))

    (df-add-lazy!
     df
     "gr"
     '("fg" "rg")
     (lambda (v)
       (match-define (list front rear) v)
       (and (rational? front)
            (rational? rear)
            (> rear 0)
            (/ front rear))))))
