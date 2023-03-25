#lang racket/base
;; aerolab-storage.rkt -- store and retrieve aerolab data from the database
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db/base
         racket/contract
         racket/math
         racket/match)

(provide/contract
 [store-aerolab-parameters (-> connection? positive-integer? hash? any/c)]
 [fetch-aerolab-parameters (-> connection? positive-integer? (or/c hash? #f))]
 [drop-aerolab-parameters (-> connection? positive-integer? any/c)])

(define (store-aerolab-parameters db session-id params)
  (call-with-transaction
   db
   (lambda ()
     ;; To keep the code simple, instead of updating an existing row, we
     ;; simply delete the previous entry and add a new one.  Aerolab
     ;; parameters are referenced by session id anyway, see
     ;; `fetch-aerolab-parameters`
     (drop-aerolab-parameters db session-id)
     (query-exec
      db
      "insert into SESSION_AEROLAB(
             session_id,
             crr,
             cda,
             air_density,
             wind_speed,
             wind_direction,
             should_use_wind,
             total_weight,
             lap_count,
             trim_start,
             trim_end,
             altitude_offset,
             air_density_calculation_method,
             temperature,
             dew_point,
             pressure,
             humidity)
      values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      session-id
      (hash-ref params 'crr (lambda () sql-null))
      (hash-ref params 'cda (lambda () sql-null))
      (hash-ref params 'air-density (lambda () sql-null))
      (hash-ref params 'wind-speed (lambda () sql-null))
      (hash-ref params 'wind-direction (lambda () sql-null))
      (let ([use-wind? (hash-ref params 'use-wind? (lambda () #t))])
        (if use-wind? 1 0))
      (hash-ref params 'total-weight (lambda () sql-null))
      (hash-ref params 'lap-count (lambda () sql-null))
      (hash-ref params 'trim-start (lambda () sql-null))
      (hash-ref params 'trim-end (lambda () sql-null))
      (hash-ref params 'altitude-offset (lambda () sql-null))
      (let ([method (hash-ref params 'calculation-method (lambda () 'dew-point))])
        (if (equal? method 'dew-point) 0 1))
      (hash-ref params 'temperature (lambda () sql-null))
      (hash-ref params 'dew-point (lambda () sql-null))
      (hash-ref params 'pressure (lambda () sql-null))
      (hash-ref params 'humidity (lambda () sql-null))))))

(define (fetch-aerolab-parameters db session-id)
  (define row
    (query-maybe-row
     db
     "select crr,
             cda,
             air_density,
             wind_speed,
             wind_direction,
             should_use_wind,
             total_weight,
             lap_count,
             trim_start,
             trim_end,
             altitude_offset,
             air_density_calculation_method,
             temperature,
             dew_point,
             pressure,
             humidity
      from SESSION_AEROLAB
      where session_id = ?"
     session-id))
  (if row
      (match-let ([(vector crr
                           cda
                           air-density
                           wind-speed
                           wind-direction
                           use-wind?
                           total-weight
                           lap-count
                           trim-start
                           trim-end
                           altitude-offset
                           calculation-method
                           temperature
                           dew-point
                           pressure
                           humidity)
                   row])
        (define h0
          (hash 'crr crr
                'cda cda
                'air-density air-density
                'wind-speed wind-speed
                'wind-direction wind-direction
                'use-wind? (if (> use-wind? 0) #t #f)
                'total-weight total-weight
                'lap-count lap-count
                'trim-start trim-start
                'trim-end trim-end
                'altitude-offset altitude-offset
                'calculation-method (if (equal? calculation-method 0) 'dew-point 'humidity)
                'temperature temperature
                'dew-point dew-point
                'pressure pressure
                'humidity humidity))
        ;; Filter out the SQL null values
        (for/hash ([(k v) (in-hash h0)] #:unless (sql-null? v))
          (values k v)))
      #f))

(define (drop-aerolab-parameters db session-id)
  (query-exec
   db
   "delete from SESSION_AEROLAB where session_id = ?" session-id))
