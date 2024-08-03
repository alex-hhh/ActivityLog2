#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; traffic.rkt -- extract traffic information from MyBikeTraffic data series
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
         racket/match
         racket/contract)

;; Find the locations where an overtaking occurred based on data series from
;; MyBikeTraffic Garmin IQ application (mbrt_vehicle_count and
;; mbrt_absolute_speed) Return a list of the start and end data frame indexes
;; where overtaking takes place, the vehicle speed, the vehicle index and
;; number of vehicles part of this overtaking (a single overtaking event can
;; involve multiple vehicles)
;;
;; Traffic data is recorded by a Garmin Varia radar using the MyBikeTraffic IQ
;; field, see https://www.mybiketraffic.com/about/ for more details.

(define (extract-overtaking-locations df)
  (for/fold ([last-vcount 0]
             [prev-aspeed 0]
             [vnum 0]
             [tracking-start 0]
             [traffic-data '()]
             #:result (reverse traffic-data))
            ([(vcount aspeed)
              (in-data-frame df "mbrt_vehicle_count" "mbrt_absolute_speed")]
             [current-index (in-naturals)])
    (define num-vehicles
      (if (rational? vcount)
          ;; AB#66 -- mbrt_vehicle_count rolls over at 310 (which is an odd
          ;; value).  Handle it gracefully.
          (if (>= vcount last-vcount)
              (- vcount last-vcount)
              (add1 vcount))
          0))
    (define uvnum (+ vnum num-vehicles))
    (define utraffic-data
      (if (zero? num-vehicles)
          traffic-data
          (cons
           (list tracking-start current-index prev-aspeed uvnum num-vehicles)
           traffic-data)))
    (define utracking
      (if (and aspeed (zero? prev-aspeed) (positive? aspeed))
          current-index
          tracking-start))
    (values (or vcount 0)
            (or aspeed 0)
            uvnum
            utracking
            utraffic-data)))

;; Construct traffic (overtaking) information based on OLOCATIONS (As returned
;; by `extract-overtaking-locations` and the session DF) Returns a list of
;; hashes, one for each overtaking event, the hash contains useful info about
;; the overtaking event (see implementation for details).
(define (prepare-traffic-data olocations df)
  (define entries
    (for/list ([entry (in-list olocations)])
      (match-define (list start end vspeed vindex vcount) entry)
      (define data1
        (for/first ([(timestamp elapsed dst lat lon bike-spd grade)
                     ;; Scan backwards looking for the first entry that has
                     ;; good data, hopefully the last one.
                     (in-data-frame
                      df
                      "timestamp" "elapsed" "dst" "lat" "lon" "spd" "grade"
                      #:start (sub1 end) #:stop (sub1 start))]
                    #:when (and (rational? timestamp)
                                (rational? elapsed)
                                (rational? dst)
                                (rational? lat)
                                (rational? lon)
                                (rational? bike-spd)))
          (list timestamp elapsed dst lat lon bike-spd grade)))

      (define data2
        (for/first ([(elapsed dst)
                     (in-data-frame df "elapsed" "dst" #:start start #:stop end)]
                    #:when (and (rational? elapsed)
                                (rational? dst)))
          (list elapsed dst)))

      (define data3
        (for/fold ([max-vehicle-speed 0])
                  ([vehicle-speed (in-data-frame df "mbrt_absolute_speed" #:start start #:stop end)]
                   #:when (rational? vehicle-speed))
          (max max-vehicle-speed vehicle-speed)))

      (if (and data1 data2 data3)
          (match-let ([(list timestamp elapsed dst lat lon bike-spd grade) data1]
                      [(list selapsed sdst) data2])
            (hash 'timestamp timestamp
                  'elapsed elapsed
                  'distance dst
                  'rider-speed bike-spd
                  'vehicle-speed (and (rational? vspeed)
                                      (positive? vspeed)
                                      (/ vspeed 3.6))
                  'max-vehicle-speed (and (rational? data3)
                                          (/ data3 3.6))
                  'total-vehicles vindex
                  'vehicle-count vcount
                  'overtaking-speed (and (rational? vspeed)
                                         (positive? vspeed) ; MBRT uses "0" to mean "no data"...
                                         (rational? bike-spd)
                                         (- (/ vspeed 3.6) bike-spd))
                  'overtaking-distance (and (rational? dst)
                                            (rational? sdst)
                                            (- dst sdst))
                  'overtaking-duration (and (rational? elapsed)
                                            (rational? selapsed)
                                            (- elapsed selapsed))
                  'location (and (rational? lat)
                                 (rational? lon)
                                 (vector lat lon))
                  'road-slope grade
                  'df-index (vector start end)))
          #f)))
  ;; Remove #f values
  (filter values entries))

;; Get traffic data for a session data frame DF.  This information is
;; calculated once and cached as a property in the data-frame.  Return #f if
;; no traffic information is available.
(define (maybe-get-traffic-data! df)
  (or (df-get-property df 'traffic #f)
      (begin
        (df-put-property!
         df 'traffic
         (if (df-contains? df "mbrt_vehicle_count" "mbrt_absolute_speed" "spd" "dst" "elapsed" "timestamp" "lat" "lon" "grade")
             (let ([overtaking-locations (extract-overtaking-locations df)])
               (prepare-traffic-data overtaking-locations df))
             #f))
        (df-get-property df 'traffic #f))))

(provide/contract
 (maybe-get-traffic-data! (-> data-frame? any/c)))
