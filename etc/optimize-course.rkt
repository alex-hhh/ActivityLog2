#lang racket

;; SPDX-License-Identifier: GPL-3.0-or-later
;; optimize-course.rkt -- experimental functions to re-sample GPX files
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         data-frame/gpx
         geoid/geodesy
         geoid
         racket/generator
         "al-interactive.rkt"
         "../rkt/models/elevation-correction.rkt")

;; Return a list of geoids that follow the course in the data frame df, but
;; interpolated so there is at most MAX-STEP-DISTANCE meters between adjacent
;; geoids.
;;
;; We return a list of (vector alt the-geoid), which is suitable for input to
;; elevation-correction/geoid.  We don't need an ID, so we use the altitude,
;; since that function requires a value to be present at that slot.
(define (upsample-locations df max-step-distance)
  (for/list
    (((lat1 lon1 alt1) (in-data-frame df "lat" "lon" "alt" #:start 0))
     ((lat2 lon2 alt2) (in-sequences
                        (in-data-frame df "lat" "lon" "alt" #:start 1)
                        ;; add an extra element to the end of the sequence, so the
                        ;; last element in the data frame is included in the
                        ;; interpolation.
                        (in-generator #:arity 3 (yield #f #f #f))))
     #:do ((define-values (lat1^ lon1^ lat2^ lon2^)
             (values (degrees->radians lat1)
                     (degrees->radians lon1)
                     (and lat2 (degrees->radians lat2))
                     (and lon2 (degrees->radians lon2))))
           (define-values (distance bearing _final-bearing)
             (if (and lat2^ lon2^)
                 (vincenty-inverse lon1^ lat1^ lon2^ lat2^ #:ellipsoid (geodesy-ellipsoid))
                 (values max-step-distance 0.0 0.0)))
           (define step-count (exact-ceiling (/ distance max-step-distance)))
           (define step-length (if (zero? step-count) 0 (/ distance step-count)))
           (define alt-delta (if (or (zero? step-count)
                                     (not (rational? alt1))
                                     (not (rational? alt2)))
                                 0
                                 (/ (- alt2 alt1) step-count))))
     (step (in-range step-count)))
    (define-values (nlon nlat _new-bearing)
      (vincenty-direct lon1^ lat1^ bearing (* step step-length)))
    (define-values (lat lon) (values (radians->degrees nlat) (radians->degrees nlon)))
    (vector (and alt1 (+ alt1 (* step alt-delta)))
            (lat-lng->geoid lat lon))))


;; Return a list of segments (indices in the ECPOINTS vector), that represent
;; a linear altitude change. ECPOINTS is a vector of ECPOINT structures, as
;; returned by the elevation-correction/geoid function.
;;
;; Each segment is a list of start-index/end-index, and represents a list of
;; points that could be removed without affecting the elevation profile.
;;
;; THRESHOLD represents the value in meters that a point can deviate from the
;; line between a start and end index before it is considered "off the line".
(define (linear-altitude-segments ecpoints threshold)

  (define (linear-segments start end accumulator)
    (define-values (start-dst start-alt)
      (let ([p (vector-ref ecpoints start)])
        (values (ecpoint-dst p) (ecpoint-calt p))))
    (define-values (end-dst end-alt)
      (let ([p (vector-ref ecpoints end)])
        (values (ecpoint-dst p) (ecpoint-calt p))))
    (define slength (- end-dst start-dst))
    (define sheight (- end-alt start-alt))
    (define-values (max-deviation max-deviation-index)
      (for/fold ([max-deviation 0.0]
                 [max-deviation-index #f])
                ([index (in-range (add1 start) end)])
        (define ec (vector-ref ecpoints index))
        (define talt (+ start-alt (* sheight (/ (- (ecpoint-dst ec) start-dst) slength))))
        (define calt (ecpoint-calt ec))
        (define deviation (abs (- calt talt)))
        (if (> deviation max-deviation)
            (values deviation index)
            (values max-deviation max-deviation-index))))

    (if (< max-deviation threshold)
        (cons (list start end) accumulator)
        (let ([na (linear-segments start max-deviation-index accumulator)])
          (linear-segments max-deviation-index end na))))

  (reverse (linear-segments 0 (sub1 (vector-length ecpoints)) '())))

;; Reduce the number of ECPOINTS between critical points (LINEAR-SEGMENTS).
;; points are discarded as long as the straight line between a "start" and
;; "end" point is within THRESHOLD meters from the line along the intermediate
;; points between the same "start" and "end" points.
;;
;; Returns a LIST (not a vector!) of ECPOINTS
(define (downsample-locations ecpoints linear-segments threshold)

  (define (simplify-segment start end accumulator)
    (define first-point (vector-ref ecpoints start))
    (for/fold ([track (cons first-point accumulator)]
               [sp first-point] ; segment start point
               [pp first-point] ; previous point
               [distance 0]
               #:result track)
              ((n (in-inclusive-range (add1 start) end)))
      (define cp (vector-ref ecpoints n)) ; current point
      ;; NDIST is the distance along the segment, following the points up to the
      ;; current point, BDIST is the direct distance between the start of the
      ;; segment and the current point
      (define ndist (+ distance (distance-between-geoids (ecpoint-geoid pp) (ecpoint-geoid cp))))
      (define bdist (distance-between-geoids (ecpoint-geoid sp) (ecpoint-geoid cp)))
      (if (< (abs (- bdist ndist)) threshold)
          (values track sp cp ndist)
          (values (cons pp track) cp cp 0))))

  (for/fold ([result '()]
             #:result
             (let ([last-point (vector-ref ecpoints (sub1 (vector-length ecpoints)))])
               (reverse (cons last-point result))))
            ([e (in-list linear-segments)])
    (match-define (list start end) e)
    (simplify-segment start end result)))

;; Construct a data frame containing lat, lon, dst and altitude from the list
;; of ecpoint structures (which are returned by the elevation correction code)
(define (ecpoints->df ecpoints)
  (for/data-frame (lat lon dst alt)
    ((p (in-list ecpoints)))
    (define-values (lat lon) (geoid->lat-lng (ecpoint-geoid p)))
    (values lat lon (ecpoint-dst p) (ecpoint-calt p))))

(define (optimise-route df
                        #:elevation-database (edb (current-database))
                        #:max-step-distance (msd 25)
                        #:elevation-threshold (et 1.0)
                        #:distance-threshold (dt 3.0))
  (define upsampled (upsample-locations df msd))
  (define corrected
    (elevation-correction/geoid edb upsampled #:smooth-altitude? #f))
  ;; Fill in missing altitude values where the elevation correction algorithm
  ;; did not have any data with original (interpolated) altitudes from the
  ;; course file.
  (for ([c (in-vector corrected)]
        [u (in-list upsampled)]
        #:unless (ecpoint-calt c))
    (set-ecpoint-calt! c (vector-ref u 0)))
  (smooth-altitude! corrected #t)
  (define ls (linear-altitude-segments corrected et))
  (define downsampled (downsample-locations corrected ls dt))
  (ecpoints->df downsampled))

(define c (df-read/gpx "C:/Users/alexh/OneDrive/Docs/Riding/Courses/CL50-10 Canning Mills Rd.gpx"))
(define optimized
  (optimise-route c
                  #:elevation-database (current-database)
                  #:max-step-distance 25
                  #:elevation-threshold 1
                  #:distance-threshold 1))

(df-write/gpx
 optimized
 "O-CL50-10 Canning Mills Rd.gpx"
 #:name "CL50-10 Canning Mills Rd")
