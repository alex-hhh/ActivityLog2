#lang racket/base
;; elevation-correction.rkt -- elevation correction for trackpoints
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020, 2021, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;; Commentary:
;;
;; This code works on the assumption that a certain route is traversed many
;; times (in different sessions or as laps of the same session), therefore
;; multiple elevation readings are available for the same positions (or close
;; by) at different times (as different A_TRACKPOINT records).  The code works
;; by averaging the elevation of all the available trackpoints recorded around
;; certain position.
;;
;; It will work best if a route is traversed several times and won't do
;; anything useful for a route that is traversed only once.

(require db/base
         geoid
         racket/class
         racket/contract
         racket/flonum
         racket/list
         racket/match
         racket/math
         racket/runtime-path
         racket/vector
         (only-in "../dbutil.rkt" define-sql-statement))


;;....................................................... geoid altitude ....

(define-runtime-path geoid-altitudes-query-file "../../sql/queries/ec-geoid-altitudes.sql")
(define geoid-altitudes-query (define-sql-statement geoid-altitudes-query-file))

;; A single track point produced by `elevation-correction`, holds the input ID
;; (which is passed into `elevation-correction`, the geoid, distance from the
;; start (DST) and the corrected altitude (CALT)
(struct ecpoint (id geoid dst (calt #:mutable)) #:transparent)

;; Helper class to manage the averaging of altitude data from GEOIDs.  To
;; avoid retrieving the same data repeatedly, we cache various intermediate
;; results.  The only public method in this class is `elevation-correction`.
;;
;; If you need to correct the elevation on several tracks, it is more
;; efficient to create one `ec-helper%` instance and call the
;; `elevation-correction` method once for each track. For once-off use, see
;; the elevation-correction function.
(define ec-helper%
  (class object%
    (init-field database)
    (super-new)

    ;; WARNING: all the hashes below are immutable.  Geoids hash poorly with
    ;; mutable hash tables and there is a huge performance penalty when they
    ;; are used as keys in mutable hash tables.
    ;;
    ;; NOTE: the poor hashing problem was fixed around Racket 8.2, we should
    ;; revisit this to see if mutable hashes provide better performance...

    ;; Fetch the list of original altitudes (A_TRACKPOINT.alt) for all geoids
    ;; in the database DB, which are inside GEOID (this is assumed to NOT be a
    ;; leaf geoid).  Returns a list of numbers representing the altitudes.
    (define/private (fetch-altitude-samples geoid)
      (define-values (start end) (leaf-span geoid))
      (query-list database
                  (geoid-altitudes-query)
                  (geoid->sqlite-integer start)
                  (geoid->sqlite-integer end)))

    ;; Map the altitude samples for a GEOID, this stores results previously
    ;; retrieved by `fetch-altitude-samples`
    (define altitude-sample-cache (hash))

    ;; Return the list of altitudes for all geoids in the database which are
    ;; inside GEOID (which is assumed to NOT be a leaf geoid).  This method
    ;; maintains the `altitude-sample-cache` cache to avoid retrieving the
    ;; same data multiple times.
    (define/private (altitude-samples geoid)
      ;;(define key (~a geoid))
      (define key geoid)
      (define data (hash-ref altitude-sample-cache key #f))
      (unless data
        (set! data (fetch-altitude-samples geoid))
        (set! altitude-sample-cache (hash-set altitude-sample-cache key data)))
      data)

    ;; Determine the altitude limits which would be considered outliers based
    ;; on all the altitude data in GEOID and its adjacent neighbors.  This
    ;; data will be used to throw out unreasonable altitude values (GPS based
    ;; altimeters can record altitude values which are off by hundreds of
    ;; meters).
    ;;
    ;; We need to consider the neighbors since an individual geoid might
    ;; contain a single elevation sample, which happens to be an outlier when
    ;; compared to other close-by altitudes from neighbor geoids.
    ;;
    ;; Returns a cons cell of the lower and upper range, or a cons cell of #f
    ;; values if there are no altitude values in this GEOID.
    ;;
    ;; IMPLEMENTATION NOTE: we use the following method to determine the
    ;; outliers: the 25% and 75% quantiles are determined from all the
    ;; altitude values from GEOID and its neighbors.  The difference between
    ;; the two values is the inter-quantile range (IQR).  The upper and lower
    ;; limits are determined as 1.5 times the IQR over the 75% and under the
    ;; 25% limits.

    (define/private (determine-outlier-limits geoid)
      (define candidates (cons geoid (adjacent-geoids geoid)))
      (define data (append*
                    (for/list ([g (in-list candidates)])
                      (altitude-samples g))))
      (define nitems (length data))
      (cond ((= nitems 0) (cons #f #f))
            ((= nitems 1)
             (let ([altitude (car data)])
               (cons (- altitude 1.5) (+ altitude 1.5))))
            (#t
             (define scale 1.5)
             (define buf (list->vector data))
             (vector-sort! buf <)
             (define q25-pos (max 0 (exact-ceiling (- (* 0.25 nitems) 1))))
             (define q75-pos (max 0 (exact-ceiling (- (* 0.75 nitems) 1))))
             (define q25 (vector-ref buf q25-pos))
             (define q75 (vector-ref buf q75-pos))
             (define inter-quantile-range (- q75 q25))
             (define upper-limit (+ q75 (* scale inter-quantile-range)))
             (define lower-limit (- q25 (* scale inter-quantile-range)))
             (cons lower-limit upper-limit))))

    ;; Map the outlier limits for altitude data for a GEOID.  The key is the
    ;; geoid, the value is a cons cell of the low and high values.
    (define outlier-limit-cache (hash))

    ;; Return the outlier limits for GEOID -- this calls
    ;; `determine-outlier-limits`, but stores the retrieved values in
    ;; `outlier-limit-cache` hash, to avoid re-calculating them.
    (define/private (outlier-limits geoid)
      ;;(define key (~a geoid))
      (define key geoid)
      (define limits (hash-ref outlier-limit-cache key #f))
      (unless limits
        (set! limits (determine-outlier-limits geoid))
        (set! outlier-limit-cache (hash-set outlier-limit-cache key limits)))
      limits)

    ;; Determine the altitude for GEOID by averaging the altitude values for
    ;; all geoids inside it, after outlier points have been removed.

    ;; NOTE that we determine the outliers for geoid level 14 (and its
    ;; adjacent neighbors).  We use a larger geoid, since the current one will
    ;; be quite small (8-10 level) and might not contain enough points to
    ;; determine relevant outlier limits.

    (define/private (determine-average-altitude geoid)
      (match-define (cons lower-limit upper-limit) (outlier-limits (enclosing-geoid geoid 14)))
      (define candidates (altitude-samples geoid))
      (for/fold ([sum 0]
                 [cnt 0]
                 #:result (if (> cnt 0) (/ sum cnt) #f))
                ([p (in-list candidates)]
                 #:when (and (>= p lower-limit) (<= p upper-limit)))
        (values (+ sum p) (add1 cnt))))

    ;; Map the calculated average altitude for each geoid
    (define average-altitude-cache (hash))

    ;; Return the average altitude for GEOID, based on all the altitudes for
    ;; the geoids in the database which are inside it.  This uses
    ;; `determine-average-altitude` to do the actual calculation and caches the
    ;; result in `altitude-for-geoid`
    (define/private (average-altitude geoid)
      (define key geoid)
      (define altitude (hash-ref average-altitude-cache key #f))
      (unless altitude
        (set! altitude (determine-average-altitude geoid))
        (set! average-altitude-cache (hash-set average-altitude-cache key altitude)))
      altitude)

    ;; Return the (weighted) average altitude at GEOID (presumably a leaf
    ;; geoid), based on the average altitude of nearby area geoids (level 10).
    ;; Note that this method will return #f if the corrected altitude cannot
    ;; be determined (e.g. because there is not enough data in the database.
    ;;
    ;; FETCH-ALTITUDE is a function which returns the altitude for a given GEOID.
    (define/private (weighted-average-altitude geoid)
      ;; Distance in meters from GEOID where the weight will be 0.5.  Points
      ;; closer than this will have a weight growing towards 1, 1 being the
      ;; weight of the point exactly at GEOID, points further away than this
      ;; value will have their weight further decreasing towards 0.
      (define hw-distance (->fl 5))

      ;; We determine the average altitude of the geoid which encloses our
      ;; geoid and its adjacent neighbors -- level 10 is about 8x8 meters.
      (define area-geoid (enclosing-geoid geoid 10))

      ;; We iterate over each candidate geoid use and compute w weighted
      ;; average of their altitudes.  The average is weighted because we are
      ;; unlikely to find a point at this exact location and we don't want to
      ;; straight average points at other nearby locations.
      (define-values (sum div)
        (for/fold ([sum 0.0]
                   [div 0.0])
                  ([g (in-list (cons area-geoid (adjacent-geoids area-geoid)))])
          (define avg-alt (average-altitude g))
          (if avg-alt
              (let* [(distance (distance-between-geoids geoid g))
                     [weight (flmax 0.0 (fl/ hw-distance (fl+ distance hw-distance)))]]
                (values (fl+ (fl* weight avg-alt) sum) (fl+ div weight)))
              (values sum div))))

      (if (> div 0) (fl/ sum div) #f))

    ;; Calculate the corrected altitude for all points in TRACKPOINTS, a list
    ;; of vectors of ID, GEOID.  Calls `CALCULATE-AVERAGE-ALTITUDE for each
    ;; point in TRACKPOINTS and returns vector of ECPOINT structures.
    (define/private (weighted-altitude-for-trackpoints
                     trackpoints [progress-monitor #f] [progress-step 100])
      (when progress-monitor
        (send progress-monitor begin-stage
              "Calculating GPS track altitude" (length trackpoints)))
      (if (null? trackpoints)
          (vector)
          (let* ([first-point (car trackpoints)]
                 [prev-geoid (vector-ref first-point 1)]
                 [distance 0])
            (for/vector #:length (length trackpoints)
                        ([point (in-list trackpoints)]
                         [index (in-naturals)])
              (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
                (send progress-monitor set-progress (add1 index)))
              (match-define (vector id geoid) point)
              (set! distance (+ distance (distance-between-geoids prev-geoid geoid)))
              (set! prev-geoid geoid)
              (ecpoint id geoid distance (weighted-average-altitude geoid))))))

    ;; Smooth the altitude in TRACKPOINTS using a low pass filter.  The filter
    ;; width is determined empirically, but it is adjusted to the average
    ;; distance between the points on the track, meaning slower activities
    ;; (e.g. hiking) have less smoothing than faster ones (e.g. cycling)
    (define/private (smooth-altitude trackpoints)
      (define point-count (vector-length trackpoints))
      (define average-delta-distance
        (/ (ecpoint-dst (vector-ref trackpoints (sub1 point-count)))
           point-count))
      (define filter-width (* 15.0 average-delta-distance))
      (for/fold
      ([pdst (ecpoint-dst (vector-ref trackpoints 0))]
       [palt (ecpoint-calt (vector-ref trackpoints 0))])
      ([p (in-vector trackpoints 1)])
        (match-define (ecpoint _id _geoid dst alt) p)
        (if palt
            (if alt
                (let* ([delta (- dst pdst)]
                       [alpha (/ delta (+ delta filter-width))]
                       [salt (+ (* alpha alt) (* (- 1.0 alpha) palt))])
                  (set-ecpoint-calt! p salt)
                  (values dst salt))
                (begin
                  (set-ecpoint-calt! p palt)
                  (values dst palt)))
            (values dst alt))))

    ;; trackpoints is a list of vectors, each vector has at least 2 slots: the
    ;; first one is an ID (copied in the returned ecpoint vector and the second
    ;; one is a GEOID representing the location.
    (define/public (elevation-correction/geoid
                    trackpoints
                    #:progress-monitor [progress-monitor #f]
                    #:progress-step [progress-step 100])
      (define tp (weighted-altitude-for-trackpoints
                  trackpoints progress-monitor progress-step))
      (smooth-altitude tp)
      tp)

    ))

;; Simple wrapper for correcting elevation of a single set of trackpoints.
;; For multiple tracks, it is more efficient to create an `ec-helper%`
;; instance and call the `elevation-correction` method for each track, as the
;; `ec-helper%` class caches calculations.
;;
;; trackpoints is a list of vectors, each vector has at least 2 slots: the
;; first one is an ID (copied in the returned ecpoint vector and the second
;; one is a GEOID representing the location.
(define (elevation-correction/geoid db trackpoints)
  (define helper (new ec-helper% [database db]))
  (send helper elevation-correction/geoid trackpoints))


;;............................................................. provides ....

(provide (struct-out ecpoint))

(define ec-helper%/c
  (class/c
   (init [database connection?])
   (elevation-correction/geoid
    (->*m ((listof (vector/c any/c valid-geoid?)))
          (#:progress-monitor (or/c #f object?)
           #:progress-step positive-integer?)
          (vectorof ecpoint?)))))

(provide/contract
 [elevation-correction/geoid
  (-> connection?
      (listof (vector/c any/c valid-geoid?))
      (vectorof ecpoint?))]
 [ec-helper% ec-helper%/c])
