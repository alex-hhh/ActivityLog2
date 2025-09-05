#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; gps-segment.rkt -- GPS Segments implementation
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021-2022, 2024, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         db/base
         geoid
         geoid/geodesy
         geoid/waypoint-alignment
         math/statistics
         racket/async-channel
         racket/contract
         racket/generator
         racket/match
         racket/math
         racket/port
         "../database.rkt"
         "../dbutil.rkt"
         "../intervals.rkt"
         "../models/fiets-score.rkt"
         "../models/grade-series.rkt"
         "../utilities.rkt")

;; Return a new gps segment that follows the same course in the data frame df,
;; but interpolated so there is at most MAX-STEP-DISTANCE meters between
;; adjacent points.  This version assumes the data frame has an altitude
;; series ("alt") which is interpolated between newly generated points
(define (upsample-segment-data/alt df max-step-distance)
  (for/data-frame (lat lon alt geoid)
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
    (define-values (lat lon)
      (values (radians->degrees nlat) (radians->degrees nlon)))
    (values lat lon (and alt1 (+ alt1 (* step alt-delta)))
            (lat-lng->geoid lat lon))))

;; Same as upsample-segment-data/alt, but no altitude series is interpolated
;; and generated in the returned data frame
(define (upsample-segment-data/no-alt df max-step-distance)
  (for/data-frame (lat lon geoid)
    (((lat1 lon1) (in-data-frame df "lat" "lon" #:start 0))
     ((lat2 lon2) (in-sequences
                   (in-data-frame df "lat" "lon" #:start 1)
                   ;; add an extra element to the end of the sequence, so the
                   ;; last element in the data frame is included in the
                   ;; interpolation.
                   (in-generator #:arity 2 (yield #f #f))))
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
           (define step-length (if (zero? step-count) 0 (/ distance step-count))))
     (step (in-range step-count)))
    (define-values (nlon nlat _new-bearing)
      (vincenty-direct lon1^ lat1^ bearing (* step step-length)))
    (define-values (lat lon)
      (values (radians->degrees nlat) (radians->degrees nlon)))
    (values lat lon (lat-lng->geoid lat lon))))

;; Add all necessary data series and properties to a segment -- the segment
;; must have latitude and longitude data, and this function will construct all
;; derived data series and properties -- if a data series or property already
;; exists, it will not be recreated.
(define (fixup-segment-data df)
  (unless (df-contains? df "lat" "lon")
    (error (format "fixup-segment-data: segment must contain \"lat\" and \"lon\" series")))

  (unless (df-contains? df "geoid")
    (df-add-derived!
     df
     "geoid"
     '("lat" "lon")
     (lambda (v)
       (match-define (list lat lon) v)
       (and lat lon (lat-lng->geoid lat lon)))))

  (if (df-contains? df "dst")
      ;; Maybe fixup the dst series to start from 0 (GPX files might contain a
      ;; dst series which does not)
      (let ([sentinel (df-ref df 0 "dst")])
        (unless (zero? sentinel)
          (df-add-derived!
           df
           "dst"
           '("dst")
           (lambda (v)
             (- (car v) sentinel)))))
      ;; Add the dst series, if not present
      (let ([current-distance 0])
        (df-add-derived!
         df
         "dst"
         '("geoid")
         (lambda (prev next)
           (when (and prev next (car prev) (car next))
             (let ([delta (distance-between-geoids (car prev) (car next))])
               (set! current-distance (+ current-distance delta))))
           current-distance))))
  (df-set-sorted! df "dst" <)

  (define segment-length
    (let ([start (df-ref df 0 "dst")]
          [end (df-ref df (sub1 (df-row-count df)) "dst")])
      (and start end (- end start))))
  (df-put-property! df 'segment-length segment-length)

  (when (df-contains? df "alt")
    (add-grade-series! df)
    (define max-grade
      (df-fold df "grade" 0
               (lambda (accumulator v)
                 (if (car v)
                     (max accumulator (car v))
                     accumulator))))

    (define-values (total-ascent total-descent)
      (total-ascent-descent df "alt" 0 (df-row-count df)))

    (df-put-property! df 'total-ascent total-ascent)
    (df-put-property! df 'total-descent total-descent)
    (df-put-property! df 'max-grade max-grade)

    (define elevation-stats (df-statistics df "alt"))
    (define min-elevation (statistics-min elevation-stats))
    (define max-elevation (statistics-max elevation-stats))
    (df-put-property! df 'min-elevation min-elevation)
    (df-put-property! df 'max-elevation max-elevation)

    (define segment-height
      (let ([bottom
             (for/or ([index (in-range (df-row-count df))])
               (df-ref df index "alt"))]
            [top
             (for/or ([index (in-inclusive-range (sub1 (df-row-count df)) 0 -1)])
               (df-ref df index "alt"))])
        (and bottom top (- top bottom))))

    ;; NOTE: this grade calculation only works correctly for segments which
    ;; are climbs, but I suspect most users would expect average grade to be
    ;; calculated this way.
    (define segment-grade
      (and segment-height segment-length
           (* (/ segment-height segment-length) 100.0)))

    (when segment-height
      (df-put-property! df 'segment-height segment-height))
    (when segment-grade
      (df-put-property! df 'segment-grade segment-grade)))

  (let ([score (fiets-score df)])
    (when score
      (df-put-property! df 'fiets-score score))))

;; Create a segment from a GPX file -- this uses `df-read/gpx` than
;; `fixup-segment-data` to add any series and properties which are missing.
(define (gps-segment-from-gpx input)
  (define df (df-read/gpx input))
  (define df1
    (if (df-contains? df "alt")
        (upsample-segment-data/alt df 20)
        (upsample-segment-data/no-alt df 20)))
  (fixup-segment-data df1)
  df1)

;; Create a new GPS segment by reversing ORIGINAL, that is we traverse this
;; segment in the opposite direction for the latitude, longitude and altitude
;; series, than call `fixup-segment-data` to create all the other necessary
;; series.
(define (gps-segment-by-reversing original)
  (unless (df-contains? original "lat" "lon")
    (error (format "gps-segment-by-reversing: segment must contain \"lat\" and \"lon\" series")))
  (define reversed
    (if (df-contains? original "alt")
        (for/data-frame (lat lon alt)
          ([(lat lon alt) (in-data-frame original
                                         "lat" "lon" "alt"
                                         #:start (sub1 (df-row-count original))
                                         #:stop -1)])
          (values lat lon alt))
        (for/data-frame (lat lon)
          ([(lat lon) (in-data-frame original
                                     "lat" "lon"
                                     #:start (sub1 (df-row-count original))
                                     #:stop -1)])
          (values lat lon))))
  (define name (df-get-property original 'name #f))
  (when name
    (df-put-property! reversed 'name (string-append name " (reversed)")))
  (fixup-segment-data reversed)
  reversed)

(define-runtime-path gs-insert-segment-path "../../sql/queries/gs-insert-segment.sql")
(define gs-insert-segment (define-sql-statement gs-insert-segment-path))
(define-runtime-path gs-insert-waypoint-path "../../sql/queries/gs-insert-waypoint.sql")
(define gs-insert-waypoint (define-sql-statement gs-insert-waypoint-path))
(define-runtime-path gs-fetch-waypoints-path "../../sql/queries/gs-fetch-waypoints.sql")
(define gs-fetch-waypoints (define-sql-statement gs-fetch-waypoints-path))
(define-runtime-path gs-fetch-segment-summary-path "../../sql/queries/gs-fetch-segment-summary.sql")
(define gs-fetch-segment-summary (define-sql-statement gs-fetch-segment-summary-path))

;; Store the SEGMENT into the DATABASE and return its segment identifier (the
;; value of GPS_SEGMENT.id) -- this will always create a new segment even if
;; the SEGMENT was previously fetched from the database.
(define (put-new-gps-segment database segment)
  (call-with-transaction
   database
   (lambda ()
     (define segment-id
       (db-insert
        database
        (gs-insert-segment)
        (df-get-property segment 'name "Unnamed")
        (df-get-property segment 'segment-length) ; must exist
        (df-get-property segment 'segment-height sql-null)
        (df-get-property segment 'segment-grade sql-null)
        (df-get-property segment 'total-ascent sql-null)
        (df-get-property segment 'total-descent sql-null)
        (df-get-property segment 'max-grade sql-null)
        (df-get-property segment 'min-elevation sql-null)
        (df-get-property segment 'max-elevation sql-null)
        (df-get-property segment 'fiets-score sql-null)))

     (if (df-contains? segment "alt" "grade")
         (for ([(lat lon geoid dst alt grade)
                (in-data-frame segment "lat" "lon" "geoid" "dst" "alt" "grade")]
               [pos (in-naturals)])
           (db-insert database (gs-insert-waypoint) segment-id pos lat lon dst
                      (geoid->sqlite-integer geoid)
                      (or alt sql-null)
                      (or grade sql-null)))
         (for ([(lat lon geoid dst)
                (in-data-frame segment "lat" "lon" "geoid" "dst")]
               [pos (in-naturals)])
           (db-insert database (gs-insert-waypoint) segment-id pos lat lon dst
                      (geoid->sqlite-integer geoid) sql-null sql-null)))

     segment-id)))

;; Fetch a segment identified by SEGMENT-ID (a GPS_SEGMENT.id value) from the
;; DATABASE.  This function will always read the segment from the database,
;; and you should use `gps-segment-df` for general use, as that function
;; caches segments.
(define (fetch-gps-segment database segment-id)
  (define df (df-read/sql database (gs-fetch-waypoints) segment-id))

  ;; Fix the geoid series, as geoids are stored in a special way in the
  ;; database...
  (df-add-derived!
   df "geoid" '("geoid") (lambda (v) (sqlite-integer->geoid (car v))))

  (when (and (df-contains? df "alt") (not (df-has-non-na? df "alt")))
    (df-del-series! df "alt"))
  (when (and (df-contains? df "grade") (not (df-has-non-na? df "grade")))
    (df-del-series! df "grade"))
  (df-set-sorted! df "dst" <)
  (match-define
    (vector name segment-length segment-height segment-grade
            total-ascent total-descent max-grade min-elevation max-elevation fiets-score)
    (query-row database (gs-fetch-segment-summary) segment-id))

  (unless (sql-null? name)
    (df-put-property! df 'name name))
  (unless (sql-null? segment-length)
    (df-put-property! df 'segment-length segment-length))
  (unless (sql-null? segment-height)
    (df-put-property! df 'segment-height segment-height))
  (unless (sql-null? segment-grade)
    (df-put-property! df 'segment-grade segment-grade))
  (unless (sql-null? total-ascent)
    (df-put-property! df 'total-ascent total-ascent))
  (unless (sql-null? total-descent)
    (df-put-property! df 'total-descent total-descent))
  (unless (sql-null? max-grade)
    (df-put-property! df 'max-grade max-grade))
  (unless (sql-null? min-elevation)
    (df-put-property! df 'min-elevation min-elevation))
  (unless (sql-null? max-elevation)
    (df-put-property! df 'max-elevation max-elevation))
  (unless (sql-null? fiets-score)
    (df-put-property! df 'fiets-score fiets-score))

  ;; Store this so we know where this segment came from
  (df-put-property! df 'segment-id segment-id)

  df)

;; Delete the GPS segment identified by SEGMENT-ID from the DATABASE.  Any
;; associated segment matches are also deleted.
(define (delete-gps-segment database segment-id)
  (call-with-transaction
   database
   (lambda ()
     (define matches
       (query-list database "select id from GPS_SEGMENT_MATCH where segment_id = ?" segment-id))
     (for ([match-id (in-list matches)])
       (delete-segment-match database match-id))
     (query-exec database "delete from GPS_SEGMENT where id = ?" segment-id))))

(define-runtime-path gs-find-trackpoint-path "../../sql/queries/gs-find-trackpoint.sql")
(define gs-find-trackpoint (define-sql-statement gs-find-trackpoint-path))
(define-runtime-path gs-insert-match-path "../../sql/queries/gs-insert-match.sql")
(define gs-insert-match (define-sql-statement gs-insert-match-path))

;; Store a new segment match in the database.  SEGMENT-ID is the segment that
;; has the match, DF represents the session data frame that matches and the
;; match is between START-INDEX and END-INDEX rows.  Finally, MATCH-COST
;; represents the cost of the match (smaller is better), this value is
;; determined by the `waypoint-alignment-cost` function.
;;
;; Returns the id of the new match row (GPS_SEGMENT_MATCH.id)
;;
(define (put-new-segment-match database segment-id df start-index end-index match-cost)
  (define session-id (df-get-property df 'session-id))
  (define summary (make-interval-summary df start-index end-index))
  (define start-trackpoint-id
    (query-value database (gs-find-trackpoint) session-id (df-ref df start-index "timestamp")))
  (define end-trackpoint-id
    (query-value database (gs-find-trackpoint) session-id (df-ref df end-index "timestamp")))

  (call-with-transaction
   database
   (lambda ()
     (define summary-id (db-insert-section-summary summary database))
     (db-insert database (gs-insert-match)
                segment-id session-id summary-id start-trackpoint-id end-trackpoint-id match-cost))))

;; Delete a segment match MATCH-ID from the database.
(define (delete-segment-match database match-id)
  (call-with-transaction
   database
   (lambda ()
     (define summary-id
       (query-maybe-value database "select summary_id from GPS_SEGMENT_MATCH where id = ?" match-id))
     (query-exec database "delete from GPS_SEGMENT_MATCH where id = ?" match-id)
     (when summary-id
       (query-exec database "delete from SECTION_SUMMARY where id = ?" summary-id)))))

;; Create an SQL quest string which finds all sessions that cross the
;; LEAF-SPAN-SET, as returned by the `leaf-span*` function.  Unfortunately, I
;; don't know how to express this as a prepared query to pass in the leaf
;; spans as parameters...
(define (make-leaf-span-set-sql-query leaf-span-set)
  (call-with-output-string
   (lambda (out)
     (write-string "select distinct P.session_id
from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where T.length_id = L.id and L.lap_id = P.id and ("
                   out)
     (define first-time? #t)
     (for ([leaf-span (in-list leaf-span-set)])
       (match-define (cons start end) leaf-span)
       (if first-time?
           (set! first-time? #f)
           (write-string " or " out))
       (write-string
        (format " T.geoid between ~a and ~a "
                (geoid->sqlite-integer start) (geoid->sqlite-integer end))
        out))
     (write-string ")" out))))

;; Same as MAKE-LEAF-SPAN-SET-SQL-QUERY, but only searches the activities from
;; LAST_IMPORT table -- that is, the activities that were last imported.
(define (make-leaf-span-set-sql-query/last-import leaf-span-set)
  (call-with-output-string
   (lambda (out)
     (write-string "select distinct P.session_id
from A_TRACKPOINT T, A_LENGTH L, A_LAP P, A_SESSION S, LAST_IMPORT LI
where T.length_id = L.id
  and L.lap_id = P.id
  and P.session_id = S.id
  and S.activity_id = LI.activity_id
  and ("
                   out)
     (define first-time? #t)
     (for ([leaf-span (in-list leaf-span-set)])
       (match-define (cons start end) leaf-span)
       (if first-time?
           (set! first-time? #f)
           (write-string " or " out))
       (write-string
        (format " T.geoid between ~a and ~a "
                (geoid->sqlite-integer start) (geoid->sqlite-integer end))
        out))
     (write-string ")" out))))

;; Geoid level used for searching sessions which cross a specific geoid.
(define default-search-geoid-level 11)  ; Approx 4.4 x 4.4 meters

;; Return a list of session IDs whose path cross (or go nearby) GEOID.
;;
;; NOTE: the geoid package does not support "caps" yet, so we just get the 9x9
;; neighborhood of the geoid after upscaling it to *search-geoid-level*
(define (find-nearby-sessions database geoid
                              #:search-geoid-level (sgl default-search-geoid-level))
  (let* ([wpt^ (enclosing-geoid geoid sgl)]
         [neighbourhood (adjacent-geoids wpt^)]
         [lss (leaf-span* neighbourhood)]
         [query (make-leaf-span-set-sql-query lss)])
    (query-list database query)))

;; Same as find-nearby-sessions, but only search the sessions from the
;; LAST_IMPORT table, that is the sessions which were last imported.
(define (find-nearby-sessions/last-import database geoid
                                          #:search-geoid-level (sgl default-search-geoid-level))
  (let* ([wpt^ (enclosing-geoid geoid sgl)]
         [neighbourhood (adjacent-geoids wpt^)]
         [lss (leaf-span* neighbourhood)]
         [query (make-leaf-span-set-sql-query/last-import lss)])
    (query-list database query)))

;; Prune indices generated by the first pass of
;; `candidate-intervals-for-segment`, so there is only one match for every
;; pass next to the target GEOID.
;;
;; As a segment starts to cross a GEOID, it is likely that many points will
;; match, as several points will be closer than the *segment-search-limit*.
;; We use PRUNE-INDICES to select the closest point from these groups.
(define (prune-indices indices cutoff-distance)
  (if (null? indices)
      '()
      (let loop ([result (list (car indices))]
                 [indices (cdr indices)])
        (if (null? indices)
            result
            (match-let ([(list r-index r-dst r-closeness) (car result)]
                        [(list i-index i-dst i-closeness) (car indices)])
              (if (< (abs (- r-dst i-dst)) cutoff-distance)
                  (if (< i-closeness r-closeness)
                      (loop (cons (car indices) (cdr result)) (cdr indices))
                      (loop result (cdr indices)))
                  (loop (cons (car indices) result) (cdr indices))))))))

;; Distance in meters between two geoids for them to be considered to match
;; the start or end of a segment
(define *segment-search-limit* 30)      ; meters

;; Return the list of intervals in the data frame DF which closely match the
;; segment defined by WAYPOINTS (a vector of GEOIDS) and SEGMENT-LENGTH the
;; length of the segment (note that the SEGMENT-LENGTH could be derived from
;; WAYPOINTS, but since this function will be called several times with the
;; same segment, we avoid calculating it).
;;
;; Returns a list of items, each item is a list of the data frame for the
;; session, start-index, end-index, alignment cost.
(define (find-segment-matches df waypoints segment-length)

  ;; TODO: use raise-argument-error here
  (unless (> (vector-length waypoints) 0)
    (error "find-intervals-for-segment: waypoints is empty"))
  (unless (df-contains? df "dst")
    (error "find-intervals-for-segment: data-frame must contain dst series"))
  (unless (df-contains? df "geoid")
    (unless (df-contains? df "lat" "lon")
      (error "find-intervals-for-segment: data-fame must contain lat and lon series"))
    (df-add-derived!
     df
     "geoid"
     '("lat" "lon")
     (lambda (v)
       (match-define (list lat lon) v)
       (and lat lon (lat-lng->geoid lat lon)))))

  (define start-geoid (vector-ref waypoints 0))
  (define end-geoid (vector-ref waypoints (sub1 (vector-length waypoints))))
  (define start (distance-from-geoid start-geoid))
  (define end (distance-from-geoid end-geoid))

  ;; Find in the data frame which have points close to START-GEOID and
  ;; END-GEOID.  Returns two lists, one for the start and one for the end
  ;; GEOID.  Each item in the list is a list of 3 elements: the index in the
  ;; data frame, the distance from the start of the session (the value of the
  ;; "dst" series at the index) and the distance from the start or end geoid.
  ;;
  ;; As a segment starts to cross a GEOID, it is likely that many points will
  ;; match this loop as several points will be closer than the
  ;; *segment-search-limit*.  We use PRUNE-INDICES to select the closest point
  ;; from these groups.
  (define-values (start-indices end-indices)
    (for/fold ([start-indices '()]
               [end-indices '()])
              ([geoid (in-data-frame df "geoid")]
               [index (in-naturals)])
      (if geoid
          (let* ([distance-to-start (start geoid)]
                 [distance-to-end (end geoid)]
                 [close-to-start? (< distance-to-start *segment-search-limit*)]
                 [close-to-end? (< distance-to-end *segment-search-limit*)])

            (cond ((and close-to-start? close-to-end?)
                   (define dst (df-ref df index "dst"))
                   (values (cons (list index dst distance-to-start) start-indices)
                           (cons (list index dst distance-to-end) end-indices)))
                  (close-to-start?
                   (define dst (df-ref df index "dst"))
                   (values (cons (list index dst distance-to-start) start-indices)
                           end-indices))
                  (close-to-end?
                   (define dst (df-ref df index "dst"))
                   (values start-indices
                           (cons (list index dst distance-to-end) end-indices)))
                  (#t
                   (values start-indices end-indices))))
          (values start-indices end-indices))))

  (define cutoff-distance (* 2 *segment-search-limit*))
  (define pruned-start-indices (prune-indices start-indices cutoff-distance))
  (define pruned-end-indices (prune-indices end-indices cutoff-distance))

  ;; For each combination of start and end points, check that the distance
  ;; along the segment between them is less than LENGTH-LIMIT.  These are
  ;; candidate segments to feed into the DTW algorithm
  (define length-limit (* 10 *segment-search-limit*))

  (define candidate-matches
    (for*/list ([start (in-list pruned-start-indices)]
                [end (in-list pruned-end-indices)]
                #:when (let ([ilen (- (list-ref end 1) (list-ref start 1))])
                         (and (> ilen 0)  ; end is after start
                              (< (abs (- ilen segment-length)) length-limit))))
      (define start-index (list-ref start 0))
      (define end-index (list-ref end 0))
      (define interval-length (- (list-ref end 1) (list-ref start 1)))
      (define interval (df-select df "geoid" #:filter valid-only
                                  #:start start-index
                                  #:stop end-index))

      (define dtw-cost (waypoint-alignment-cost interval waypoints))
      (define good? (good-segment-match?
                     dtw-cost
                     interval-length (- end-index start-index)
                     segment-length (vector-length waypoints)
                     15.0))
      (list start-index
            end-index
            (- end-index start-index)
            good?
            dtw-cost)))

  ;; sort candidate matches based on their length (in terms of number of
  ;; points).
  (define sorted (sort candidate-matches
                       (lambda (a b) (< (list-ref a 2) (list-ref b 2)))))

  ;; Filter out and keep only the good matches: a match is good if its "good?"
  ;; flag is #t and we haven't found a shorter match starting or ending at the
  ;; same index (since we sorted them, shorter matches are in RESULT already).
  ;; See also AB#61, this prevents matching the segment twice in the same
  ;; location under certain conditions.
  (let loop ([result '()]
             [remaining sorted])
    (if (null? remaining)
        result
        (match-let ([(list start-index end-index _l good? dtw-cost) (car remaining)])
          (loop
           (if (and good?
                    (not (findf (lambda (e)
                                  (or (= (list-ref e 1) start-index)
                                      (= (list-ref e 2) end-index)))
                                result)))
               (cons (list df start-index end-index dtw-cost) result)
               result)
           (cdr remaining))))))

;; Return #t if the DTW cost is acceptable and the selected interval is a
;; match for the segment.
;;
;; The DTW algorithm we use to check if a segment matches will return a value
;; which is larger if the segments align poorly, but it is also larger if the
;; segment is longer.  This means that we cannot simply compare the DTW cost
;; against a fixed benchmark value to determine if a match is good or not.
;; Instead we attempt to normalize the cost and compare it against a
;; benchmark.
;;
;; This is experimental, but seems to work OK for the segments I tested so
;; far...
(define (good-segment-match?
         cost
         interval-length waypoint-count
         segment-length segment-waypoint-count
         average-allowable-distance)
  ;; The normalized cost represents the average distance, in meters, between
  ;; each set of paired waypoints.
  (define normalized-cost (/ cost (max waypoint-count segment-waypoint-count)))
  ;; the benchmark is the average distance between the waypoints in either the
  ;; segment or in the matched interval -- i.e. how close are the samples
  ;; against the path...
  (define benchmark
    (max (/ segment-length segment-waypoint-count)
         (/ interval-length waypoint-count)))
  #;(printf "+++ good-match cost ~a; ilen ~a; wpcount ~a; slen ~a; wpcount ~a; ncost ~a, benchmark ~a~%"
          cost
          interval-length waypoint-count
          segment-length segment-waypoint-count
          normalized-cost benchmark)
  ;; we have a match if the segment is about "average-allowable-distance" away
  ;; from the segment.  We add half the benchmark to account for two paths
  ;; which are exactly the same, except the points are offset along the path.
  (< normalized-cost (+ average-allowable-distance (* 0.5 benchmark))))


;;........................................................... segment-df ....

;; A data frame cache, to avoid reading data frames again.  This is a two
;; stage cache, allowing us to expire old entries.  See 'gps-segment-df' on
;; how this cache is managed.
(define df-cache (make-hash))
(define df-cache2 (make-hash))

;; Number of data frames to keep in df-cache.  NOTE: total data frame count is
;; up to (hash-count df-cache) + (hash-count df-cache2), so in total number of
;; cached data frames can be up to (* 2 df-cache-limit)
(define df-cache-limit 50)

;; Return the segment data frame identified by the SID (a GPS_SEGMENT.id) from
;; the database DB.  A cache of segments is kept, so calling this function
;; should be fast once a segment is already in memory -- also this function
;; will keep track of which segments have been updated and will automatically
;; re-fetch them.  The simplest option is for other code to not keep segments
;; around and instead use this function.
(define (gps-segment-df db sid)
  (process-events)
  (cond ((hash-ref df-cache sid #f))
        ((hash-ref df-cache2 sid #f)
         => (lambda (df)
              ;; Promote it to first cache
              (hash-set! df-cache sid df)
              df))
        (#t
         (let ((df (fetch-gps-segment db sid)))
           (hash-set! df-cache sid df)
           (when (> (hash-count df-cache) df-cache-limit)
             ;; Cache limit reached, demote df-cache to df-cache2 (loosing old
             ;; data) and create a fresh df-cache
             (set! df-cache2 df-cache)
             (set! df-cache (make-hash)))
           df))))

;; Remove segment id SID from the cache.  If SID is #f, the entire cache is
;; cleared.
(define (clear-gps-segment-cache (sid #f))
  (if sid
      (begin
        (hash-remove! df-cache sid)
        (hash-remove! df-cache2 sid))
      (begin
        (hash-clear! df-cache)
        (hash-clear! df-cache2))))

(define log-event-source (make-log-event-source))

;; Process gps segment related events from LOG-EVENT-SOURCE and update the
;; cache accordingly.
(define (process-events)
  (let loop ((item (async-channel-try-get log-event-source)))
    (when item
      (match-define (list tag data) item)
      (case tag
        ((measurement-system-changed
          database-opened)
         (clear-gps-segment-cache))
        ((gps-segment-updated
          gps-segment-updated-data
          gps-segment-deleted)
         (clear-gps-segment-cache data)))
      (loop (async-channel-try-get log-event-source)))))


;;............................................................. provides ....

(provide/contract
 (gps-segment-from-gpx (-> (or/c path-string? input-port?) data-frame?))
 (gps-segment-by-reversing (-> data-frame? data-frame?))

 (put-new-gps-segment (-> connection? data-frame? exact-nonnegative-integer?))
 (fetch-gps-segment (-> connection? exact-nonnegative-integer? data-frame?))
 (delete-gps-segment (-> connection? exact-nonnegative-integer? any/c))

 (put-new-segment-match (-> connection? exact-nonnegative-integer? data-frame?
                            exact-nonnegative-integer? exact-nonnegative-integer?
                            positive? exact-nonnegative-integer?))
 (delete-segment-match (-> connection? exact-nonnegative-integer? any/c))

 (find-nearby-sessions (->* (connection? exact-nonnegative-integer?)
                            (#:search-geoid-level exact-nonnegative-integer?)
                            (listof exact-nonnegative-integer?)))
 (find-nearby-sessions/last-import (->* (connection? exact-nonnegative-integer?)
                                        (#:search-geoid-level exact-nonnegative-integer?)
                                        (listof exact-nonnegative-integer?)))
 (find-segment-matches (-> data-frame? (vectorof exact-nonnegative-integer?) positive?
                           (listof (list/c data-frame? integer? integer? positive?))))

 (gps-segment-df (-> connection? exact-nonnegative-integer? data-frame?))

 (good-segment-match? (-> rational?
                          positive? exact-nonnegative-integer?
                          positive? exact-nonnegative-integer?
                          positive?
                          boolean?))

 (fixup-segment-data (-> data-frame? any/c)))
