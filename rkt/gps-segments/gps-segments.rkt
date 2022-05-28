#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; gps-segment.rkt -- GPS Segments implementation
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         geoid/waypoint-alignment
         geoid/geodesy
         math/statistics
         racket/contract
         racket/match
         racket/port
         racket/async-channel
         "../utilities.rkt"
         "../dbutil.rkt"
         "../database.rkt"              ; for db-insert-section-summary
         "../models/grade-series.rkt"
         "../models/fiets-score.rkt"
         "../intervals.rkt")            ; for total-ascent-descent, make-interval-summary

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
      (let ([bottom (df-ref df 0 "alt")]
            [top (df-ref df (sub1 (df-row-count df)) "alt")])
        (and bottom top (- top bottom))))

    ;; NOTE: this grade calculation only works correctly for segments which
    ;; are climbs, but I suspect most users would expect average grade to be
    ;; calculated this way.
    (define segment-grade
      (and segment-height segment-length
           (* (/ segment-height segment-length) 100.0)))

    (df-put-property! df 'segment-height segment-height)
    (df-put-property! df 'segment-grade segment-grade))

  (let ([score (fiets-score df)])
    (when score
      (df-put-property! df 'fiets-score score))))

;; Create a segment from a GPX file -- this uses `df-read/gpx` than
;; `fixup-segment-data` to add any series and properties which are missing.
(define (gps-segment-from-gpx input)
  (define df (df-read/gpx input))
  (fixup-segment-data df)
  df)

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
(define *search-geoid-level* 11)        ; Approx 4.4 x 4.4 meters

;; Return a list of session IDs whose path cross (or go nearby) GEOID.
;;
;; NOTE: the geoid package does not support "caps" yet, so we just get the 9x9
;; neighborhood of the geoid after upscaling it to *search-geoid-level*
(define (find-nearby-sessions database geoid)
  (let* ([wpt^ (enclosing-geoid geoid *search-geoid-level*)]
         [neighbourhood (adjacent-geoids wpt^)]
         [lss (leaf-span* neighbourhood)]
         [query (make-leaf-span-set-sql-query lss)])
    (query-list database query)))

;; Same as find-nearby-sessions, but only search the sessions from the
;; LAST_IMPORT table, that is the sessions which were last imported.
(define (find-nearby-sessions/last-import database geoid)
  (let* ([wpt^ (enclosing-geoid geoid *search-geoid-level*)]
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
(define *segment-search-limit* 15)      ; meters

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
    (define good? (good-match? dtw-cost interval-length (- end-index start-index)
                               segment-length (vector-length waypoints)))
    (list start-index
          end-index
          good?
          dtw-cost)))

  (for/list ([m (in-list candidate-matches)] #:when (list-ref m 2))
    (match-define (list start-index end-index good? dtw-cost) m)
    (list df start-index end-index dtw-cost)))

;; Return #t if the DTW cost is acceptable and the selected interval is a
;; match for the segment.
;;
;; The DTW algorithm we use to check if a segment matches will return a value
;; which is larger if the segment is longer, it is also larger if the segments
;; align poorly.  This means that we cannot simply compare the DTW cost
;; against a benchmark value to determine if a match is good or not.  Instead
;; we attempt to normalize the cost and compare it against a benchmark.
;;
;; This is experimental, but seems to work OK for the segments I tested so
;; far...
(define (good-match? cost interval-length waypoint-count segment-length segment-waypoint-count)
  ;; The normalized cost represents the average distance between each set of
  ;; paired waypoints.
  (define normalized-cost (/ cost (max waypoint-count segment-waypoint-count)))
  ;; the benchmark is the average distance between the waypoints in either the
  ;; segment or in the matched interval -- i.e. how close are the samples
  ;; against the path...
  (define benchmark
    (max (/ segment-length segment-waypoint-count)
         (/ interval-length waypoint-count)))
  #;(printf "+++ good-match cost ~a; ilen ~a; wpcount ~a; slen ~a; wpcount ~a; ncost ~a, benchmark ~a~%"
          cost interval-length waypoint-count segment-length segment-waypoint-count normalized-cost benchmark)
  (< normalized-cost (* 2.0 benchmark)))


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

 (find-nearby-sessions (-> connection? exact-nonnegative-integer?
                           (listof exact-nonnegative-integer?)))
 (find-nearby-sessions/last-import (-> connection? exact-nonnegative-integer?
                                       (listof exact-nonnegative-integer?)))
 (find-segment-matches (-> data-frame? (vectorof exact-nonnegative-integer?) positive?
                           (listof (list/c data-frame? integer? integer? positive?))))

 (gps-segment-df (-> connection? exact-nonnegative-integer? data-frame?))

 (fixup-segment-data (-> data-frame? any/c)))
