#lang racket/base
;; elevation-correction.rkt -- elevation correction for trackpoints
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require data-frame/private/bsearch
         db/base
         racket/class
         racket/flonum
         racket/list
         racket/match
         racket/math
         racket/contract
         "../utilities.rkt"                ; for dbglog
         "../widgets/main.rkt"             ; for progress-dialog%
         map-widget/utils)


;; Elevation correction does not produce good results when there are small
;; amounts of data and sessions are recorded with mostly flat terrain.  While
;; actual corrected elevation is stored in parallel to the actual elevation
;; recorded by the device and both can be plotted, the summary values (total
;; ascent and descent) are computed from the corrected elevation and, if this
;; is incorrect, these values can be wildly inaccurate.
;;
;; Users can manually delete the corrected elevation and re-calculate it for
;; individual activities, but we also allow disabling this feature on import
;; for users who find their data to be completely incorrect
;;
;; See also discussion thread on #51

(define fix-elevation-on-import-tag 'activity-log:fix-elevation-on-import)
(define fix-elevation-on-import-val (get-pref fix-elevation-on-import-tag (lambda () #t)))
(define (fix-elevation-on-import) fix-elevation-on-import-val)
(define (set-fix-elevation-on-import new-val)
  ;; Write the value back to the store
  (put-pref fix-elevation-on-import-tag new-val)
  (set! fix-elevation-on-import-val new-val)
  (if new-val
      (dbglog "fix elevation on import enabled")
      (dbglog "fix elevation on import disabled")))

(provide/contract
 [fix-elevation-on-import (-> boolean?)]
 [set-fix-elevation-on-import (-> boolean? any/c)])

;; On "tile-codes"
;;
;; This code works by averaging nearby codes, the application needs a way to
;; say "give me the GPS coordinates around this location".  The naive solution
;; of calculating the distance to every other GPS point in the database would
;; be too slow, instead, the application groups nearby points in "tile codes".
;; Essentially, the entire world map is split into squares, such that there
;; are 2^18 = 262144 x 262144 squares, and GPS points are assigned to the
;; square that they are in.  The coordinates of each square (the row and
;; column) are combined into one single number (see `lat-lon->tile-code`
;; function in this file).  The tile code for each track point is stored in
;; the database in the A_TRACKPOINT.tile_code column and when the application
;; needs all GPS points around a location, it calculates the locations tile
;; code and retrieves all the GPS points with this tile code.
;;
;; NOTE: there are other solutions for assigning identifiers to GPS locations,
;; for example the S2 geometry library, but I already had tile code
;; calculation done as part of the map widget, so I just used these.

;; NOTE: changing this value requires invalidating all the tile codes in the
;; database and re-building them
(define tile-level 18)
(define tile-mult (expt 2 tile-level))


;;.............................................. clusters and tile codes ....

;; A cluster of points in a tile code.  START and END represent the start and
;; end of the cluster (all points are consecutive points between these
;; timestamps).  Data is a flvector containing in groups of 3 flonums the
;; latitude, longitude and altitude of each point.  Latitude and longitude are
;; in radians.
;;
;; Clusters represent segments of consecutive track points in a single
;; activity, but an activity may traverse the same tile multiple times
;; generating multiple clusters (such as when the activity is an out-and-back
;; or a loop) -- essentially we want to only average points which are from
;; different traversals over the same area as opposed to averaging points
;; which are just before or just after out point (which would result in a
;; simple smoothing of the altitude data).
(struct cluster (start end data) #:transparent)

;; Create a cluster from a list of POINTS, each point being a vector of 4
;; elements: the latitude, longitude, altitude and time stamp.  POINTS are
;; from the same cluster, ordered from LARGEST to SMALLEST timestamp (they are
;; in reverse order).  Note that the latitude and longitude is in radians.
(define (make-cluster points)
  (define count (length points))
  (define start #f)
  (define end #f)
  (define data (make-flvector (* 3 count)))
  (for ([(point index) (in-indexed (in-list points))])
    (match-define (vector lat lon alt timestamp) point)
    (when (= index 0)                 ; note that points are in reverse order!
      (set! end timestamp))
    (when (= index (sub1 count))
      (set! start timestamp))
    (define base (* 3 (- count index 1)))
    (flvector-set! data base lat)
    (flvector-set! data (+ base 1) lon)
    (flvector-set! data (+ base 2) alt))
  (unless (>= end start)
    (error (format "make-cluster: points in wrong order (start = ~a, end = ~a)~%"
                   start end)))
  (cluster start end data))

;; SQL query to fetch all the track points for a specified tile code.  Track
;; points are ordered by their timestamp (lowest first), so it is easier to
;; split them in clusters, one cluster per segment.
(define fetch-track-points-for-tilecode-query
  (virtual-statement
   (lambda (db)
     "\
select position_lat, position_long, altitude, timestamp
from A_TRACKPOINT where tile_code = ?
  and position_lat is not null
  and position_long is not null
  and altitude is not null
order by timestamp")))

;; Fetch a track points from the database for the specified TILE-CODE,
;; creating clusters which are separated by GAP seconds between them.  That
;; is, points are accumulated in a cluster until the time gap between adjacent
;; points is larger than GAP.
;;
;; Returns a list of clusters (see the CLUSTER struct and MAKE-CLUSTER)
(define (fetch-track-points-for-tilecode db tile-code #:cluster-gap (gap 60))

  (define current-timestamp 0)
  (define clusters '())
  (define current-cluster '())

  (for ([(lat lon alt timestamp)
         (in-query db fetch-track-points-for-tilecode-query tile-code #:fetch 500)])
    (define entry (vector (degrees->radians lat)
                          (degrees->radians lon)
                          (exact->inexact alt)
                          timestamp))
    (when (> (abs (- timestamp current-timestamp)) gap)
      (unless (null? current-cluster)
        (set! clusters (cons (make-cluster current-cluster) clusters)))
      (set! current-cluster '()))
    (set! current-timestamp timestamp)
    (set! current-cluster (cons entry current-cluster)))
  (unless (null? current-cluster)       ; add the last cluster
    (set! clusters (cons (make-cluster current-cluster) clusters)))
  clusters)

;; Return a function that produces the points for a tile code.  Points are
;; fetched from the database as needed and kept in a cache.
(define (make-tile-code-fetcher db)
  (define cache (make-hash))

  (define (populate-cache tile-code)
    (define clusters (fetch-track-points-for-tilecode db tile-code))
    (hash-set! cache tile-code clusters)
    clusters)

  (lambda (tile-code)
    (let ([data (hash-ref cache tile-code (lambda () #f))])
      (if data data (populate-cache tile-code)))))

;; Return the closest point in the CLUSTER for LAT, LON.  LAT and LON are in
;; radians, CLUSTER is a struct, see MAKE-CLUSTER.
;;
;; Returns 4 values: the lat, lon, alt and distance to the closest point, or
;; #f if it cannot be found (i.e. the cluster is empty).  LAT and LON are in
;; radians.
(define (closest-point cluster lat lon)
  (define data (cluster-data cluster))
  (define limit (flvector-length data))
  (let loop ([index 0]
             [candidate #f]
             [min-distance +inf.0])
    (cond ((< min-distance 1e-1)
           ;; Bail out early if we found the actual point on this path --
           ;; there might be a closer one, but 0.1 meters is worth the risk...
           (values
            (flvector-ref data candidate)
            (flvector-ref data (+ 1 candidate))
            (flvector-ref data (+ 2 candidate))
            min-distance))
          ((< index limit)
           (let* ([clat (flvector-ref data index)]
                  [clon (flvector-ref data (+ 1 index))]
                  [distance (map-distance/radians lat lon clat clon)])
             (if (< distance min-distance)
                 (loop (+ index 3) index distance)
                 (loop (+ index 3) candidate min-distance))))
          (candidate
           (values
            (flvector-ref data candidate)
            (flvector-ref data (+ 1 candidate))
            (flvector-ref data (+ 2 candidate))
            min-distance))
          (#t
           (values #f #f #f #f)))))

;; Return a list of tile-codes that contain points of interest for the
;; position at LAT, LON.  The tile-code which contains LAT, LON is always
;; returned, but if the position is close to one of the tile edges, the
;; adjacent tile codes are also returned.
;;
;; WARNING: LAT, LON are in degrees!
(define (candidate-tile-codes lat lon)
  (define mp (lat-lon->npoint (exact->inexact lat) (exact->inexact lon)))

  (define (xy->tile-code x y)
    (bitwise-ior (arithmetic-shift x tile-level) y))

  (define upper-limit 0.8)
  (define lower-limit 0.2)

  (let* ((x0 (* tile-mult (npoint-x mp)))
         (y0 (* tile-mult (npoint-y mp)))
         (x (exact-truncate x0))
         (y (exact-truncate y0))
         ;; xrem, yrem are the position of MP within the tile (0..1) range.
         (xrem (- x0 x))
         (yrem (- y0 y))
         ;; the tile of the point is always a candidate
         (result (list (xy->tile-code x y))))
    ;; Edges
    (when (> xrem upper-limit)
      (set! result (cons (xy->tile-code (+ x 1) y) result)))
    (when (< xrem lower-limit)
      (set! result (cons (xy->tile-code (- x 1) y) result)))
    (when (> yrem upper-limit)
      (set! result (cons (xy->tile-code x (+ y 1)) result)))
    (when (< yrem lower-limit)
      (set! result (cons (xy->tile-code x (- y 1)) result)))
    ;; Corners
    (when (and (> xrem upper-limit) (> yrem upper-limit))
      (set! result (cons (xy->tile-code (+ x 1) (+ y 1)) result)))
    (when (and (> xrem upper-limit) (< yrem lower-limit))
      (set! result (cons (xy->tile-code (+ x 1) (- y 1)) result)))
    (when (and (< xrem lower-limit) (> yrem upper-limit))
      (set! result (cons (xy->tile-code (- x 1) (+ y 1)) result)))
    (when (and (< xrem lower-limit) (< yrem lower-limit))
      (set! result (cons (xy->tile-code (- x 1) (- y 1)) result)))

    result))


;;..................................................... average-altitude ....

;; Return the average altitude at LAT, LONG based on other traversals of the
;; nearby of the same area.
;;
;; FETCH-CLUSTERS is a function which returns a list of clusters (see the
;; CLUSTER structure) for a specific tile code.
;;
;; WARNING: LAT, LON are in degrees
(define (calculate-average-altitude lat lon fetch-clusters)
  ;; Distance in meters from LAT/LONG where the weight will be 0.5.  Points
  ;; closer than this will have a weight growing towards 1, 1 being the weight
  ;; of the point exactly at LAT/LONG, points further away than this value
  ;; will have their weight further decreasing towards 0.
  (define hw-distance (->fl 5))
  ;; LAT LON converted to radians
  (define-values (r-lat r-lon) (values (degrees->radians lat) (degrees->radians lon)))

  ;; We iterate over the clusters in each candidate tile code. select the
  ;; closest point from each cluster and compute a weighted average of these
  ;; points.  Each cluster represents a different "pass" over or near this
  ;; point and as such we average readings at different times of the height of
  ;; this location.
  ;;
  ;; The average is weighted because we are unlikely to find a point at this
  ;; exact location and we don't want to straight average points at other
  ;; nearby locations.
  (define-values (sum div)
    (for/fold ([sum 0.0] [div 0.0])
              ([tile-code (in-list (candidate-tile-codes lat lon))])
      (for/fold ([sum sum] [div div])
                ([cluster (in-list (fetch-clusters tile-code))])
        (define-values (clat clon calt distance) (closest-point cluster r-lat r-lon))
        (define weight (flmax 0.0 (fl/ hw-distance (fl+ distance hw-distance))))
        (values (fl+ (fl* weight calt) sum) (fl+ div weight)))))

  (if (> div 0) (fl/ sum div) #f))

;; A single track point produced by 'calculate-altitude' and used by
;; 'smooth-altitude'.
(struct tpoint (id lat lon dst (calt #:mutable)))

;; Calculate the corrected altitude for all points in TRACKPOINTS based of
;; ALTITUDE-DATA.  Calls `CALCULATE-AVERAGE-ALTITUDE for each point in
;; TRACKPOINTS and returns vector of TPOINT structures.
;;
;; See CALCULATE-AVERAGE-ALTITUDE for the meaning of FETCH-CLUSTERS.
(define (average-altitude trackpoints fetch-clusters
                          [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage
          "Calculating GPS track altitude" (length trackpoints)))
  (if (null? trackpoints)
      (vector)
      (let* ([first-point (car trackpoints)]
             (prev-lat (vector-ref first-point 1))
             (prev-lon (vector-ref first-point 2))
             (distance 0))
        (for/vector #:length (length trackpoints)
            ([(point index) (in-indexed (in-list trackpoints))])
          (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
            (send progress-monitor set-progress (add1 index)))
          (match-define (vector id lat lon) point)
          (set! distance (+ distance (map-distance/degrees prev-lat prev-lon lat lon)))
          (set! prev-lat lat)
          (set! prev-lon lon)
          (tpoint id lat lon distance (calculate-average-altitude lat lon fetch-clusters))))))


;;...................................................... smooth-altitude ....

;; Smooth the altitude points produced by 'average-altitude' using an idea
;; from http://regex.info/blog/2015-05-09/2568, the resulting altitude is only
;; marginally better from a visual point of view (and somewhat worse for small
;; elevation changes), but total ascent and descent calculated off this
;; smoothed data are significantly more accurate.
;;
;; TRACKPOINTS is a vector of TPOINT structures and each point will have their
;; CALT slot updated in place.
(define (smooth-altitude trackpoints)

  ;; Minimum distance between which we can smooth the altitude.  For distances
  ;; less than this, we interpolate between the start and end point.
  (define minimum-distance 50.0)

  ;; Minimum altidute difference in a range for which we split the range.  If
  ;; the altidute difference in a range is less than this, we consider the
  ;; range monotonic.
  (define minimum-altitude 3.0)

  ;; Compute the distance between START and END (indexes in the TRACKPOINTS
  ;; vector)
  (define (delta-dist start end)
    (- (tpoint-dst (vector-ref trackpoints end))
       (tpoint-dst (vector-ref trackpoints start))))
  ;; Compute the altitude change between START and END (indexes in the
  ;; TRACKPOINTS vector).  This will be negative if the slope is downhill.
  (define (delta-alt start end)
    (- (tpoint-calt (vector-ref trackpoints end))
       (tpoint-calt (vector-ref trackpoints start))))
  ;; Fill in a monotonic altitude increase/decrease between START and END.
  (define (monotonic-slope start end)
    (let* ((delta-dst (delta-dist start end))
           (delta-alt (delta-alt start end))
           (start-dst (tpoint-dst (vector-ref trackpoints start)))
           (start-alt (tpoint-calt (vector-ref trackpoints start))))
      (for ([idx (in-range start (add1 end))])
        (let* ((dst (tpoint-dst (vector-ref trackpoints idx)))
               (nalt
                (if (> delta-dst 0)     ; the device can be stationary!
                    (+ start-alt (* delta-alt (/ (- dst start-dst) delta-dst)))
                    start-alt)))
          (set-tpoint-calt! (vector-ref trackpoints idx) nalt)))))
  ;; Find the minimum and maximum altitude between START and END and return 4
  ;; values: min-alt, min-alt position, max-alt, max-alt position.
  (define (find-min-max-alt start end)
    (let ((min-alt (tpoint-calt (vector-ref trackpoints start)))
          (min-alt-idx start)
          (max-alt (tpoint-calt (vector-ref trackpoints start)))
          (max-alt-idx start))
      (for ([idx (in-range start (add1 end))])
        (define a (tpoint-calt (vector-ref trackpoints idx)))
        (when (< a min-alt)
          (set! min-alt a)
          (set! min-alt-idx idx))
        (when (> a max-alt)
          (set! max-alt a)
          (set! max-alt-idx idx)))
      (values min-alt min-alt-idx max-alt max-alt-idx)))
  ;; Return the position of the middle point between START and END.  This is
  ;; done based on distances, not indices, and depending on sampling rates and
  ;; stop points, it might be "off" from the middle. Still, the "best" middle
  ;; point is returned.
  (define (find-middle start end)
    (let* ((sdist (tpoint-dst (vector-ref trackpoints start)))
           (half (/ (delta-dist start end) 2))
           (mid (bsearch trackpoints (+ sdist half)
                         #:start start #:stop end #:key tpoint-dst)))
      mid))
  (define (order-points p1 p2 p3 p4)
    (let ((points (list p1 p2 p3 p4)))
      (remove-duplicates (sort points <))))

  ;; Split the interval between START and END between the highest and lowest
  ;; point, until the interval is too small or the altitude difference is less
  ;; than MINIMUM-ALTITUDE, in which case the altitude is simply interpolated
  ;; between start and end.
  (define (iterate start end)
    (let ((dist (delta-dist start end)))
      (cond
        ((or (<= (- end start) 1) (< dist minimum-distance))
         (monotonic-slope start end))
        (#t
         (let-values (((min-alt min-alt-idx max-alt max-alt-idx)
                       (find-min-max-alt start end)))
           (if (< (- max-alt min-alt) minimum-altitude)
               (monotonic-slope start end)
               (let ((ranges (order-points start min-alt-idx max-alt-idx end)))
                 (if (= (length ranges) 2)
                     ;; range is monotonic, split it in two and recurse
                     (let ((mid (find-middle start end)))
                       (iterate start (sub1 mid))
                       (iterate mid end))
                     ;; Else, iterate over the defined ranges
                     (for ([s ranges] [e (cdr ranges)])
                       (iterate s e))))))))))

  (when (and (> (vector-length trackpoints) 0)
             ;; At least one valid altitude in the data set
             (for/first ((tp (in-vector trackpoints)) #:when (tpoint-calt tp)) #t))

    ;; Fixup #f's in the calt values, this works OK for one-off missing
    ;; values, if whole ranges are missing, this will not produce nice
    ;; results.
    (for ([(tp idx) (in-indexed (in-vector trackpoints))] #:unless (tpoint-calt tp))
      (if (= idx 0)
          ;; Scan forward for the first good altitude value
          (let ((a (for/first ([tp (in-vector trackpoints)] #:when (tpoint-calt tp))
                     (tpoint-calt tp))))
            (set-tpoint-calt! tp a))
          ;; Use the previous value
          (set-tpoint-calt! tp (tpoint-calt (vector-ref trackpoints (sub1 idx))))))

    (iterate 0 (sub1 (vector-length trackpoints)))))


;;...................................... updating the corrected altitude ....

;; This section deals with updating the corrected altitude in the database

;; SQL statement to update the corrected altitude for a trackpoint
(define update-trackpoint-stmt
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set corrected_altitude = ? where id = ?")))

;; Update the A_TRACKPOINT rows in the database with altitude data from
;; ALTITUDE-DATA which is a vector of TPOINT structures as produced by
;; AVERAGE-ALTITUDE.
(define (update-trackpoints db altitude-data
                            [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage
          "Updating altitude for GPS track" (vector-length altitude-data)))
  (call-with-transaction
   db
   (lambda ()
     (for ([(point index) (in-indexed (in-vector altitude-data))])
       (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
         (send progress-monitor set-progress (add1 index)))
       (let ((id (tpoint-id point))
             (calt (tpoint-calt point)))
         (query-exec db update-trackpoint-stmt (or calt sql-null) id))))))

(define q-get-altitude1
  (virtual-statement
   (lambda (dbsys) "
select corrected_altitude
  from A_TRACKPOINT
 where corrected_altitude is not null
   and length_id = ? order by timestamp")))

(define q-update-ss1
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_LENGTH L where L.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session length
;; based on the already corrected trackpoints in this length.
(define (update-summary-altitude-for-length db length-id)
  (let ((altitude (query-list db q-get-altitude1 length-id)))
    (unless (null? altitude)
      (let ((ascent 0)
            (descent 0))
        (for ((first (in-list altitude))
              (second (in-list (cdr altitude))))
          (let ((diff (- second first)))
            (if (> diff 0)
                (set! ascent (+ ascent diff))
                (set! descent (+ descent (- diff))))))
        (query-exec db q-update-ss1
                    (exact->inexact ascent) (exact->inexact descent) length-id)))))

(define q-get-altitude2
  (virtual-statement
   (lambda (dbsys) "
select sum(SS.total_corrected_ascent),
       sum(SS.total_corrected_descent)
  from SECTION_SUMMARY SS,
       A_LENGTH LL
  where LL.lap_id = ? and LL.summary_id = SS.id")))

(define q-update-ss2
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_LAP L where L.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session lap
;; based on the already corrected trackpoints in this lap (the summary data
;; for the laps lengths will also be corrected).
(define (update-summary-altitude-for-lap db lap-id)
  (let ((lengths (query-list db "select id from A_LENGTH where lap_id = ?" lap-id)))
    (for ((length (in-list lengths)))
      (update-summary-altitude-for-length db length))
    (let ((row (query-row db q-get-altitude2 lap-id)))
      (query-exec db q-update-ss2 (vector-ref row 0) (vector-ref row 1) lap-id))))

(define q-get-altitude3
  (virtual-statement
   (lambda (dbsys) "
select sum(SS.total_corrected_ascent),
       sum(SS.total_corrected_descent)
  from SECTION_SUMMARY SS,
       A_LAP L
  where L.session_id = ? and L.summary_id = SS.id")))

(define q-update-ss3
  (virtual-statement
   (lambda (dbsys) "
update SECTION_SUMMARY
   set total_corrected_ascent = ?,
       total_corrected_descent = ?
where id = (select summary_id from A_SESSION S where S.id = ?)")))

;; Update the summary altitude data (ascent and descent) for a session based
;; on the already corrected trackpoints in this session (the summary data for
;; the laps and lengths will also be corrected).
(define (update-summary-altitude-for-session db session-id)
  (let ((laps (query-list db "select id from A_LAP where session_id = ?" session-id)))
    (for ((lap (in-list laps)))
      (update-summary-altitude-for-lap db lap))
    (let ((row (query-row db q-get-altitude3 session-id)))
      (query-exec db q-update-ss3 (vector-ref row 0) (vector-ref row 1) session-id))))


;;......................................... fixup elevation entry points ....

;; SQL query to return all track points for a session
(define session-trackpoints-query
  (virtual-statement
   (lambda (dbsys) "
select T.id, T.position_lat, T.position_long
from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where P.session_id = ?
  and L.lap_id = P.id
  and T.length_id = L.id
  and position_lat is not null and position_long is not null
order by T.timestamp")))

;; Return latitude/longitude data for all track points in a session, along
;; with the trackpoint id. Returns a list of (vector trackpoint-id latitude
;; longitude)
(define (session-trackpoints db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session" 0))
  (query-rows db session-trackpoints-query session-id))

(define (fixup-elevation-for-session-internal db session-id altitude-data [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session..." 0))
  (define trackpoints (session-trackpoints db session-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for session..." 0))
  (unless (null? trackpoints)           ; maybe the session has no GPS data?
    (define tp-elevation (average-altitude trackpoints altitude-data progress-monitor))
    (smooth-altitude tp-elevation)
    (call-with-transaction
     db
     (lambda ()
       (update-trackpoints db tp-elevation progress-monitor)
       (when progress-monitor
         (send progress-monitor begin-stage (format "Updating summary altitude") 0))
       (update-summary-altitude-for-session db session-id)))))

(define (fixup-elevation-for-session db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define fetch-clusters (make-tile-code-fetcher db))
  (if (cons? session-id)
      (for/list ([sid session-id])
        (dbglog "fixup-elevation-for-session ~a started" sid)
        (fixup-elevation-for-session-internal db sid fetch-clusters progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" sid))
      (begin
        (dbglog "fixup-elevation-for-session ~a started" session-id)
        (fixup-elevation-for-session-internal db session-id fetch-clusters progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" session-id)))
  (when progress-monitor
    (send progress-monitor finished)))

(define (fixup-elevation-for-all-sessions db [progress-monitor #f])
  (dbglog "fixup-elevation-for-all-sessions started")
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define altidude-data (make-tile-code-fetcher db))
  (let ((sessions (query-list db "select id from A_SESSION")))
    (when progress-monitor
      (send progress-monitor begin-stage "Fixup elevation for all sessions"
            (length sessions)))
    (for (((sid index) (in-indexed (in-list sessions))))
      #:break (if progress-monitor
                  (not (send progress-monitor set-progress index))
                  #f)
      (fixup-elevation-for-session-internal db sid altidude-data #f)))
  (when progress-monitor
    (send progress-monitor finished))
  (dbglog "fixup-elevation-for-all-sessions completed"))

(define (interactive-fixup-elevation database session-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data"]
         [icon (sql-export-icon)]))

  (define progress-monitor
    (class object% (init-field progress-dialog) (super-new)

      (define num-items 100)

      (define/public (begin-stage msg max-items)
        (send progress-dialog set-message msg)
        (send progress-dialog set-progress 0)
        (set! num-items max-items))

      (define/public (set-progress n)
        (let ((pct (exact-round (* 100.0 (if (> num-items 0) (/ n num-items) 1.0)))))
          (send progress-dialog set-progress pct)))

      (define/public (finished)
        (send progress-dialog set-progress 100))))

  (define (task progress-dialog)
    (let ((m (new progress-monitor [progress-dialog progress-dialog])))
      (if session-id
          (fixup-elevation-for-session database session-id m)
          (fixup-elevation-for-all-sessions database m))))

  (send progress-dialog run parent-window task))


;.....................................................................  ....

;; SQL statement to update the tile code for a trackpoint
(define tile-update-stmt
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set tile_code = ? where id = ?")))

;; SQL statement returning the track points with no tile code (which need
;; their tile code updating)
(define tile-update-candidates
  (virtual-statement
   (lambda (dbsys)
     "\
select id, position_lat, position_long
from A_TRACKPOINT
where position_lat is not null
  and position_long is not null
  and tile_code is null")))

;; Return a tile-code corresponding to the map point MP (see
;; `lat-lon->map-point')
(define (map-point->tile-code mp)
  (let ((x (exact-truncate (* tile-mult (npoint-x mp))))
        (y (exact-truncate (* tile-mult (npoint-y mp)))))
    (bitwise-ior (arithmetic-shift x tile-level) y)))

(define (lat-lon->tile-code lat lon)
  (map-point->tile-code
   (lat-lon->npoint (exact->inexact lat) (exact->inexact lon))))

;; Update the tile code for any trackpoints in the database that don't have
;; one.
(define (update-tile-codes db)
  (define tp-canditates (query-rows db tile-update-candidates))
  (call-with-transaction
   db
   (lambda ()
     (for ([tp (in-list tp-canditates)])
       (match-define (vector id lat lon) tp)
       (let ((tile-code (lat-lon->tile-code lat lon)))
         (query-exec db tile-update-stmt tile-code id))))))

;; Remove the corrected elevation information for SESSION-ID.  Trackpoint and
;; section summary altitudes are removed.  This will cause all grade and
;; summary information displays to use the recorded elevation.
(define (clear-corrected-elevation-for-session database session-id)
  (call-with-transaction
   database
   (lambda ()
     (query-exec database "
update A_TRACKPOINT
   set corrected_altitude = null
 where length_id in (select L.id
                       from A_LENGTH L, A_LAP P
                      where L.lap_id = P.id
                        and P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select P.summary_id
                from A_LAP P
               where P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select L.summary_id
                from A_LAP P, A_LENGTH L
               where L.lap_id = P.id
                 and P.session_id = ?)" session-id)
     (query-exec database "
update SECTION_SUMMARY
   set total_corrected_ascent = null,
       total_corrected_descent = null
 where id in (select S.summary_id
                from A_SESSION S
               where S.id = ?)" session-id))))


;;............................................................. provides ....

(provide/contract
 (update-tile-codes (-> connection? any/c))
 (fixup-elevation-for-session (->* (connection?
                                    (or/c exact-nonnegative-integer?
                                          (listof exact-nonnegative-integer?)))
                                   (any/c) ; the progress monitor
                                   any/c))
 (fixup-elevation-for-all-sessions (->* (connection?)
                                        (any/c) ; the progress monitor
                                        any/c))
 (interactive-fixup-elevation (->* (connection?
                                    (or/c #f
                                          exact-nonnegative-integer?
                                          (listof exact-nonnegative-integer?)))
                                   (any/c) ; the parent window
                                   any/c))
 (clear-corrected-elevation-for-session (-> connection? exact-nonnegative-integer? any/c)))

(provide lat-lon->tile-code)

(provide make-tile-code-fetcher calculate-average-altitude candidate-tile-codes
         fetch-track-points-for-tilecode)
