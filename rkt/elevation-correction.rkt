#lang racket/base
;; elevation-correction.rkt -- elevation correction for trackpoints
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
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
;; multiple elevation readings are available for the same positions at
;; different times (as different A_TRACKPOINT records).  The code works by
;; averaging the reported elevation of all the trackpoints in the database
;; recorded around certain position.
;;
;; It will work best if a route is traversed several times and won't do
;; anything usefull for a route that is traversed only once.

(require db/base
         racket/class
         racket/flonum
         racket/math
         racket/match
         racket/list
         math/statistics
         "utilities.rkt"
         "widgets/map-widget/map-util.rkt"
         "widgets/main.rkt"
         "data-frame/bsearch.rkt")


(provide update-tile-codes)
(provide fixup-elevation-for-session)
(provide fixup-elevation-for-all-sessions)
(provide interactive-fixup-elevation)
(provide clear-corrected-elevation-for-session)

(define tile-level 18)
(define tile-mult (expt 2 tile-level))

(define tile-update-stmt
  ;; SQL statement to update the tile code for a trackpoint
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set tile_code = ? where id = ?")))

(define tile-update-candidates
  ;; SQL statement returning the track points with no tile code (which need
  ;; their tile code updating)
  (virtual-statement
   (lambda (dbsys)
     "\
select id, position_lat, position_long
from A_TRACKPOINT
where position_lat is not null
  and position_long is not null
  and tile_code is null")))

(define (map-point->tile-code mp)
  ;; Return a tile-code corresponding to the map point MP (see
  ;; `lat-lon->map-point')
  (let ((x (exact-truncate (* tile-mult (map-point-x mp))))
        (y (exact-truncate (* tile-mult (map-point-y mp)))))
    (bitwise-ior (arithmetic-shift x tile-level) y)))

(define (lat-lon->tile-code lat lon)
  (map-point->tile-code (lat-lon->map-point
                         (exact->inexact lat)
                         (exact->inexact lon))))
(provide lat-lon->tile-code)

(define (update-tile-codes db)
  ;; Update the tile code for any trackpoints in the database that don't have
  ;; one.
  (define tp-canditates (query-rows db tile-update-candidates))
  (call-with-transaction
   db
   (lambda ()
     (for ([tp (in-list tp-canditates)])
       (match-define (vector id lat lon) tp)
       (let ((tile-code (lat-lon->tile-code lat lon)))
         (query-exec db tile-update-stmt tile-code id))))))

(define (candidate-tile-codes mp)
  ;; Return a list of tile-codes that contain points of interest for the map
  ;; point MP.  The tile-code for the MP is always returned, but if the MP is
  ;; close to one of the tile edges, the adjacent tile codes are also
  ;; returned.

  (define (xy->tile-code x y)
    (bitwise-ior (arithmetic-shift x tile-level) y))

  (define upper-limit 0.8)
  (define lower-limit 0.2)

  (let* ((x0 (* tile-mult (map-point-x mp)))
         (y0 (* tile-mult (map-point-y mp)))
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

(define fetch-points-query
  ;; SQL query to fetch all points for a tile code.
  (virtual-statement
   (lambda (dbsys)
     "\
select position_lat, position_long, altitude
from A_TRACKPOINT where tile_code = ?
  and position_lat is not null
  and position_long is not null
  and altitude is not null")))

(define (make-tile-code-fetcher db)
  ;; Return a function that produces the points for a tile code.  Points are
  ;; fetched from the database as needed and kept in a cache.

  (define cache (make-hash))

  (define (populate-cache tile-code)

    (define trackpoints (query-rows db fetch-points-query tile-code))

    ;; We determine the mean and stddev of the altitude in this tile.  We will
    ;; filter out points who'se altidude is too far away from the mean (2 *
    ;; stddev).  Hopefully this will eliminate some of the bad altitude data
    ;; we have...
    (define stats
      (foldl (lambda (tp stats)
               (update-statistics stats (vector-ref tp 2)))
             empty-statistics
             trackpoints))
    (define mean (statistics-mean stats))
    (define 2stddev (* 2 (statistics-stddev stats #:bias #t)))

    (define points
      (for/list ([tp (in-list trackpoints)] #:when (< (abs (- (vector-ref tp 2) mean)) 2stddev))
        (match-define (vector lat lon alt) tp)
        (vector (degrees->radians lat)
                (degrees->radians lon)
                alt)))
    (hash-set! cache tile-code points)
    ;; Return the points we just inserted
    points)

  (lambda (tile-code)
    (let ([data (hash-ref cache tile-code (lambda () #f))])
      (if data data (populate-cache tile-code)))))

(define (average-altitude lat long altitude-data)
  ;; Return the average altitude at LAT, LONG based on points in
  ;; ALTITUDE-DATA.  ALTITUDE-DATA is a function which returns a set of points
  ;; given a tile code.

  ;; Implementaion: we select points in the tile containing LAT/LONG plus all
  ;; surronding tiles and average the altitude of those points weighted by
  ;; their distance from LAT/LONG.

  ;; Distance in meters from LAT/LONG where the weight will be 0.5 (points
  ;; closer than this will have a weight growing towards 1 (1 being the weight
  ;; of the point exactly at LAT/LONG), points further away than this value
  ;; will have their weight further decreasing towards 0.
  (define half-weight-distance (->fl 30)) ; meters

  ;; Update SUM-DIV, a (cons SUM DIV), with data from POINTS and return a new
  ;; SUM-DIV.  SUM contains the sum of the altitude of the points weighted by
  ;; their distance from LAT/LONG and DIV is the sum of the weights.  The
  ;; average altitude will be (/ (car sum-div) (cdr sum-div)).  This function
  ;; allows us to process several distinct lists of points.
  (define (accumulate-average-altitude lat long points sum-div)
    (match-define (cons sum div) sum-div)
    (for ([p (in-list points)])
      (match-define (vector plat plon palt) p)
      (let* ((d (map-distance/radians lat long plat plon))
             (w (flmax 0.0 (fl/ half-weight-distance (fl+ d half-weight-distance)))))
        (set! sum (fl+ (fl* w palt) sum))
        (set! div (fl+ div w))))
    (cons sum div))

  (let ((r-lat (degrees->radians lat))
        (r-long (degrees->radians long))
        (sum-div (cons 0.0 0.0))
        (i-lat (exact->inexact lat))
        (i-lon (exact->inexact long)))
    (for ([tile-code (candidate-tile-codes (lat-lon->map-point i-lat i-lon))])
      (let ([points (altitude-data tile-code)])
        (set! sum-div (accumulate-average-altitude r-lat r-long points sum-div))))
    (if (> (cdr sum-div) 0)
        (fl/ (car sum-div) (cdr sum-div))
        #f)))

(define session-trackpoints-query
  ;; SQL query to return all track points for a session.  Note that the
  ;; trackpoints are not returned in any particular order.
  (virtual-statement
   (lambda (dbsys) "
select T.id, T.position_lat, T.position_long
from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where P.session_id = ?
  and L.lap_id = P.id
  and T.length_id = L.id
  and position_lat is not null and position_long is not null
order by T.timestamp")))

(define (get-trackpoints-for-session db session-id [progress-monitor #f])
  ;; Return latitude/longitude data for all track points in a session, along
  ;; with the trackpoint id. Returns a list of (vector trackpoint-id latitude
  ;; longitude).  Trackpoints are not returned in any particular order.
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session" 0))
  (query-rows db session-trackpoints-query session-id))

;; Track point produced by 'calculate-altitude' and used by 'smooth-altitude'.
(struct tpoint (id lat lon dst (calt #:mutable)))

(define (calculate-altitude trackpoints altitude-data [progress-monitor #f] [progress-step 100])
  ;; Calculate the corrected altitude for all points in TRACKPOINTS based of
  ;; ALTITUDE-DATA.  Return a vector of TPOINT structures.
  (when progress-monitor
    (send progress-monitor begin-stage "Calculating GPS track altitude" (length trackpoints)))
  (let ((num-processed 0)
        (prev-lat #f)
        (prev-lon #f)
        (distance 0))
    (for/vector ([point (in-list trackpoints)])
      (set! num-processed (+ num-processed 1))
      (when (and progress-monitor (= (remainder num-processed progress-step) 0))
        (send progress-monitor set-progress num-processed))
      (match-define (vector id lat lon) point)
      (when (and prev-lat prev-lon)
        (set! distance (+ distance (map-distance/degrees prev-lat prev-lon lat lon))))
      (set! prev-lat lat)
      (set! prev-lon lon)
      (tpoint id lat lon distance
              (average-altitude lat lon altitude-data)))))

(define (smooth-altitude trackpoints)
  ;; Smooth the altitude points produced by 'calculate-altitude' using an idea
  ;; from http://regex.info/blog/2015-05-09/2568, the resulting altitude is
  ;; only marginally better from a visual point of view (and somewhat worse
  ;; for small elevation changes), but total ascent and descent calculated off
  ;; this smoothed data are significantly more accurate.
  ;;
  ;; NOTE: TRACKPOINTS have their CALT updated in place.

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

(define update-trackpoint-stmt
  ;; SQL statement to update the corrected altitude for a trackpoint
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set corrected_altitude = ? where id = ?")))

(define (update-trackpoints-altitude db altitude-data
                                     [progress-monitor #f] [progress-step 100])
  ;; Update the trackpoints in the database with altitude data from
  ;; GPS-TRACK-ALTIDUDE (a list of (cons id altitude)
  (when progress-monitor
    (send progress-monitor begin-stage "Updating altitude for GPS track" (vector-length altitude-data)))
  (let ((num-processed 0))
    (call-with-transaction
     db
     (lambda ()
       (for ((point (in-vector altitude-data)))
         (set! num-processed (+ num-processed 1))
         (when (and progress-monitor (= (remainder num-processed progress-step) 0))
           (send progress-monitor set-progress num-processed))
         (let ((id (tpoint-id point))
               (calt (tpoint-calt point)))
           (query-exec db update-trackpoint-stmt (or calt sql-null) id)))))))

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

(define (update-summary-altitude-for-length db length-id)
  ;; Update the summary altitude data (ascent and descent) for a session
  ;; length based on the already corrected trackpoints in this length.
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

(define (update-summary-altitude-for-lap db lap-id)
  ;; Update the summary altitude data (ascent and descent) for a session lap
  ;; based on the already corrected trackpoints in this lap (the summary data
  ;; for the laps lengths will also be corrected).
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

(define (update-summary-altitude-for-session db session-id)
  ;; Update the summary altitude data (ascent and descent) for a session based
  ;; on the already corrected trackpoints in this session (the summary data
  ;; for the laps and lengths will also be corrected).
  (let ((laps (query-list db "select id from A_LAP where session_id = ?" session-id)))
    (for ((lap (in-list laps)))
      (update-summary-altitude-for-lap db lap))
    (let ((row (query-row db q-get-altitude3 session-id)))
      (query-exec db q-update-ss3 (vector-ref row 0) (vector-ref row 1) session-id))))

(define (fixup-elevation-for-session-internal db session-id altitude-data [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session..." 0))
  (define trackpoints (get-trackpoints-for-session db session-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for session..." 0))
  (define tp-elevation (calculate-altitude trackpoints altitude-data progress-monitor))
  (smooth-altitude tp-elevation)
  (call-with-transaction
   db
   (lambda ()
     (update-trackpoints-altitude db tp-elevation progress-monitor)
     (when progress-monitor
       (send progress-monitor begin-stage (format "Updating summary altitude") 0))
     (update-summary-altitude-for-session db session-id))))

(define (fixup-elevation-for-session db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define altidude-data (make-tile-code-fetcher db))
  (if (cons? session-id)
      (for/list ([sid session-id])
        (dbglog "fixup-elevation-for-session ~a started" sid)
        (fixup-elevation-for-session-internal db sid altidude-data progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" sid))
      (begin
        (dbglog "fixup-elevation-for-session ~a started" session-id)
        (fixup-elevation-for-session-internal db session-id altidude-data progress-monitor)
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
