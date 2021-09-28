#lang racket/base
;; elevation-correction.rkt -- elevation correction for trackpoints
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         data-frame
         "../dbutil.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt"
         "../gps-segments/gps-segments.rkt")

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


;;....................................................... geoid altitude ....

(define-runtime-path geoid-altitudes-query-file "../../sql/queries/ec-geoid-altitudes.sql")
(define geoid-altitudes-query (define-sql-statement geoid-altitudes-query-file))

;; Helper class to manage the averaging of altitude data from GEOIDs.  To
;; avoid retrieving the same data repeatedly, we cache various intermediate
;; results.  The only public method in this class is `geoid-altitude`.
(define altitude-helper%
  (class object%
    (init-field database)
    (super-new)

    ;; WARNING: all the hashes below are immutable.  Geoids hash poorly with
    ;; mutable hash tables and there is a huge performance penalty when they
    ;; are used as keys in mutable hash tables.

    ;; Map the altitude points for a GEOID, this stores results previously
    ;; retrieved by `fetch-geoid-altitudes`
    (define altitudes-for-geoid (hash))

    ;; Map the outlier limits for altitude data for a GEOID.  The key is the
    ;; geoid, the value is a cons cell of the low and high values.
    (define outlier-limits-for-geoid (hash))

    ;; Map the calculated altitude for each geoid.
    (define altitude-for-geoid (hash))

    ;; Fetch the list of original altitudes (A_TRACKPOINT.alt) for all geoids
    ;; in the database DB, which are inside GEOID (this is assumed to NOT be a
    ;; leaf geoid).  Returns a list of numbers representing the altitudes.
    (define/private (fetch-geoid-altitudes geoid)
      (define-values (start end) (leaf-span geoid))
      (query-list database
                  (geoid-altitudes-query)
                  (geoid->sqlite-integer start)
                  (geoid->sqlite-integer end)))

    ;; Return the list of altitudes for all geoids in the database which are
    ;; inside GEOID (which is assumed to NOT be a leaf geoid).  This method
    ;; maintains the `altitudes-for-geoid` cache to avoid retrieving the same
    ;; data multiple times.
    (define/private (geoid-altitudes geoid)
      ;;(define key (~a geoid))
      (define key geoid)
      (define data (hash-ref altitudes-for-geoid key #f))
      (unless data
        (set! data (fetch-geoid-altitudes geoid))
        (set! altitudes-for-geoid (hash-set altitudes-for-geoid key data)))
      data)

    ;; Determine the altitude limits which would be considered outliers based
    ;; on all the altitude data in GEOID and its adjacent neighbors.  This
    ;; data will be used to throw out unreasonable altitude values (GPS based
    ;; altimeters can record altitude values which are off by hundreds of
    ;; meters).  Returns a cons cell of the lower and upper range, or a cons
    ;; cell of #f values if there are no altitude values in this GEOID.
    ;;
    ;; Implementation note: we use the following method to determine the
    ;; outliers: the 25% and 75% quantiles are determined from all the
    ;; altitude values.  The difference between the two values is the
    ;; inter-quantile range (IQR).  The upper and lower limits are determined
    ;; as 1.5 times the IQR over the 75% and under the 25% limits.
    (define/private (determine-outlier-limits geoid)
      (define candidates (cons geoid (adjacent-geoids geoid)))
      (define data (append*
                    (for/list ([g (in-list (cons geoid (adjacent-geoids geoid)))])
                      (geoid-altitudes g))))
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

    ;; Return the outlier limits for GEOID -- this calls
    ;; `determine-outlier-limits`, but stores the retrieved values in
    ;; `outlier-limits-for-geoid` hash, to avoid re-calculating them.
    (define/private (outlier-limits geoid)
      ;;(define key (~a geoid))
      (define key geoid)
      (define limits (hash-ref outlier-limits-for-geoid key #f))
      (unless limits
        (set! limits (determine-outlier-limits geoid))
        (set! outlier-limits-for-geoid (hash-set outlier-limits-for-geoid key limits)))
      limits)

    ;; Determine the altitude for GEOID by averaging the altitude values for
    ;; all geoids inside it, after outlier points have been removed.
    (define/private (determine-geoid-altitude geoid)
      ;; Note that we determine the outliers for geoid level 14 (and its
      ;; adjacent neighbors).  We use a larger geoid, since the current one
      ;; will be quite small (8-10 level) and might not contain enough points
      ;; to determine relevant outlier limits.
      (match-define (cons lower-limit upper-limit) (outlier-limits (enclosing-geoid geoid 14)))
      (define candidates (geoid-altitudes geoid))
      (for/fold ([sum 0]
                 [cnt 0]
                 #:result (if (> cnt 0) (/ sum cnt) #f))
                ([p (in-list candidates)]
                 #:when (and (>= p lower-limit) (<= p upper-limit)))
        (values (+ sum p) (add1 cnt))))

    ;; Return the average altitude for GEOID, based on all the altitudes for
    ;; the geoids in the database which are inside it.  This uses
    ;; `determine-geoid-altitude` to do the actual calculation and caches the
    ;; result in `altitude-for-geoid`
    (define/public (geoid-altitude geoid)
      ;;(define key (~a geoid))
      (define key geoid)
      (define altitude (hash-ref altitude-for-geoid key #f))
      (unless altitude
        (set! altitude (determine-geoid-altitude geoid))
        (set! altitude-for-geoid (hash-set altitude-for-geoid key altitude)))
      altitude)

    ))

;; Return a function that produces the altitude for a geoid.  Points are
;; fetched from the database as needed to calculate the average altitude, and
;; kept in a cache
(define (make-geoid-fetcher db)

  (define helper (new altitude-helper% [database db]))

  (lambda (geoid)
    (send helper geoid-altitude geoid)))

;; Return the average altitude at GEOID (presumably a leaf geoid), based on
;; the average altitude of nearby geoids.
;;
;; FETCH-ALTITUDE is a function which returns the altitude for a given GEOID.
(define (calculate-average-altitude geoid fetch-altitude)
  ;; Distance in meters from GEOID where the weight will be 0.5.  Points
  ;; closer than this will have a weight growing towards 1, 1 being the weight
  ;; of the point exactly at GEOID, points further away than this value will
  ;; have their weight further decreasing towards 0.
  (define hw-distance (->fl 5))

  ;; We determine the average altitude of the geoid which encloses our geoid
  ;; and its adjacent neighbors -- level 10 is about 8x8 meters.
  (define area-geoid (enclosing-geoid geoid 10))

  ;; We iterate over each candidate geoid use and compute w weighted average
  ;; of their altitudes.  The average is weighted because we are unlikely to
  ;; find a point at this exact location and we don't want to straight average
  ;; points at other nearby locations.
  (define-values (sum div)
    (for/fold ([sum 0.0]
               [div 0.0])
              ([g (in-list (cons area-geoid (adjacent-geoids area-geoid)))])
      (define avg-alt (fetch-altitude g))
      (if avg-alt
          (let* [(distance (distance-between-geoids geoid g))
                 [weight (flmax 0.0 (fl/ hw-distance (fl+ distance hw-distance)))]]
            (values (fl+ (fl* weight avg-alt) sum) (fl+ div weight)))
          (values sum div))))

  (if (> div 0) (fl/ sum div) #f))


;;..................................................... average-altitude ....

;; A single track point produced by 'calculate-altitude' and used by
;; 'smooth-altitude'.
(struct tpoint (id geoid dst (calt #:mutable)))

;; Calculate the corrected altitude for all points in TRACKPOINTS based of
;; ALTITUDE-DATA.  Calls `CALCULATE-AVERAGE-ALTITUDE for each point in
;; TRACKPOINTS and returns vector of TPOINT structures.
(define (average-altitude trackpoints fetch-altitude
                          [progress-monitor #f] [progress-step 100])
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
          (tpoint id geoid distance (calculate-average-altitude geoid fetch-altitude))))))


;;...................................................... smooth-altitude ....

;; Smooth the altitude in TRACKPOINTS using a low pass filter.  The filter
;; width is determined empirically, but it is adjusted to the average distance
;; between the points on the track, meaning slower activities (e.g. hiking)
;; have less smoothing than faster ones (e.g. cycling)
(define (smooth-altitude trackpoints)
  (define point-count (vector-length trackpoints))
  (define average-delta-distance
    (/ (tpoint-dst (vector-ref trackpoints (sub1 point-count)))
       point-count))
  (define filter-width (* 15.0 average-delta-distance))
  (for/fold
      ([pdst (tpoint-dst (vector-ref trackpoints 0))]
       [palt (tpoint-calt (vector-ref trackpoints 0))])
      ([p (in-vector trackpoints 1)])
    (match-define (tpoint _id _geoid dst alt) p)
    (if palt
        (if alt
            (let* ([delta (- dst pdst)]
                   [alpha (/ delta (+ delta filter-width))]
                   [salt (+ (* alpha alt) (* (- 1.0 alpha) palt))])
              (set-tpoint-calt! p salt)
              (values dst salt))
            (begin
              (set-tpoint-calt! p palt)
              (values dst palt)))
        (values dst alt))))


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
      ;; NOTE: we only accumulate ascent and descent if the elevation gain or
      ;; loss is greater than 1 meter -- this avoids accumulating lots of very
      ;; small elevation changes, which would artificially inflate the total
      ;; elevation gain.
      (define-values (ascent descent)
        (for/fold ([ascent 0.0]
                   [descent 0.0]
                   [base (car altitude)]
                   #:result (values ascent descent))
                  ([current (in-list (cdr altitude))])
          (cond ((> current (add1 base))
                 (values (+ ascent (- current base)) descent current))
                ((< current (sub1 base))
                 (values ascent (+ descent (- base current)) current))
                (#t
                 (values ascent descent base)))))
        (query-exec db q-update-ss1
                    (exact->inexact ascent) (exact->inexact descent) length-id))))

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

(define-runtime-path session-geoids-query-file "../../sql/queries/ec-session-geoids.sql")
(define session-geoids-query (define-sql-statement session-geoids-query-file))

;; Return the geoids for all track points in a session, along with the
;; trackpoint id. Returns a list of (vector trackpoint-id geoid)
(define (session-trackpoints db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session" 0))
  (for/list ([(id geoid) (in-query db (session-geoids-query) session-id #:fetch 1000)])
    (vector id (sqlite-integer->geoid geoid))))

(define (fixup-elevation-for-session-internal db session-id fetch-altitude-fn [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session..." 0))
  (define trackpoints (session-trackpoints db session-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for session..." 0))
  (unless (null? trackpoints)           ; maybe the session has no GPS data?
    (define tp-elevation (average-altitude trackpoints fetch-altitude-fn progress-monitor))
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
  (define fetch-altitude-fn (make-geoid-fetcher db))
  (if (cons? session-id)
      (for/list ([sid session-id])
        (dbglog "fixup-elevation-for-session ~a started" sid)
        (fixup-elevation-for-session-internal db sid fetch-altitude-fn progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" sid))
      (begin
        (dbglog "fixup-elevation-for-session ~a started" session-id)
        (fixup-elevation-for-session-internal db session-id fetch-altitude-fn progress-monitor)
        (dbglog "fixup-elevation-for-session ~a completed" session-id)))
  (when progress-monitor
    (send progress-monitor finished)))

(define (fixup-elevation-for-all-sessions db [progress-monitor #f])
  (dbglog "fixup-elevation-for-all-sessions started")
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define fetch-altitude-fn (make-geoid-fetcher db))
  (let ((sessions (query-list db "select id from A_SESSION")))
    (when progress-monitor
      (send progress-monitor begin-stage "Fixup elevation for all sessions"
            (length sessions)))
    (for (((sid index) (in-indexed (in-list sessions))))
      #:break (if progress-monitor
                  (not (send progress-monitor set-progress index))
                  #f)
      (fixup-elevation-for-session-internal db sid fetch-altitude-fn #f)))
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


;;.......................................... fixup-elevation-for-segment ....

(define-runtime-path segment-geoids-query-file "../../sql/queries/ec-segment-geoids.sql")
(define segment-geoids-query (define-sql-statement segment-geoids-query-file))

;; Return the geoids for all track points in a segment, along with the
;; waypoint id. Returns a list of (vector waypoint-id geoid)
(define (segment-waypoints db segment-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for segment" 0))
  (for/list ([(id geoid) (in-query db (segment-geoids-query) segment-id #:fetch 1000)])
    (vector id (sqlite-integer->geoid geoid))))

;; SQL statement to update the corrected altitude for a trackpoint
(define update-waypoint-stmt
  (virtual-statement
   (lambda (dbsys)
     "update GPS_SEGMENT_WAYPOINT set altitude = ? where id = ?")))

;; Update the GPS_SEGMENT_WAYPOINT rows in the database with altitude data
;; from ALTITUDE-DATA which is a vector of TPOINT structures as produced by
;; AVERAGE-ALTITUDE.
(define (update-segment-waypoints db altitude-data
                                  [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage
          "Updating altitude for GPS segment" (vector-length altitude-data)))
  (call-with-transaction
   db
   (lambda ()
     (for ([point (in-vector altitude-data)]
           [index (in-naturals)])
       (when (and progress-monitor (= (remainder (add1 index) progress-step) 0))
         (send progress-monitor set-progress (add1 index)))
       (let ((id (tpoint-id point))
             (calt (tpoint-calt point)))
         (query-exec db update-waypoint-stmt (or calt sql-null) id))))))

;; SQL statement to update the grade for a waypoint
(define update-segment-grade-stmt
  (virtual-statement
   (lambda (dbsys)
     "update GPS_SEGMENT_WAYPOINT set grade = ? where id = ?")))

(define-runtime-path update-segment-summary-query-file "../../sql/queries/ec-update-segment.sql")
(define update-segment-summary-query (define-sql-statement update-segment-summary-query-file))

;; Update in the database the fields derived from altitude data in the segment
;; identified by SEGMENT-ID.  The segment is assumed to already have its
;; altitude data updated (see `update-segment-waypoints`), and ALTITUDE-DATA
;; is used just to find the GPS_SEGMENT_WAYPOINT database IDs of each
;; waypoint.
(define (update-summary-altitude-for-segment db segment-id altitude-data)
  ;; force refresh of the segment, to get new altitude data
  (log-event 'gps-segment-updated segment-id)
  (define segment (fetch-gps-segment db segment-id))
  ;; remove grade series as it is now incorrect, as well as all of the
  ;; altitude related summary properties.
  (df-del-series! segment "grade")
  (for ([p (in-list '(segment-height segment-grade total-ascent total-descent
                                     max-grade min-elevation max-elevation))])
    (df-del-property! segment p))
  ;; Add grade series and new summary data based on fresh altitude
  (fixup-segment-data segment)
  (call-with-transaction
   db
   (lambda ()
     ;; Put the grade values back
     (for ([point (in-vector altitude-data)]
           [grade (in-data-frame segment "grade")])
       (let ((id (tpoint-id point)))
         (query-exec db update-segment-grade-stmt (or grade sql-null) id)))
     (query-exec
      db
      (update-segment-summary-query)
      (df-get-property segment 'segment-height sql-null)
      (df-get-property segment 'segment-grade sql-null)
      (df-get-property segment 'total-ascent sql-null)
      (df-get-property segment 'total-descent sql-null)
      (df-get-property segment 'max-grade sql-null)
      (df-get-property segment 'min-elevation sql-null)
      (df-get-property segment 'max-elevation sql-null)
      segment-id))))

;; Same as `fixup-elevation-for-session-internal`, except for GPS segments
;; instead of sessions
(define (fixup-elevation-for-segment-internal db segment-id fetch-altitude-fn [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for segments..." 0))
  (define trackpoints (segment-waypoints db segment-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for segment..." 0))
  (unless (null? trackpoints)           ; maybe the segment has no GPS data?
    (define tp-elevation (average-altitude trackpoints fetch-altitude-fn progress-monitor))
    (smooth-altitude tp-elevation)
    (call-with-transaction
     db
     (lambda ()
       (update-segment-waypoints db tp-elevation progress-monitor)
       (when progress-monitor
         (send progress-monitor begin-stage (format "Updating summary altitude") 0))
       (update-summary-altitude-for-segment db segment-id tp-elevation)))))

;; Same as `fixup-elevation-for-session`, except for GPS segments instead of
;; sessions
(define (fixup-elevation-for-segment db segment-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define fetch-altitude-fn (make-geoid-fetcher db))
  (dbglog "fixup-elevation-for-segment ~a started" segment-id)
  (fixup-elevation-for-segment-internal db segment-id fetch-altitude-fn progress-monitor)
  (dbglog "fixup-elevation-for-segment ~a completed" segment-id)
  (when progress-monitor
    (send progress-monitor finished)))

;; Same as `interactive-fixup-elevation-for-session`, except for GPS segments
;; instead of sessions
(define (interactive-fixup-elevation-for-segment database segment-id [parent-window #f])

  (define progress-dialog
    (new progress-dialog%
         [title "Update elevation data for segment"]
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
      (when segment-id
        (fixup-elevation-for-segment database segment-id m))))

  (send progress-dialog run parent-window task))



;;............................................................. provides ....

(provide/contract
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
 (clear-corrected-elevation-for-session (-> connection? exact-nonnegative-integer? any/c))

 (fixup-elevation-for-segment (->* (connection? exact-nonnegative-integer?)
                                   (any/c) ; the progress monitor
                                   any/c))
 (interactive-fixup-elevation-for-segment (->* (connection?
                                                (or/c #f
                                                      exact-nonnegative-integer?
                                                      (listof exact-nonnegative-integer?)))
                                               (any/c) ; the parent window
                                               any/c)))
