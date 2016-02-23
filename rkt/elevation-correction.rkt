#lang racket/base
;; elevation-correction.rkt -- elevation correction for trackpoints
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require db
         racket/class
         racket/flonum
         racket/gui/base
         racket/math
         "dbglog.rkt"
         "fmt-util.rkt"
         "icon-resources.rkt"
         "map-util.rkt"
         "widgets.rkt")

(provide populate-altitude-data)
(provide fixup-elevation-for-session)
(provide fixup-elevation-for-all-sessions)
(provide interactive-fixup-elevation)

;; Return the "box" where the (LAT LONG) coordinate resides.  The entire Earth
;; surface is subdivided in a grid of one arc second boxes, which correspond
;; to 30m x 30m squares.

(define (pos->box lat long)
  ;; NOTE: (futures) we use flfloor since this is a primitive operation which
  ;; will not block when called as part of a future.  This limits LAT and LONG
  ;; parameters to be flonums (which they are if called in
  ;; `fetch-altitude-data')
  (cons (inexact->exact (flfloor (* lat 3600.0)))
        (inexact->exact (flfloor (* long 3600.0)))))

;; Select all trackpoints that have a GPS position and an altidude
(define *q1*
  "select position_lat, position_long, altitude 
     from A_TRACKPOINT 
    where position_lat is not null 
      and position_long is not null 
      and altitude is not null")

;; Select all trackpoints that have a GPS position and an altitude and are not
;; withing the first 5 minutes of a session.  It seems that altitude data at
;; the start of a session is unreliable, so we discard it.  This query is
;; probably slower than *q1*, but the trackpoints it produces provide a better
;; approximation of the elevation.
(define *q2*
"select position_lat, position_long, altitude 
  from A_TRACKPOINT T, A_LENGTH L, A_LAP LA, A_SESSION S
 where position_lat is not null 
   and position_long is not null 
   and altitude is not null
   and T.length_id = L.id
   and L.lap_id = LA.id
   and LA.session_id = S.id
   and (T.timestamp - S.start_time) > 300")

;; Add trackpoints (lat, long and elevation) from an ActivityLog database to a
;; hash table and return it. The positions are grouped in "boxes" (see
;; pos->box) in the hash table for faster lookup.
;;
;; NOTE: latidude and longitude are converted to radians.
(define (fetch-altitude-data db)

  (define (mkpoint lat long altitude)
    (vector (degrees->radians lat) (degrees->radians long) altitude))

  (let ((altitude-data (make-hash)))
    (for (((lat long altitude) 
           (in-query db *q2* #:fetch 1000)))
      (let* ((box (pos->box lat long))
             (others (hash-ref altitude-data box '())))
        (hash-set! altitude-data box (cons (mkpoint lat long altitude)  others))))
    altitude-data))

;; `fetch-altitude-data' is called in a separate thread to prevent the main
;; thread locking up.  Placing a data base reference onto the *request-ch*
;; will cause an altitude hash table to be returned on the *reply-ch*.
(define *request-ch* (make-channel))
(define *reply-ch* (make-channel))

;; Setup a thread that continuously reads from *request-ch* and puts replys on
;; *reply-ch* with the altitude data.
(define *th* (thread 
              (lambda ()
                (define (loop)
                  (let ((db (channel-get *request-ch*)))
                    (when db            ; exit the thread if #f is received
                      (channel-put *reply-ch* (fetch-altitude-data db))
                      (loop))))
                (loop))))

(define altitude-data #f)
  
(define (populate-altitude-data db (wait? #f)) 
  (channel-put *request-ch* db)
  (when wait?
    (set! altitude-data #f)
    (get-altitude-data)))

(define (get-altitude-data)
  (let ((new-data ((if altitude-data channel-try-get channel-get) *reply-ch*)))
    (when new-data (set! altitude-data new-data))
    altitude-data))

(define (point-lat p) (vector-ref p 0))
(define (point-long p) (vector-ref p 1))
(define (point-altitude p) (vector-ref p 2))

;; Return the average altitude at LAT, LONG based on points in ALTITUDE-DATA
(define (average-altitude lat long altitude-data)

  ;; Implementaion: we select points in the box (see pos->box) containing
  ;; LAT/LONG plus all surronding boxes and average the altitude of those
  ;; points weighted by their distance from LAT/LONG.
  
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
    (let ((sum (car sum-div))
          (div (cdr sum-div)))
      (for ((p (in-list points)))
        (let* ((d (map-distance/radians lat long (point-lat p) (point-long p)))
               (w (flmax 0.0 (fl/ half-weight-distance (fl+ d half-weight-distance)))))
          (set! sum (fl+ (fl* w (point-altitude p)) sum))
          (set! div (fl+ div w))))
      (cons sum div)))

  (define (get-points b) (hash-ref altitude-data b '()))

  (let ((b0 (pos->box lat long)))
    (let ((b1 (cons (car b0) (+ (cdr b0) 1)))
          (b2 (cons (+ (car b0) 1) (+ (cdr b0) 1)))
          (b3 (cons (+ (car b0) 1) (cdr b0)))
          (b4 (cons (+ (car b0) 1) (- (cdr b0) 1)))
          (b5 (cons (car b0) (- (cdr b0) 1)))
          (b6 (cons (- (car b0) 1) (- (cdr b0) 1)))
          (b7 (cons (- (car b0) 1) (cdr b0)))
          (b8 (cons (- (car b0) 1) (+ (cdr b0) 1))))
      (let ((r-lat (degrees->radians lat))
            (r-long (degrees->radians long))
            (sum-div (cons 0.0 0.0)))
        ;; NOTE: we unroll this bit to avoid memory allocations if we would
        ;; construct a consolidated list of points.
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b0) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b1) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b2) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b3) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b4) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b5) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b6) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b7) sum-div))
        (set! sum-div (accumulate-average-altitude r-lat r-long (get-points b8) sum-div))

        (if (> (cdr sum-div) 0)
            (fl/ (car sum-div) (cdr sum-div))
            0.0)))))

(define q-get-gps-track
  (virtual-statement
   (lambda (dbsys) "
select T.id, T.position_lat, T.position_long
from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where P.session_id = ?
  and L.lap_id = P.id
  and T.length_id = L.id
  and position_lat is not null and position_long is not null")))

;; Return latitude/longitude data for all track points in a session that have
;; them, along with the trackpoint id.
;;
;; Returns a list of (vector trackpoint-id latitude longitude)
(define (get-gps-track-for-session db session-id [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session" 0))
  (query-rows db q-get-gps-track session-id))

;; Calculate the corrected altitude for all points in GPS-TRACK based of
;; ALTITUDE-DATA.  Return a list of (cons id corrected-altitude).
(define (calculate-track-altitude gps-track altitude-data [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage "Calculating GPS track altitude" (length gps-track)))
  (let ((num-processed 0))
    (for/list ((point (in-list gps-track)))
      (set! num-processed (+ num-processed 1))
      (when (and progress-monitor (= (remainder num-processed progress-step) 0))
        (send progress-monitor set-progress num-processed))
      (let ((id (vector-ref point 0))
            (lat (vector-ref point 1))
            (long (vector-ref point 2)))
        (cons id (average-altitude lat long altitude-data))))))

(define q-update-trackpoint
  (virtual-statement
   (lambda (dbsys)
     "update A_TRACKPOINT set corrected_altitude = ? where id = ?")))

;; Update the trackpoints in the databse with altitude data from
;; GPS-TRACK-ALTIDUDE (a list of (cons id altitude)
(define (update-gps-track db gps-track-altidude
                          [progress-monitor #f] [progress-step 100])
  (when progress-monitor
    (send progress-monitor begin-stage "Updating altitude for GPS track" (length gps-track-altidude)))
  (let ((num-processed 0))
    (call-with-transaction
     db
     (lambda ()
       (for ((point (in-list gps-track-altidude)))
         (set! num-processed (+ num-processed 1))
         (when (and progress-monitor (= (remainder num-processed progress-step) 0))
           (send progress-monitor set-progress num-processed))
         (query-exec db q-update-trackpoint (or (cdr point) sql-null) (car point)))))))

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
            ;; (printf "diff ~a, ascent ~a, descent ~a~%" diff ascent descent)
            (if (> diff 0)
                (set! ascent (+ ascent diff))
                (set! descent (+ descent (- diff))))))
        (query-exec db q-update-ss1 
                    (exact-truncate ascent) (exact-truncate descent) length-id)))))

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
  ;; (printf "*** update-summary-altitude-for-lap ~a~%" lap-id)
  (let ((lengths (query-list db "select id from A_LENGTH where lap_id = ?" lap-id)))
    (for ((length (in-list lengths)))
      (update-summary-altitude-for-length db length))
    (let ((row (query-row db q-get-altitude2  lap-id)))
      (query-exec db q-update-ss2
                  (vector-ref row 0) (vector-ref row 1) lap-id))))

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
      (query-exec db q-update-ss3
                  (vector-ref row 0) (vector-ref row 1) session-id))))

(define (fixup-elevation-for-session-internal db session-id altitude-data [progress-monitor #f])
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching GPS track for session..." 0))
  (define gps-track (get-gps-track-for-session db session-id progress-monitor))
  (when progress-monitor
    (send progress-monitor begin-stage "Correcting elevation for session..." 0))
  (define gps-elevation (calculate-track-altitude gps-track altitude-data progress-monitor))

  (call-with-transaction
   db 
   (lambda ()
     (update-gps-track db gps-elevation progress-monitor)
     (when progress-monitor
       (send progress-monitor begin-stage (format "Updating summary altitude") 0))
     (update-summary-altitude-for-session db session-id))))
  
(define (fixup-elevation-for-session db session-id [progress-monitor #f])
  (dbglog (format "fixup-elevation-for-session ~a started" session-id))
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define altidude-data (populate-altitude-data db #t))
  (fixup-elevation-for-session-internal db session-id altidude-data progress-monitor)
  (when progress-monitor 
    (send progress-monitor finished))
  (dbglog (format "fixup-elevation-for-session ~a completed" session-id)))

(define (fixup-elevation-for-all-sessions db [progress-monitor #f])
  (dbglog "fixup-elevation-for-all-sessions started")
  (when progress-monitor
    (send progress-monitor begin-stage "Fetching altitude data..." 0))
  (define altidude-data (populate-altitude-data db #t))
  
  (let ((sessions (query-list db "select id from A_SESSION")))
    (when progress-monitor
      (send progress-monitor begin-stage "Fixup elevation for all sessions" 
            (length sessions)))
    (for ((s (in-list sessions))
          (i (in-range (length sessions))))
      #:break (if progress-monitor
                  (not (send progress-monitor set-progress i))
                  #f)
      (fixup-elevation-for-session-internal db s altidude-data #f)))

  (when progress-monitor 
    (send progress-monitor finished))
  (dbglog "fixup-elevation-for-all-sessions completed"))

(define (interactive-fixup-elevation database session-id [parent-window #f])

  (define progress-dialog
    (new al-progress-dialog%
         [title "Update elevation data"]
         [icon sql-export-icon]))

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



