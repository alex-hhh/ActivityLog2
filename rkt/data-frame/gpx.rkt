#lang racket/base
;; gpx.rkt -- read and write GPX files from data frames
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/contract
         racket/date
         racket/flonum
         racket/format
         racket/match
         racket/math
         (only-in srfi/19/time string->date)
         xml
         "df.rkt"
         "exn.rkt"
         "series.rkt")


;;................................................. map-distance/degrees ....

;; Formulas from http://www.movable-type.co.uk/scripts/latlong.html

(define earth-radius (->fl 6371000))    ; meters

(define (haversin theta)
  (fl/ (fl- 1.0 (flcos theta)) 2.0))

(define (inv-haversin h)
  (fl* 2.0 (flasin (flsqrt h))))

;; Calculate the distance in meters between two map coordinates
(define (map-distance/radians lat1 lon1 lat2 lon2)
  (let ((delta-lat (fl- lat2 lat1))
        (delta-lon (fl- lon2 lon1)))
    (let* ((a (fl+ (haversin delta-lat)
                   (fl* (fl* (flcos lat1) (flcos lat2))
                        (haversin delta-lon))))
           (c (inv-haversin a)))
      (fl* c earth-radius))))

(define (map-distance/degrees lat1 lon1 lat2 lon2)
  (map-distance/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))


;;......................................................... df-write/gpx ....

(define gpx-header-tag
  "<gpx xmlns=\"http://www.topografix.com/GPX/1/1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" creator=\"ActivityLog2\" version=\"1.1\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\">\n")

(define gpx-indent (make-parameter 0))
(define gpx-indent-string (make-parameter ""))
(define gpx-export-series (make-parameter '("timestamp" "lat" "lon" "alt")))

(define (gpx-timestamp timestamp)

  (define (fmt num width)
    (~a #:width width #:align 'right #:pad-string "0" num))

  (let ((d (seconds->date timestamp #f)))
    (string-append
     (fmt (date-year d) 4) "-" (fmt (date-month d) 2) "-" (fmt (date-day d) 2)
     "T"
     (fmt (date-hour d) 2) ":" (fmt (date-minute d) 2) ":" (fmt (date-second d) 2) "Z")))

;; Write to (current-output-port) the XML content for a GPX track point.
(define (gpx-emit-trkpt lat lon alt timestamp)
  (let ((indent (gpx-indent-string)))
    (write-string (format "~a<trkpt lat=\"~a\" lon=\"~a\">~%" indent lat lon))
    (write-string (format "~a  <ele>~a</ele>\n" indent alt))
    (write-string (format "~a  <time>~a</time>\n" indent (gpx-timestamp timestamp)))
    (write-string (format "~a</trkpt>\n" indent))))

;; Write to (current-output-port) the XML content for a GPX way point
(define (gpx-emit-wpt lat lon alt timestamp name)
  (let ((indent (gpx-indent-string)))
    (write-string (format "~a<wpt lat=\"~a\" lon=\"~a\">~%" indent lat lon))
    (write-string (format "~a  <name>~a</name>\n" indent name))
    (write-string (format "~a  <ele>~a</ele>\n" indent alt))
    (write-string (format "~a  <time>~a</time>\n" indent (gpx-timestamp timestamp)))
    (write-string (format "~a</wpt>\n" indent))))

;; Write to (current-output-port) the lap markers in DF as way points.
(define (gpx-emit-laps df)
  (define laps (or (df-get-property df 'laps) '()))
  (define limit (df-row-count df))
  (unless (null? laps)
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      ;; NOTE: don't emit the first lap, as that coincides with the start of
      ;; the track
      (for ([(lap lap-num) (in-indexed laps)] #:when (> lap-num 0))
        (match-define (vector timestamp lat lon ele)
          (df-lookup df "timestamp" (gpx-export-series) lap))
        (gpx-emit-wpt lat lon ele timestamp (format "Lap ~a" lap-num))))))

;; Write to (current-output-port) the GPS points in DF as a track.  The track
;; is named by the NAME parameter.
;;
;; NAME specifies the name of the track.  If it is #f, the 'name property of
;; DF is consulted and if that one is missing, a default name is used.
(define (gpx-emit-trk df (name #f))
  (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                  (gpx-indent-string (make-string (gpx-indent) #\ )))
    (write-string (format "~a<trk>\n" (gpx-indent-string)))
    (let ((name (or name (df-get-property df 'name) "GPX track")))
      (write-string (format "~a  <name>~a</name>\n" (gpx-indent-string) name)))
    (write-string (format "~a  <trkseg>\n" (gpx-indent-string)))
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      (df-for-each
       df (gpx-export-series)
       (lambda (data)
         (when data
           (match-define (list timestamp lat lon calt) data)
           (when (and lat lon calt timestamp)
             (gpx-emit-trkpt lat lon calt timestamp))))))
    (write-string (format "~a  </trkseg>\n" (gpx-indent-string)))
    (write-string (format "~a</trk>\n" (gpx-indent-string)))))

;; Write the contents of DF in GPX format to the output port OUT.  See
;; `df-write/gpx` for some notes on what is actually written.
(define (write-gpx df out #:name (name #f))
  (unless (df-contains? df "timestamp" "lat" "lon")
    (df-raise "cannot export GPX track -- timestamp or lat or lon series missing"))
  (unless (df-contains/any? df "calt" "alt")
    (df-raise "cannot export GPX track -- altitude series missing"))
  (parameterize ((current-output-port out)
                 (gpx-indent 0)
                 (gpx-indent-string "")
                 (gpx-export-series
                  (cond ((df-contains? df "calt")
                         ;; Prefer corrected altitude
                         (list "timestamp" "lat" "lon" "calt"))
                        ((df-contains? df "alt")
                         ;; Fall back on recorded altitude, if that is
                         ;; available
                         (list "timestamp" "lat" "lon" "alt"))
                        (#t
                         (df-raise "cannot export GPX track -- alt or calt series missing")))))
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (write-string gpx-header-tag)
    (gpx-emit-laps df)
    (gpx-emit-trk df name)
    (write-string "</gpx>\n")))

;; Export the GPS track from the data frame DF to OUT -- which is either an
;; output port or a string, in which case it denotes a file name.  The data
;; frame is expected to contain the "timestamp", "lat", "lon" series, and
;; optionally "alt" or "calt" (corrected altitude) series.
;;
;; The entire GPS track is exported as a single track segment.
;;
;; The 'laps property, if present, is assumed to contain a list of timestamps.
;; The positions corresponding to these timestamps are exported as way points.
;;
;; The name of the segment can be specified as the NAME parameter. If this is
;; #f, the 'name property in the data frame is consulted, if that one is
;; missing a default track name is used.
;;
;; TODO: as an improvement, we could split the track around teleport points
;; into different segments.  This would work nicely when exporting skiing
;; runs.
;;
(define (df-write/gpx df outp #:name (name #f))
  (if (path-string? outp)
      (call-with-output-file outp
        #:mode 'text #:exists 'truncate/replace
        (lambda (o)
          (write-gpx df o #:name name)))
      (write-gpx df outp #:name name)))



;;.......................................................... df-read/gpx ....

;; Read the GPX XML document from the input port IN.  While reading the XML
;; contents, white space is collapsed and comments are skipped.
(define (slurp-xml in)
  (parameterize ((collapse-whitespace #t)
                 (read-comments #f))
    (read-xml/document in)))

(define (get-gpx-tree xml)
  (let ((e (document-element xml)))
    (if (eq? (element-name e) 'gpx)
        e
        (df-raise "not a gpx file"))))

;; Convenience function to check if E is an XML document by NAME
(define (e-name? e name)
  (and (element? e) (eq? (element-name e) name)))

(define (get-track gpx)
  (for/first ([e (element-content gpx)] #:when (e-name? e 'trk))
    e))

(define (get-track-name track)
  (for/first ([e (element-content track)] #:when (e-name? e 'name))
    (pcdata-string
     (for/first ([e (element-content e)] #:when (pcdata? e)) e))))

(define (get-first-track-seg track)
  (for/first ([e (element-content track)] #:when (e-name? e 'trkseg))
    e))

(define (count-track-points track)
  (for/sum ([e (element-content track)] #:when (e-name? e 'trkpt))
    1))

(define (first-trkpt track)
  (for/first ([e (element-content track)] #:when (e-name? e 'trkpt))
    e))

(define (parse-track-point trkpt)
  (let ((lat #f)
        (lon #f)
        (timestamp #f)
        (elevation #f))
    (for ([a (element-attributes trkpt)])
      (case (attribute-name a)
        ((lat) (set! lat (string->number (attribute-value a))))
        ((lon) (set! lon (string->number (attribute-value a))))))
    ;; NOTE: we should extract the timestamp perhaps?  we don't really care
    ;; about it for now...
    (for ([e (element-content trkpt)] #:when (element? e))
      (let ((data (pcdata-string
                   (for/first ([e (element-content e)] #:when (pcdata? e)) e))))
        (case (element-name e)
          ((time) (set! timestamp (date->seconds (string->date data "~Y-~m-~dT~H:~M:~SZ") #f)))
          ((ele) (set! elevation (string->number data))))))
    (list timestamp lat lon elevation)))

(define (parse-way-point wpt)
  (let ((lat #f)
        (lon #f)
        (timestamp #f)
        (elevation #f)
        (name #f))
    (for ([a (element-attributes wpt)])
      (case (attribute-name a)
        ((lat) (set! lat (string->number (attribute-value a))))
        ((lon) (set! lon (string->number (attribute-value a))))))
    ;; NOTE: we should extract the timestamp perhaps?  we don't really care
    ;; about it for now...
    (for ([e (element-content wpt)] #:when (element? e))
      (let ((data (pcdata-string
                   (for/first ([e (element-content e)] #:when (pcdata? e)) e))))
        (case (element-name e)
          ((time) (set! timestamp (date->seconds (string->date data "~Y-~m-~dT~H:~M:~SZ"))))
          ((ele) (set! elevation (string->number data)))
          ((name) (set! name data)))))
    (list timestamp lat lon elevation name)))

(define (parse-all-way-points gpx)
  (for/list ([e (element-content gpx)] #:when (e-name? e 'wpt))
    (parse-way-point e)))

;; Find the timestamp for the DF row that contains the point that is closest
;; to the LAT + LON position.  Note that this is a simplistic method and will
;; produce incorrect results if the GPX track contains loops or it is an "out
;; and back" track.
(define (get-closest-timestamp df lat lon)
  (define-values
    (timestamp distance)
    (for/fold ([timestamp #f] [distance #f])
              (([plat plon ts] (in-data-frame df '("lat" "lon" "timestamp"))))
      (if (and plat plon)
          (let ((dst (map-distance/degrees plat plon lat lon)))
            (if (or (not timestamp) (< dst distance))
                (values ts dst)
                (values timestamp distance)))
          (values timestamp distance))))
  timestamp)

;; Construct a data frame from the GPX document specified as an input port.
;; See `df-read/gpx` for details on what is read from the document.
(define (read-gpx in)
  (define gpx (get-gpx-tree (slurp-xml in)))
  (define track (get-track gpx))
  (unless track (df-raise "could not find track"))
  (define track-segment (get-first-track-seg track))
  (unless track-segment (df-raise "could not find track segment"))
  (define item-count (count-track-points track-segment))
  (define lat (make-vector item-count))
  (define lon (make-vector item-count))
  (define alt (make-vector item-count))
  (define timestamp (make-vector item-count #f))
  (define index 0)
  (for ([e (element-content track-segment)] #:when (e-name? e 'trkpt))
    (match-define (list pt-timestamp pt-lat pt-lon pt-ele) (parse-track-point e))
    (vector-set! timestamp index pt-timestamp)
    (vector-set! lat index pt-lat)
    (vector-set! lon index pt-lon)
    (vector-set! alt index pt-ele)
    (set! index (add1 index)))

  (define timestamp-col (make-series "timestamp" #:data timestamp #:cmpfn <=))
  (define lat-col (make-series "lat" #:data lat))
  (define lon-col (make-series "lon" #:data lon))
  (define alt-col (make-series "alt" #:data alt))
  
  (define df (make-data-frame))
  (df-add-series df timestamp-col)
  (df-add-series df lat-col)
  (df-add-series df lon-col)
  (df-add-series df alt-col)

  ;; Create the "dst" series, this is needed by the trainer application
  (define dst 0)
  (df-add-derived
   df
   "dst"
   '("lat" "lon")
   (lambda (prev next)
     (when (and prev next)
       (match-define (list prev-lat prev-lon) prev)
       (match-define (list next-lat next-lon) next)
       (when (and prev-lat prev-lon next-lat next-lon)
         (set! dst (+ dst (map-distance/degrees prev-lat prev-lon next-lat next-lon)))))
     dst))
  (df-set-sorted df "dst" <=)

  (define track-name (get-track-name track))
  (when track-name
    (df-put-property df 'name track-name))
  (define waypoints (parse-all-way-points gpx))
  ;; Try to reconstruct the 'laps property from the way points -- this will
  ;; not work very well if the way points don't have a timestamp and the GPX
  ;; track contains loops or it is an "out and back" track.
  (define laps
    (for/list ([wpt (in-list waypoints)])
      (match-define (list timestamp lat lon elevation name) wpt)
      (or timestamp (get-closest-timestamp df lat lon))))
  (df-put-property df 'waypoints waypoints)
  (df-put-property df 'laps (filter values laps))

  df)

;; Construct a data frame from the GPX document specified in INP -- which is
;; either an input port or a string, in which case it denotes an input file.
;; The data frame will have "timestamp", "lat", "lon", "alt", "dst" and
;; "grade" series (the last two are computed.  See doc/session-df.md for the
;; meaning of these series.
;;
;; The data frame will also have the following properties:
;; 
;; * a 'name property containing the "NAME" of the track segment, if this is
;;   present in the GPX file.
;;
;; * a 'waypoints property containing a list of waypoints, if they GPX track
;;   has any.  Each waypoint is represented as a list of TIMESTAMP, LAT, LON,
;;   ELEVATION and NAME
;;
;; * a 'laps property containing a list of timestamps corresponding to each
;;   way point in the waypoint list -- the laps property cannot be constructed
;;   correctly if the waypoints are missing a timestamp property.
;;
;; Only the first track segment in the GPX file will be read.
(define (df-read/gpx inp)
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-gpx i)))
      (read-gpx inp)))


;;............................................................. provides ....

(provide/contract
 (df-write/gpx (->* (data-frame? (or/c path-string? output-port?))
                    (#:name (or/c #f string?))
                    any/c))
 (df-read/gpx (-> (or/c path-string? input-port?) data-frame?)))
