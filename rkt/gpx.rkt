#lang racket/base
;; gpx.rkt -- export session data frames to GPX format
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017 Alex Harsanyi (AlexHarsanyi@gmail.com)
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
         racket/class
         racket/format
         racket/match
         racket/date
         xml
         (only-in srfi/19 string->date)
         "data-frame.rkt"
         "session-df.rkt"               ; for add-grade-series
         "map-util.rkt"                 ; to calculate the "dst" series
         )

(provide/contract
 (df-write/gpx (->* ((is-a?/c data-frame%) output-port?) (#:name (or/c #f string?)) any/c))
 (df-read/gpx (-> input-port? (is-a?/c data-frame%))))

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
  (define laps (send df get-property 'laps))
  (unless (null? laps)
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      ;; NOTE: don't emit the first lap, as that coincides with the start of
      ;; the track
      (for ([(lap lap-num) (in-indexed laps)] #:when (> lap-num 0))
        (let ((index (send df get-index "timestamp" lap)))
          (when index
            (match-define (vector timestamp lat lon ele)
              (send/apply df ref* index (gpx-export-series)))
            (gpx-emit-wpt lat lon ele timestamp (format "Lap ~a" lap-num))))))))

;; Write to (current-output-port) the GPS points in DF as a track.  The track
;; is named by the NAME parameter.
;;
;; NAME specifies the name of the track.  If it is #f, the 'name property of
;; DF is consulted and if that one is missing, a default name is used.
(define (gpx-emit-trk df (name #f))
  (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                  (gpx-indent-string (make-string (gpx-indent) #\ )))
    (write-string (format "~a<trk>\n" (gpx-indent-string)))
    (let ((name (or name (send df get-property 'name) "GPX track")))
      (write-string (format "~a  <name>~a</name>\n" (gpx-indent-string) name)))
    (write-string (format "~a  <trkseg>\n" (gpx-indent-string)))
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      (send df for-each (gpx-export-series)
            (lambda (data)
              (when data
                (match-define (vector timestamp lat lon calt) data)
                (when (and lat lon calt timestamp)
                  (gpx-emit-trkpt lat lon calt timestamp))))))
    (write-string (format "~a  </trkseg>\n" (gpx-indent-string)))
    (write-string (format "~a</trk>\n" (gpx-indent-string)))))

;; Export the GPS track from the data frame DF to the output port OUT.  The
;; data frame is expected to contain the "timestamp", "lat", "lon" and
;; optionally "alt" or "calt" (corrected altitude) series.
;;
;; The data is exported in GPX format. Positions in the 'laps property are
;; exported as way points and the entire GPS track is exported as a single
;; track segment.  The name of the segment can be specified as the NAME
;; parameter. If this is #f, the 'name property in the data frame is
;; consulted, if that one is missing a default track name is used.
;;
;; TODO: as an improvement, we could split the track around teleport points
;; into different segments.  This would work nicely when exporting skiing
;; runs.
;;
(define (df-write/gpx df out #:name (name #f))
  (unless (send df contains? "timestamp" "lat" "lon")
    (error "cannot export GPX track -- timestamp or lat or lon series missing"))
  (unless (send df contains/any? "calt" "alt")
    (error "cannot export GPX track -- altitude series missing"))
  (parameterize ((current-output-port out)
                 (gpx-indent 0)
                 (gpx-indent-string "")
                 (gpx-export-series
                  (cond ((send df contains? "calt")
                         ;; Prefer corrected altitude
                         (list "timestamp" "lat" "lon" "calt"))
                        ((send df contains? "alt")
                         ;; Fall back on recorded altitude, if that is
                         ;; available
                         (list "timestamp" "lat" "lon" "alt"))
                        (#t
                         (error "cannot export GPX track -- alt or calt series missing")))))
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (write-string gpx-header-tag)
    (gpx-emit-laps df)
    (gpx-emit-trk df name)
    (write-string "</gpx>\n")))


;;.................................................... reading gpx files ....

;; Read the GPX XML document from the input port IN.  While reading the XML
;; contents, white space is collapsed and comments are skipped.
(define (read-gpx in)
  (parameterize ((collapse-whitespace #t)
                 (read-comments #f))
    (read-xml/document in)))

(define (get-gpx-tree xml)
  (let ((e (document-element xml)))
    (if (eq? (element-name e) 'gpx)
        e
        (error "not a gpx file"))))

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
          ((time) (set! timestamp (date->seconds (string->date data "~Y-~m-~dT~H:~M:~SZ"))))
          ((ele) (set! elevation (string->number data))))))
    (list timestamp lat lon elevation)))

;; Construct a data frame from the GPX document specified as an input port.
;; The data frame will have "timestamp", "lat", "lon", "alt", "dst" and
;; "grade" series (the last two are computed.  See doc/session-df.md for the
;; meaning of these series.  The data frame will also have a 'name property
;; containing the "NAME" of the track segment, if this is present in the GPX
;; file.
;;
;; LIMITATIONS:
;;
;;  * only the first track segment is read
;;  * way-points are not read (they could be stored in the 'laps property)
;;
;; HINT: to read the GPX from a file name use:
;;
;;     (call-with-input-file FILE-NAME df-read/gpx)
;;
(define (df-read/gpx in)
  (define gpx (get-gpx-tree (read-gpx in)))
  (define track (get-track gpx))
  (unless track (error "could not find track"))
  (define track-name (get-track-name track))
  (define track-segment (get-first-track-seg track))
  (unless track-segment (error "could not find track segment"))
  (define item-count (count-track-points track-segment))
  (define lat (make-vector item-count))
  (define lon (make-vector item-count))
  (define alt (make-vector item-count))
  (define timestamp (make-vector item-count))
  (define index 0)
  (for ([e (element-content track-segment)]
        #:when (and (element? e) (eq? (element-name e) 'trkpt)))
    (match-define (list pt-timestamp pt-lat pt-lon pt-ele) (parse-track-point e))
    (vector-set! timestamp index pt-timestamp)
    (vector-set! lat index pt-lat)
    (vector-set! lon index pt-lon)
    (vector-set! alt index pt-ele)
    (set! index (add1 index)))
  (define df
    (new data-frame%
         [series
          (list
           (new data-series% [name "timestamp"] [data timestamp])
           (new data-series% [name "lat"] [data lat])
           (new data-series% [name "lon"] [data lon])
           (new data-series% [name "alt"] [data alt]))]))
  (when track-name
    (send df put-property 'name track-name))
  ;; Create the "dst" series, this is needed by the trainer application
  (define dst 0)
  (send df add-derived-series
        "dst"
        '("lat" "lon")
        (lambda (prev next)
          (when (and prev next)
            (match-define (vector prev-lat prev-lon) prev)
            (match-define (vector next-lat next-lon) next)
            (when (and prev-lat prev-lon next-lat next-lon)
              (set! dst (+ dst (map-distance/degrees prev-lat prev-lon next-lat next-lon)))))
          dst))
  (send (send df get-series "dst") set-sorted #t)
  ;; Add the "grade" series, this is needed by the trainer application
  (add-grade-series df)
  df)
