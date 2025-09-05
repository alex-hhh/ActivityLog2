#lang racket/base
;; SPDX-License-Identifier: GPL-3.0-or-later
;; gps-segment-test.rkt -- tests for the GPS Segments functionality
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2021, 2022, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require al2-test-runner
         rackunit
         data-frame
         data-frame/gpx
         db
         racket/match
         "../rkt/utilities.rkt"
         "test-util.rkt"
         "../rkt/gps-segments/gps-segments.rkt"
         "../rkt/session-df/session-df.rkt"
         "../rkt/database.rkt"
         (only-in "../rkt/import.rkt" do-post-import-tasks))

(set-dbglog-to-standard-output #t)     ; send dbglog calls to stdout, so we can see them!
;; Use 1 worker thread, so we can determine when tasks finish (See
;; `do-tc-check`)
(set-worker-thread-count 1)

;; Verify the sanity of the ecc.gpx segment
(define (check-ecc-segment segment)
  (check > (df-row-count segment) 0)
  (check-true (df-contains? segment "lat" "lon" "geoid" "dst" "alt" "grade"))
  (check-pred number? (df-get-property segment 'segment-length #f))
  (check-pred number? (df-get-property segment 'segment-height #f))
  (check-pred number? (df-get-property segment 'segment-grade #f))
  (check-pred number? (df-get-property segment 'total-ascent #f))
  (check-pred number? (df-get-property segment 'total-descent #f))
  (check-pred number? (df-get-property segment 'max-grade #f))
  (check-pred number? (df-get-property segment 'min-elevation #f))
  (check-pred number? (df-get-property segment 'max-elevation #f))
  (check-pred number? (df-get-property segment 'fiets-score #f)))

;; Verify the sanity of the ecc-no-alt.gpx segment
(define (check-ecc-no-alt-segment segment)
  (check > (df-row-count segment) 0)
  (check-true (df-contains? segment "lat" "lon" "geoid" "dst"))
  (check-false (df-contains/any? segment "alt" "grade"))
  (check-pred number? (df-get-property segment 'segment-length #f))
  (check-false (df-get-property segment 'segment-height #f))
  (check-false (df-get-property segment 'segment-grade #f))
  (check-false (df-get-property segment 'total-ascent #f))
  (check-false (df-get-property segment 'total-descent #f))
  (check-false (df-get-property segment 'max-grade #f))
  (check-false (df-get-property segment 'min-elevation #f))
  (check-false (df-get-property segment 'max-elevation #f))
  (check-false (df-get-property segment 'fiets-score #f)))

(define gps-segments-test-suite
  (test-suite
   "GPS Segments"
   (test-case "get/put segment (ecc)"
     (define gpx-file "./test-data/ecc.gpx")
     (define raw-gpx (df-read/gpx gpx-file))
     (define segment (gps-segment-from-gpx gpx-file))
     ;; Reading the GPX file as a segment should have interpolated points to
     ;; not have big distances between adjacent data points
     (check > (df-row-count segment) (df-row-count raw-gpx))
     (check-ecc-segment segment)
     (with-fresh-database
       (lambda (db)
         (define segment-id (put-new-gps-segment db segment))
         (check = (query-value db "select count(*) from GPS_SEGMENT") 1)
         (check = (query-value db "select count(*) from GPS_SEGMENT_WAYPOINT where segment_id = ?" segment-id)
                (df-row-count segment))
         (define nsegment (fetch-gps-segment db segment-id))
         (check-ecc-segment nsegment)
         (delete-gps-segment db segment-id)
         (check = (query-value db "select count(*) from GPS_SEGMENT") 0)
         (check = (query-value db "select count(*) from GPS_SEGMENT_WAYPOINT") 0))))

   (test-case "get/put segment (ecc-no-alt)"
     (define gpx-file "./test-data/ecc-no-alt.gpx")
     (define raw-gpx (df-read/gpx gpx-file))
     (define segment (gps-segment-from-gpx gpx-file))
     ;; Reading the GPX file as a segment should have interpolated points to
     ;; not have big distances between adjacent data points
     (check > (df-row-count segment) (df-row-count raw-gpx))
     (check-ecc-no-alt-segment segment)
     (with-fresh-database
       (lambda (db)
         (define segment-id (put-new-gps-segment db segment))
         (check = (query-value db "select count(*) from GPS_SEGMENT") 1)
         (check = (query-value db "select count(*) from GPS_SEGMENT_WAYPOINT where segment_id = ?" segment-id)
                (df-row-count segment))
         (define nsegment (fetch-gps-segment db segment-id))
         (check-ecc-no-alt-segment nsegment)
         (delete-gps-segment db segment-id)
         (check = (query-value db "select count(*) from GPS_SEGMENT") 0)
         (check = (query-value db "select count(*) from GPS_SEGMENT_WAYPOINT") 0))))

   (test-case "segment match"
     (define segment (gps-segment-from-gpx "./test-data/ecc-no-alt.gpx"))
     (define first-geoid (df-ref segment 0 "geoid"))
     (with-fresh-database
       (lambda (db)
         (db-import-activity-from-file "./test-data/310xt-run.fit" db)
         (define matching-sessions (find-nearby-sessions db first-geoid))
         (check = (length matching-sessions) 1)
         (define waypoints (df-select segment "geoid"))
         (define segment-length (df-get-property segment 'segment-length))
         (define df (session-df db (car matching-sessions)))
         (define matches (find-segment-matches df waypoints segment-length))
         (check = (length matches) 1)   ; one single match should be found

         ;; Store everything in the database
         (define segment-id (put-new-gps-segment db segment))
         (match-define (list _df start stop cost) (car matches))
         (define match-id (put-new-segment-match db segment-id df start stop cost))

         ;; 1 Match should be stored in the database
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 1)
         (delete-segment-match db match-id)
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 0)
         (check-pred null? (leaked-section-summaries db)
                     "Leaking SECTION_SUMMARY entries after deleting segment match")

         (define segment-id2 (put-new-gps-segment db segment))
         (define match-id2 (put-new-segment-match db segment-id2 df start stop cost))
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 1)
         (delete-gps-segment db segment-id2)
         ;; Match should be gone after deleting the segment
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 0)


         (define segment-id3 (put-new-gps-segment db segment))
         (define match-id3 (put-new-segment-match db segment-id3 df start stop cost))
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 1)
         (db-delete-session (df-get-property df 'session-id #f) db)
         ;; Match should be gone after deleting the session
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 0)

         )))

   (test-case "segment match on import"
     (define segment (gps-segment-from-gpx "./test-data/ecc.gpx"))
     (check-ecc-segment segment)
     (with-fresh-database
       (lambda (db)
         (define segment-id (put-new-gps-segment db segment))
         (db-import-activity-from-file "./test-data/310xt-run.fit" db)
         (do-post-import-tasks db) ; this should find a match on the segment we imported
         (check = (query-value db "select count(*) from GPS_SEGMENT_MATCH") 1))))

   ))


(module+ test
  (run-tests #:package "gps-segments-test"
             #:results-file "test-results/gps-segments-test.xml"
             gps-segments-test-suite))
