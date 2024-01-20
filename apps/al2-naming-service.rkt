#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; al2-naming-service.rkt --
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require
 json
 geoid
 racket/match
 math/statistics
 data-frame
 data-frame/gpx
 geoid
 geoid/waypoint-alignment
 racket/port
 racket/vector
 racket/math
 racket/format
 web-server/servlet
 web-server/servlet-env
 web-server/dispatch
 web-server/http/json)


;;................................................... program parameters ....

;; Application key -- silly way to "protect" this service API
(define app-key (make-parameter "abc"))

(define routes (make-parameter (hash)))

;; Parameter controlling how much we reduce the number of waypoints in tracks
;; for running a DTW algorithm on them using `waypoint-alignment-cost` --
;; smaller number of waypoints make the matching run faster.
(define track-simplification-threshold (make-parameter 10.0))

(define cities (make-parameter #f))
(define city-threshold (make-parameter 50000))
(define user-locations (make-parameter #f))
(define user-location-threshold (make-parameter 500))


;;................................................... logging facilities ....

;; Internal logger that `log` sends messages to -- this ensures logging works
;; correctly when invoked from multiple threads.
(define al2ns-logger (make-logger 'al2ns-logger #f))

(define (log fmtstring . args)
  (define msg (apply format fmtstring args))
  (log-message al2ns-logger 'info 'al2ns-logger msg #f #f))

(define (elog fmtstring . args)
  (define msg (apply format fmtstring args))
  (log-message al2ns-logger 'error 'al2ns-logger msg #f #f))

;; Return the current timestamp as a string.  Includes milliseconds.  It is
;; used to put timestamps in the log messages.
(define (get-current-timestamp)

  (define (fmt val width)
    (~a val #:width width #:align 'right #:pad-string "0"))

  (let ((ts (exact-round (current-inexact-milliseconds))))
    (let-values (([q r] (quotient/remainder ts 1000)))
      (let ((date (seconds->date q)))
        (string-append
         (fmt (date-year date) 4)
         "-"
         (fmt (date-month date) 2)
         "-"
         (fmt (date-day date) 2)
         " "
         (fmt (date-hour date) 2)
         ":"
         (fmt (date-minute date) 2)
         ":"
         (fmt (date-second date) 2)
         "."
         (fmt r 3))))))

(define (start-logging-thread log-file)
  (let (;; This is the output port we log messages to.  This file will be closed
        ;; when the current custodian shuts down the application.
        [output (if log-file
                    (open-output-file log-file #:mode 'text #:exists 'append)
                    (current-output-port))])

    ;; Register a flush callback with the current plumber, so we flush our log
    (plumber-add-flush!
     (current-plumber)
     (lambda (_handle)
       (flush-output output)))

    ;; In a separate thread, receive messages from the al2-logger and write them
    ;; out to the output port, keeping track of duplicate messages.
    (thread
     (lambda ()
       (let ([receiver (make-log-receiver al2ns-logger 'info)])
         (let loop ([item (sync receiver)])
           (define level (string-upcase (symbol->string (vector-ref item 0))))
           (define message (vector-ref item 1))
           (write-string (get-current-timestamp) output)
           (write-string (format " ~a: ~a~%" level message) output)
           (flush-output output)
           (loop (sync receiver))))))

    ;; Return void so we don't print the thread id to stdout
    (void)))


;; Remove waypoints from track0 up to `threshold` -- waypoints between two
;; points p1 and p2 are removed as long as the direct distance between p1 and
;; p2 is within `threshold` from the distance along the path of all waypoints
;; between p1 ... p2.  That is, if the path p1, p2, p3, p4 is mostly a
;; straight line than p2 an p3 can be removed.  This does not affect the DTW
;; alignment cost algorithm, which looks at the "shape" of the path.
(define (simplify-track track0 #:threshold (threshold (track-simplification-threshold)))
  ;; We'll do in-place track simplification
  (define track (vector-copy track0))
  (define sentinel (sentinel-geoid))
  (define (tref index)
    (vector-ref track index))
  (define (tset index value)
    (vector-set! track index value))
  (define limit (sub1 (vector-length track0)))
  (let loop ([segment-start 0]
             [start-geoid (tref 0)]
             [segment-length (distance-between-geoids (tref 0) (tref 1))]
             [current-index 1])
    (when (< current-index limit)
      (define current-geoid (tref current-index))
      (define arc-length (distance-between-geoids start-geoid current-geoid))
      (if (< (abs (- arc-length segment-length)) threshold)
          (let ([dt (distance-between-geoids (tref current-index) (tref (add1 current-index)))])
            (tset current-index sentinel)
            (loop segment-start start-geoid (+ segment-length dt) (add1 current-index)))
          (loop current-index current-geoid 0 (add1 current-index)))))
  (for/vector ([g (in-vector track)] #:unless (equal? g sentinel))
    g))

;; Reverse a vector, there does not seem to be a built-in function for this?
(define (vreverse v)
  (for/vector #:length (vector-length v)
              ((x (in-vector v (sub1 (vector-length v)) -1 -1)))
    x))

;; Read a route from a GPX file and produce two route entries, one for the
;; direct route and one for the reverse route.  Each entry is a hash table
;; containing the name, waypoints and other parameters used to calculate a
;; "cost benchmark", which is used in determining if a
;; `waypoint-alignment-cost` is good enough for the route to be a match.
(define (read-route-from-file file-name)
  (define df (df-read/gpx file-name))
  (df-add-derived!
   df
   "geoid"
   '("lat" "lon")
   (lambda (v)
     (match-define (list lat lon) v)
     (and lat lon (lat-lng->geoid lat lon))))
  (define waypoints (df-select df "geoid" #:filter valid-only))
  (define s (df-statistics df "dst"))
  (define benchmark (/ (statistics-max s) (statistics-count s)))
  (define name (df-get-property df 'name))

  ;; Simplify the waypoints in the route -- smaller number of waypoints make
  ;; `waypoint-alignment-cost` run faster, without (much) loss in prediction
  ;; accuracy.  We simplify both forward and reverse, and pick the one with
  ;; the smaller number of waypoints.
  (define-values (fwd rev)
    (let ([sfwd (simplify-track waypoints)]
          [srev (simplify-track (vreverse waypoints))])
      (if (< (vector-length sfwd) (vector-length srev))
          (values sfwd (vreverse sfwd))
          (values (vreverse srev) srev))))

  (log "Loaded ~a, waypoint reduction ~a%"
       file-name
       (exact-round (* 100.0 (/ (vector-length fwd) (vector-length waypoints)))))

  (values
   (hash
   'name name
   'waypoints fwd
   'benchmark benchmark
   'distance (statistics-max s)
   'point-count (statistics-count s))

   (hash
   'name (string-append name " (in reverse)")
   'waypoints rev
   'benchmark benchmark
   'distance (statistics-max s)
   'point-count (statistics-count s))))


;; Load all routes (GPX files) from directory DIR and produce a list of route
;; entries (as returned by `read-route-from-file`
(define (load-routes-from-directory dir)
  (for/fold ([routes '()])
            ([file-name (in-directory dir)]
             #:when (and (file-exists? file-name)
                         (regexp-match #rx"\\.gpx$" file-name)))
    (define-values (fwd rev) (read-route-from-file file-name))
    (cons fwd (cons rev routes))))

;; Return true if the start point for TRACK is close to the start point for
;; ROUTE.  "close" means the distance is less than THRESHOLD.
(define (track-starts-close-by? track route #:threshold (threshold 3000))
  (define route-start (vector-ref (hash-ref route 'waypoints) 0))
  (define track-start (vector-ref track 0))
  (< (distance-between-geoids route-start track-start) threshold))

;; Determine the benchmark value for a TRACK (a list of GEOIDS), the benchmark
;; is compared against the `waypoint-alignment-cost` result to determine if
;; the match is good or not.
(define (track-benchmark track)
  (define total-distance
    (for/sum ([p1 (in-vector track)]
              [p2 (in-vector track 1)])
      (distance-between-geoids p1 p2)))
  (values total-distance (/ total-distance (vector-length track))))

;; Calculate a benchmark value for the match between TRACK and ROUTE.  A value
;; of less than 1.0 indicates that the TRACK follows ROUTE very closely, a
;; value greater than 1.0 indicates that it does less so.  The greater the
;; number returned, the less likely it is that the TRACK is actually following
;; the ROUTE.
(define (route-match-benchmark track track-benchmark route)

  ;; The waypoint alignment cost is smaller the closer the two tracks are,
  ;; unfortunately, this cost is also affected by the number of waypoints and
  ;; the distance between these waypoints, so we attempt to normalize this
  ;; cost to produce a value that only depends on how close the two tracks
  ;; are...
  (define cost
    (waypoint-alignment-cost
     (hash-ref route 'waypoints)
     track))

  (define normalized
    (/ cost (max (hash-ref route 'point-count) (vector-length track))))

  (define benchmark
    (max (hash-ref route 'benchmark) track-benchmark))

  (/ normalized (* 2.0 benchmark)))

;; return a list of routes and their benchmark, i.e. how closely they match
;; track.  Only routes for which `track-starts-close-by?` returns #t are
;; included.
(define (find-route-benchmarks track routes)
  (define-values (len benchmark) (track-benchmark track))
  (define candidates
    (for/list ([route (in-list routes)]
               #:when (and (track-starts-close-by? track route)
                           (< (abs (- len (hash-ref route 'distance))) 5000)))
      (list (hash-ref route 'name)
            (route-match-benchmark track benchmark route))))
  (sort candidates < #:key cadr))

;; Find the closest city to the start location of track, but only if it is
;; closer than THRESHOLD.
(define (closest-city track worldcities #:threshold (threshold 50000))
  (define g (vector-ref track 0))
  (define-values (city distance)
    (for/fold ([city #f]
               [distance +inf.0])
              ([(c lat lon) (in-data-frame worldcities "city" "lat" "lng")])
      (define d (distance-between-geoids g (lat-lng->geoid lat lon)))
      (if (< d distance)
          (values c d)
          (values city distance))))
  (if (< distance threshold)
      (hash 'type "city"
            'name city
            'benchmark (/ distance threshold))
      #f))

(define (closest-user-location track user-locations #:threshold (threshold 500))
  (define g (vector-ref track 0))
  (define-values (user-location distance)
    (for/fold ([user-location #f]
               [distance +inf.0])
              ([(u lat lon) (in-data-frame user-locations "name" "lat" "lng")])
      (define d (distance-between-geoids g (lat-lng->geoid lat lon)))
      (if (< d distance)
          (values u d)
          (values user-location distance))))
  (if (< distance threshold)
      (hash 'type "user-location"
            'name user-location
            'benchmark (/ distance threshold))
      #f))

(define (determine-track-name/route track)
  (define bms (find-route-benchmarks track (routes)))
  (if (null? bms)
      #f
      (match-let ([(list route benchmark) (car bms)])
        (cond ((<= benchmark 1.0)
               (hash 'type "route"
                     'name route
                     'benchmark benchmark))
              ((< benchmark 1.5)
               (hash 'type "route"
                     'name (string-append "Maybe " route)
                     'benchmark benchmark))
              (else
               #f)))))

(define (determine-track-name/user-location track)
  (and (user-locations)
       (closest-user-location (user-locations) #:threshod (user-location-threshold))))

(define (determine-track-name/city track)
  (and (cities)
       (closest-city (cities) #:threshod (city-threshold))))

;; Try to determine a track name based on our loaded routes and loaded cities.
(define (determine-track-name track)
  (or (determine-track-name/route track)
      (determine-track-name/user-location track)
      (determine-track-name/city track)))

(define (report-404 req)
  (response/empty #:code 404))

(define (report-400 req)
  (response/empty #:code 400))

(define (request-data->geoids data)
  (define json
    (call-with-input-bytes
     data
     (lambda (in)
       (read-json in))))
  (unless (list? json)
    (error 'request-data->geoids "not a list"))
  (define geoids
    (for/vector ([item (in-list json)])
      (unless (list? item)
        (error 'request-data->geoids "not a hash: ~a" item))
      (match-define (list lat lon) item)
      (unless (and (rational? lat) (rational? lon))
        (error 'request-data->geoids "latitude or longitude not rationals: ~a, ~a" lat lon))
      (lat-lng->geoid lat lon)))
  geoids)

(define (name-for-route req akey)
  (if (equal? akey (app-key))
      (let ([data (request-data->geoids (request-post-data/raw req))])
        (define data0 (simplify-track data))
        (log "Got request, track length ~a, simplified to ~a, reduced to ~a%"
             (vector-length data)
             (vector-length data0)
             (exact-round (* 100 (/ (vector-length data0) (vector-length data)))))
        (define result (determine-track-name data0))
        (log "Result: ~a"
             (call-with-output-string (lambda (o) (write-json result o))))
        (collect-garbage 'major)
        (response/jsexpr result))
      (report-400 req)))

(define-values (app-dispatch app-url)
  (dispatch-rules
   [("app" (string-arg) "route-name") #:method "post" name-for-route]
   [else report-404]))

(module+ main
  (require racket/cmdline)

  ;; File name we append log messages to.  If #f, messages are printed to stdout
  (define log-file-name (make-parameter #f))

  ;; The configuration file from where we read parameters
  (define config-file-name (make-parameter #f))

  (command-line
   #:program "al2-naming-service"
   #:once-each
   [("-c" "--config-file")
    config
    "Configuration file for the naming service"
    (unless (file-exists? config)
      (error (format "configuration file does not exist: ~a" config)))
    (config-file-name config)])

  (define config
    (with-handlers
      ((exn? (lambda (e)
               (error (format "bad config file (~a): ~a" (config-file-name) e)))))
      (call-with-input-file (config-file-name) read-json)))

  (log-file-name (hash-ref config 'log_file #f))
  (unless (or (equal? #f (log-file-name)) (path-string? (log-file-name)))
    (error (format "bad log file name (not a string): ~a" (log-file-name))))
  (start-logging-thread (log-file-name))

  (app-key (hash-ref config 'app_key #f))

  (define service-port (hash-ref config 'port 8080))
  (unless (integer? service-port)
    (error (format "Service port is not a number: ~a~%" service-port)))

  (define the-routes
    (let ([courses-directory (hash-ref config 'courses_directory #f)])
      (if courses-directory
          (if (and (path-string? courses-directory)
                   (directory-exists? courses-directory))
              (begin
                (log "Reading routes...")
                (load-routes-from-directory courses-directory))
              (begin
                (log "Bad courses-directory: ~a" courses-directory)
                (hash)))
          (hash))))
  (routes the-routes)

  (let ([ct (hash-ref config 'track_simplification_threshold #f)])
    (when ct
      (unless (rational? ct)
        (log "Bad track_simplification_threshold, ignoring: ~a" ct))
      (track-simplification-threshold ct)))

  (define the-cities
    (let ([cities-file (hash-ref config 'cities-file #f)])
      (if cities-file
          (if (and (path-string? cities-file)
                   (file-exists? cities-file))
              (begin
                (log "Reading world cities...")
                (let ([df (df-read/csv cities-file #:quoted-numbers? #t)])
                  (for ([s (df-series-names df)]
                        #:unless (member s '("city" "lat" "lon")))
                  (df-del-series! df s)
                  df)))
              (begin
                (log "Bad cities-file: ~a" cities-file)
                #f))
          #f)))
  (cities the-cities)

  (let ([ct (hash-ref config 'city-threshold #f)])
    (when ct
      (unless (rational? ct)
        (log "Bad city_threshold, ignoring: ~a" ct))
      (city-threshold ct)))

  (define the-user-locations
    (let ([user-locations-file (hash-ref config 'user-locations-file #f)])
      (if user-locations-file
          (if (and (path-string? user-locations-file)
                   (file-exists? user-locations-file))
              (begin
                (log "Reading user locations...")
                (let ([df (df-read/csv user-locations-file #:quoted-numbers? #t)])
                  (for ([s (df-series-names df)]
                        #:unless (member s '("name" "lat" "lon")))
                  (df-del-series! df s)
                  df)))
              (begin
                (log "Bad user location file: ~a" user-locations-file)
                #f))
          #f)))

  (user-locations the-user-locations)

  (let ([ct (hash-ref config 'user_location_threshold #f)])
    (when ct
      (unless (rational? ct)
        (log "Bad user_location_threshold, ignoring: ~a" ct))
      (user-location-threshold ct)))

  (collect-garbage 'major)
  (log "Finished loading data")

  (serve/servlet app-dispatch
                 #:port service-port
                 #:banner? #f
                 #:launch-browser? #f
                 #:extra-files-paths '()
                 #:servlet-regexp #rx""))
