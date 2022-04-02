#lang racket/base
;; weather.rkt -- fetch weather data from wunderground
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019, 2020, 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require (for-syntax racket/base)
         db/base
         json
         map-widget/utils
         net/url
         net/url-connect
         racket/contract
         racket/date
         racket/format
         racket/list
         racket/math
         racket/port
         racket/runtime-path
         racket/string
         "dbutil.rkt"
         "utilities.rkt")


;;........................................................... data types ....

;; Weather observation
(struct wobs (ts temp dewp hum wspd wgust wdir pressure)
        #:transparent
        #:guard
        (lambda (ts temp dewp hum wspd wgust wdir pressure name)
          ;; WOBS retrieved from the cache contain invalid values, replace
          ;; them with #f, this is a bit of a hack.
          (values
           ts
           (if (and (number? temp) (> temp -1000)) temp #f)
           (if (and (number? dewp) (> dewp -1000)) dewp #f)
           (if (and (number? hum) (>= hum 0) (<= hum 100)) hum #f)
           (if (and (number? wspd) (>= wspd 0)) wspd #f)
           (if (and (number? wgust) (>= wgust 0)) wgust #f)
           wdir
           pressure)))




;;............................................ DS Json Response Parsing ....

;; read a weather observation from NODE, a json node
(define (json->wobs node)
  (let ((timestamp (hash-ref node 'time #f))
        (temp (hash-ref node 'temperature #f))
        (dewp (hash-ref node 'dewPoint #f))
        (hum (hash-ref node 'humidity #f))
        (wspd (hash-ref node 'windSpeed #f))
        (wgust (hash-ref node 'windGust #f))
        (wdir (hash-ref node 'windBearing #f))
        (pres (hash-ref node 'pressure #f)))
    (wobs
     timestamp
     temp
     dewp
     (and hum (* hum 100.0))            ; convert to 0..100
     wspd
     wgust
     wdir
     pres)))


;;...................................................... DS API requests ....

(define (make-request-limiter num-requests time-period)
  (let ((past-requests '()))
    (lambda ()
      (define now (current-inexact-milliseconds))
      (define old-timestamp (- now (* time-period 1000)))
      (define trimed-requests (takef past-requests (lambda (t) (> t old-timestamp))))
      (when (>= (length trimed-requests) num-requests)
        (define sleep-time (/ (- (last trimed-requests) old-timestamp) 1000))
        ;; Put something in the log when we are about to sleep for long
        ;; periods of time -- this will happen if large amounts of activities
        ;; are imported, or unit tests are run...
        (when (> sleep-time 10)
          (dbglog "weather request limiter, about to sleep for ~a seconds"
                  (~r sleep-time #:precision 2)))
        (sleep sleep-time))
      (set! past-requests (cons now trimed-requests)))))

;; Weather requests not work if the internet access needs to be done via a
;; proxy or there is no internet connnection.  This will prevent the
;; downloader from even trying.
(define allow-weather-download-tag 'activity-log:allow-weather-download)
(define allow-weather-download-val (get-pref allow-weather-download-tag (lambda () #t)))
(define (allow-weather-download) allow-weather-download-val)
(define (set-allow-weather-download new-val)
  ;; Write the value back to the store
  (put-pref allow-weather-download-tag new-val)
  (set! allow-weather-download-val new-val)
  (if new-val
      (dbglog "weaher data download enabled")
      (dbglog "weather data download disabled")))


;; This "magic" macro will embed the API key at compile time, allowing us to
;; ship a built version of the application with the API key already inside it.
(define-syntax (embedded-api-key stx)
  (syntax-case stx ()
    [_ #`(quote #,(getenv "AL2DSAPIKEY"))]))
(define (builtin-api-key) (embedded-api-key))

(define ds-api-key-tag 'activity-log:ds-api-key)
;; the API key value comes either from the preferences file (needs to be
;; manually stored there), from an environment variable, or a built in one (if
;; available)
(define ds-api-key-val
  (or (get-pref ds-api-key-tag (lambda () #f))
      (getenv "AL2DSAPIKEY")
      (builtin-api-key)))
(define (ds-api-key) ds-api-key-val)

(define ds-api-url "https://api.darksky.net/forecast")

;; NOTE: don't put the URL in the exception, as this will leak the API key!
(struct exn:fail:wufetch exn:fail:network (json)
  #:extra-constructor-name make-exn:fail:wufetch
  #:transparent)

(define (raise-wufetch-error message data)
  (raise (make-exn:fail:wufetch message (current-continuation-marks) data)))

;; Wrap thunk in exception handlers, log any exceptions and re-throw as
;; necessary
(define (with-wu-handlers thunk)
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       ;; First, put the actual error to the log file
       (dbglog-exception "ds-fetch-json" e)
       (cond
         ((exn:fail:network? e)
          (error "Network error while fetching weather"))
         ((exn:fail:wufetch? e)
          (error "Error reply from weather server"))
         ((exn:fail? e)
          (raise e))                    ; raise it again
         (#t
          (error "Unknown error while fetching weather data"))))))
   (thunk)))

(define (ds-fetch-json url)
  ;; Normally, these two conditions should not be encountered.
  (unless (ds-api-key) (error "no DarkSky.net api key set"))
  (unless (allow-weather-download) (error "weather download not permitted"))
  (with-wu-handlers
    (lambda ()
      (parameterize ((current-https-protocol 'secure))
        (let* ((data (port->string (get-pure-port (string->url url))))
               (json (call-with-input-string data read-json)))
          (unless json
            (raise-wufetch-error "Bad DarkSky.net reply" json))
          json)))))


;;....................................................... Weather Lookup ....

(define (can-do-web-requests?)
  (and (allow-weather-download) (ds-api-key)))

(define (get-session-weather-lookup-data db sid)
  (let ((row (query-row db "
 select S.start_time,
        SS.total_elapsed_time,
       max(T.position_lat) as max_lat,
       max(T.position_long) as max_lon,
       min(T.position_lat) as min_lat,
       min(T.position_long) as min_lon
  from A_TRACKPOINT T, A_LENGTH LN, A_LAP LP,
       A_SESSION S, SECTION_SUMMARY SS
 where S.id = ?
   and S.summary_id = SS.id
   and LP.session_id = S.id
   and LN.lap_id = LP.id
   and T.length_id = LN.id" sid)))
    (if (or (sql-null? (vector-ref row 0))
            (sql-null? (vector-ref row 1))
            (sql-null? (vector-ref row 2))
            (sql-null? (vector-ref row 3)))
        (values #f #f #f)
        (values
         (vector-ref row 0)
         (vector-ref row 1)
         (bbox (vector-ref row 2) (vector-ref row 3)
               (vector-ref row 4) (vector-ref row 5))))))

(define (get-session-weather db sid)
  (and (can-do-web-requests?)
       (let-values (([start duration bbox]
                     (get-session-weather-lookup-data db sid)))
         (if bbox
             (let-values (([lat lon] (bbox-center bbox)))
               (let* ((url (format "~a/~a/~a,~a,~a?units=si"
                                   ds-api-url (ds-api-key)
                                   (~r lat #:precision 5)
                                   (~r lon #:precision 5)
                                   start))
                      (json (ds-fetch-json url)))
                 ;; even though the node is named 'currently, it contains data
                 ;; from when we asked for, even if the time is in the past.
                 (let ((currently (hash-ref json 'currently #f)))
                   (unless currently
                     (raise-wufetch-error
                      "Bad DarkSky.net reply: missing currently"
                      json))
                   (json->wobs currently))))
             (error "Cannot fetch weather data: no GPS location info")))))

(define (get-daily-observations-for-session db sid)
  (and (can-do-web-requests?)
       (let-values (([start duration bbox]
                     (get-session-weather-lookup-data db sid)))
         (if bbox
             (let-values (([lat lon] (bbox-center bbox)))
               (let* ((url (format "~a/~a/~a,~a,~a?units=si"
                                   ds-api-url (ds-api-key)
                                   (~r lat #:precision 5)
                                   (~r lon #:precision 5)
                                   start))
                      (json (ds-fetch-json url)))
                 ;; even though the node is named 'currently, it contains data
                 ;; from when we asked for, even if the time is in the past.
                 (let* ((hourly (hash-ref json 'hourly #f))
                        (currently (hash-ref json 'currently #f))
                        (data (and hourly (hash-ref hourly 'data))))
                   (unless (and currently data)
                     (raise-wufetch-error
                      "Bad DarkSky.net reply: missing currently or hourly"
                      json))
                   (sort (cons (json->wobs currently)
                               (for/list ([node data])
                                 (json->wobs node)))
                         < #:key wobs-ts))))
             (error "Cannot fetch weather data: no GPS location info")))))

(define session-weather-store-sql
  (virtual-statement
   (lambda (dbsys)
     "\
insert into SESSION_WEATHER(
  session_id, wstation, timestamp,
  temperature, dew_point, humidity,
  wind_speed, wind_gusts, wind_direction, pressure)
values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))

;; Store weather data in the database for SID.  If a previous weather record
;; for this session is present, it is removed first.  WS can be either a
;; WSTATION object or a string.
(define (update-session-weather db sid ws wo)
  (call-with-transaction
   db
   (lambda ()
     (query-exec
      db "delete from SESSION_WEATHER where session_id = ?" sid)
     (query-exec
      db session-weather-store-sql
      sid
      ws
      (wobs-ts wo)
      (or (wobs-temp wo) sql-null)
      (or (wobs-dewp wo) sql-null)
      (or (wobs-hum wo) sql-null)
      (or (wobs-wspd wo) sql-null)
      (or (wobs-wgust wo) sql-null)
      (or (wobs-wdir wo) sql-null)
      (or (wobs-pressure wo) sql-null)))))

(define (update-session-weather-auto db sid)
  ;; Skip this session if it already has weather records (e.g. from FIT files)
  (define c (query-value db "select count(*) from SESSION_WEATHER where session_id = ?" sid))
  (if (> c 0)
      (dbglog "update-session-weather-auto: session ~a already has ~a weather records" sid c)
      (with-handlers
        (((lambda (e) #t) (lambda (e) #f)))
        (let ((wobs (get-session-weather db sid)))
          (when wobs
            (update-session-weather db sid "#dark-sky" wobs))))))


;;............................................................. provides ....

(provide (struct-out wobs))

(provide/contract
 (update-session-weather-auto (-> connection? positive-integer? any/c))
 (update-session-weather (-> connection? positive-integer? string? wobs? any/c))
 (ds-api-key (-> (or/c string? #f)))
 (allow-weather-download (-> (or/c #t #f)))
 (set-allow-weather-download (-> (or/c #t #f) any/c))
 (get-daily-observations-for-session (-> connection? positive-integer? (or/c #f (listof wobs?))))
 (get-session-weather (-> connection? positive-integer? wobs?)))
