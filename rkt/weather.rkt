#lang racket/base
;; weather.rkt -- fetch weather data from wunderground
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

(require db/base
         json
         net/url
         net/url-connect
         racket/date
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/contract
         racket/format
         (rename-in srfi/48 (format format-48))
         "utilities.rkt"
         "dbutil.rkt"
         "widgets/map-widget/map-util.rkt")
(require (for-syntax racket/base))

(provide (struct-out wstation)
         (struct-out wobs)
         ;; init-weather
         ;; get-nearby-wstations
         get-nearby-wstations-for-session
         get-observations-for-station
         update-session-weather
         update-session-weather-auto)


;; Compute a "Feels like" temperature based on TEMPERATURE and DEW-POINT, this
;; is done using the formula from
;; http://climate.weather.gc.ca/climate_normals/normals_documentation_e.html

(define (humindex temperature dew-point)
  (let* ((dewpk (+ dew-point 273.16))
         (e (* 6.11 (exp (* 5417.7530 (- (/ 1 273.16) (/ 1 dewpk))))))
         (h (* 0.5555 (- e 10.0))))
    (+ temperature h)))

(provide/contract
 (humindex (-> real? real? real?)))


;;...................................................... small utilities ....

(define (hash-ref-many hash keys)
  (if (null? keys)
      hash
      (hash-ref-many (hash-ref hash (car keys)) (cdr keys))))


;;........................................................... data types ....

;; Weather station
(struct wstation (name ident type lat lon active-since) #:transparent)

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


;;............................................ WU Json Response Parsing ....

(define (wu-utc-date->seconds h)
  (let ((year (string->number (hash-ref h 'year)))
        (month (string->number (hash-ref h 'mon)))
        (day (string->number (hash-ref h 'mday)))
        (hour (string->number (hash-ref h 'hour)))
        (min (string->number (hash-ref h 'min))))
    (date->seconds
     (date 0 min hour day month year 0 0 #f 0)
     #f)))

;; read a weather observation from NODE, a json node
(define (json->wobs node)
  (let ((timestamp (wu-utc-date->seconds (hash-ref node 'utcdate)))
        (temp (string->number (hash-ref node 'tempm #f)))
        (dewp (string->number (hash-ref node 'dewptm #f)))
        (hum (string->number (hash-ref node 'hum #f)))
        (wspd (string->number (hash-ref node 'wspdm #f)))
        (wgust (string->number (hash-ref node 'wgustm #f)))
        (wdir (string->number (hash-ref node 'wdird #f)))
        (pres (string->number (hash-ref node 'pressurem #f))))
    (wobs
     timestamp
     temp
     dewp
     hum
     ;; convert windspeeds from km/h to m/s
     (if (and wspd (>= wspd 0)) (* wspd (/ 1000 3600)) #f)
     (if (and wgust (>= wgust 0)) (* wgust (/ 1000 3600)) #f)
     wdir
     pres)))

;; Read a weather station definition from NODE, a json node
(define (json->wstation node)
  (let ((city (hash-ref node 'city #f))
        (lat (hash-ref node 'lat))
        (lon (hash-ref node 'lon))
        (icao (hash-ref node 'icao #f))
        (country (hash-ref node 'country #f))
        (pws (hash-ref node 'id #f)))
    (let* ((ident (or icao pws))
           (name (or city ident)))
      (if (and ident (not (equal? "" ident)))
          (wstation name
                    (if icao (format "~a/~a" country ident)
                        (format "pws:~a" ident))
                    (if icao 'icao 'pws)
                    (if (string? lat) (string->number lat) lat)
                    (if (string? lon) (string->number lon) lon)
                    #f)
          #f))))

;; Parse the JSON response from a geolocation query and return a list of
;; weather stations.
(define (parse-nearby-response node)
  (let ((stations (hash-ref-many node '(location nearby_weather_stations))))
    (let ((airport (hash-ref-many stations '(airport station)))
          (pws (hash-ref-many stations '(pws station))))
      (filter values
              (append
               (map json->wstation airport)
               (map json->wstation pws))))))

;; Parse the JSON response from a weather history query and return a list of
;; observations (wobs structures)
(define (parse-history-response node)
  (let ((h (hash-ref-many node '(history observations))))
    (filter values (map json->wobs h))))




;;...................................................... WU API requests ....

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
(provide/contract
 (allow-weather-download (-> (or/c #t #f)))
 (set-allow-weather-download (-> (or/c #t #f) any/c)))

;; This "magic" macro will embed the API key at compile time, allowing us to
;; ship a built version of the application with the API key already inside it.
(define-syntax (embedded-api-key stx)
  (syntax-case stx ()
    [_ #`(quote #,(getenv "AL2WUAPIKEY"))]))
(define (builtin-api-key) (embedded-api-key))

(define wu-api-key-tag 'activity-log:wu-api-key)
;; the API key value comes either from the preferences file (needs to be
;; manually stored there), from an environment variable, or a built in one (if
;; available)
(define wu-api-key-val
  (or (get-pref wu-api-key-tag (lambda () #f))
      (getenv "AL2WUAPIKEY")
      (builtin-api-key)))
(define (wu-api-key) wu-api-key-val)

(provide/contract
 (wu-api-key (-> (or/c string? #f))))

(define wu-api-url "https://api.wunderground.com/api")

;; Limit requests to 8 every 60 seconds
(define wu-request-limiter (make-request-limiter 8 60))

(struct exn:fail:wufetch exn:fail:network (url json)
  #:extra-constructor-name make-exn:fail:wufetch
  #:transparent)

;; Wrap thunk in exception handlers, log any exceptions and re-throw as
;; necessary
(define (with-wu-handlers thunk)
  (with-handlers
   (((lambda (e) #t)
     (lambda (e)
       ;; First, put the actual error to the log file
       (dbglog-exception "wu-fetch-json" e)
       (cond
         ((exn:fail:network? e)
          (raise-user-error "Network error while fetching weather"))
         ((exn:fail:wufetch? e)
          (raise-user-error "Error reply from weather server"))
         ((exn:fail:user? e)
          (raise e))                    ; raise it again
         (#t
          (raise-user-error "Unknown error while fetching weather data"))))))
   (thunk)))

(define (wu-fetch-json url)
  ;; Normally, these two conditions should not be encountered.
  (unless (wu-api-key) (raise-user-error "no Wunderground api key set"))
  (unless (allow-weather-download) (raise-user-error "weather download not permitted"))
  (wu-request-limiter)
  (parameterize ((current-https-protocol 'secure))
    (let* ((data (port->string (get-pure-port (string->url url))))
           (json (call-with-input-string data read-json))
           (response (hash-ref json 'response #f)))
      (if response
          (if (hash-ref response 'error #f)
              (raise (make-exn:fail:wufetch "Error Wunderground reply" (current-continuation-marks) url json))
              json)
          (raise (make-exn:fail:wufetch "Bad Wunderground reply" (current-continuation-marks) url json))))))

(define (wu-make-geolookup-url lat lon)
  (format-48 "~a/~a/geolookup/q/~a,~a.json" wu-api-url (wu-api-key) lat lon))

(define (wu-fetch-nearby lat lon)
  (with-wu-handlers
    (lambda ()
      (let* ((url (wu-make-geolookup-url lat lon))
             (json (wu-fetch-json url)))
        (if json (parse-nearby-response json) '())))))

(define (wu-make-history-url wsident timestamp)
  (let* ((d (seconds->date timestamp #t))
         (dtext (string-replace
                 (format-48 "~4F~2F~2F"
                            (date-year d) (date-month d) (date-day d))
                 " " "0")))
    (format-48
     "~a/~a/history_~a/q/~a.json"
     wu-api-url (wu-api-key) dtext wsident)))

(define (wu-fetch-history wsident timestamp)
  (with-wu-handlers
    (lambda ()
      (let* ((url (wu-make-history-url wsident timestamp))
             (json (wu-fetch-json url)))
        (if json (parse-history-response json) '())))))


;;............................................................. WU cache ....

(define (get-default-db-file-name)
  (build-path (data-directory) "WuCache.db"))

(define wucache-file-name
  (get-pref 'activity-log:wu-cache-file
               (lambda () (get-default-db-file-name))))

(define-runtime-path wucache-schema-file "../sql/wucache-schema.sql")

(define (open-wucache-database database-file)
  (db-open
   database-file
   #:schema-file wucache-schema-file
   #:expected-version 1))

(define wstation-find-sql
  (virtual-statement
   (lambda (dbsys)
     "select id from WU_WSTATION where ident = ? and type = ?")))

(define (wucache-get-wstation-id db type ident)
  (query-maybe-value
   db wstation-find-sql
   ident (if (symbol? type) (symbol->string type) type)))

(define wstation-store-sql
  (virtual-statement
   (lambda (dbsys)
     "
insert into WU_WSTATION (name, ident, type,
                         position_lat, position_lon, active_since)
values (?, ?, ?, ?, ?, ?)")))

(define (wucache-store-wstation db wstation)
  (unless (wucache-get-wstation-id
           db (wstation-type wstation) (wstation-ident wstation))
    (query-exec
     db wstation-store-sql
     (wstation-name wstation)
     (wstation-ident wstation)
     (symbol->string (wstation-type wstation))
     (wstation-lat wstation)
     (wstation-lon wstation)
     (let ((as (wstation-active-since wstation)))
       (if as as sql-null)))))

(define (wucache-store-wstation-list db wstation-list)
  (call-with-transaction
   db
   (lambda ()
     (for ((ws (in-list wstation-list)))
       (wucache-store-wstation db ws)))))

(define (wucache-get-all-wstations db)
  (for/list (([name ident type lat lon active-since]
              (in-query db "\
select name, ident, type, position_lat, position_lon, active_since
  from WU_WSTATION")))
    (wstation name ident (string->symbol type) lat lon
              (if (sql-null? active-since) #f active-since))))

(define wobs-store-sql
  (virtual-statement
   (lambda (dbsys)
     "\
insert into WU_OBSERVATION(
  wstation_id, timestamp, temperature, dew_point, humidity,
  wind_speed, wind_gusts, wind_direction, pressure)
values (?, ?, ?, ?, ?, ?, ?, ?, ?)")))

(define (wucache-store-wobs db station-id wo)
  (unless (query-maybe-value db "\
select id from WU_OBSERVATION where wstation_id = ? and timestamp = ?"
                           station-id (wobs-ts wo))
    (query-exec db wobs-store-sql
              station-id
              (wobs-ts wo)
              (or (wobs-temp wo) sql-null)
              (or (wobs-dewp wo) sql-null)
              (or (wobs-hum wo) sql-null)
              (or (wobs-wspd wo) sql-null)
              (or (wobs-wgust wo) sql-null)
              (or (wobs-wdir wo) sql-null)
              (or (wobs-pressure wo) sql-null))))

(define (wucache-store-wobs-list db wstype wsident wo-list)
  (call-with-transaction
   db
   (lambda ()
     (let ((station-id (wucache-get-wstation-id db wstype wsident)))
       (when station-id
         (for ((ws (in-list wo-list)))
           (wucache-store-wobs db station-id ws)))))))

(define (wucache-get-wobs db station-id start-ts end-ts)
  (for/list (([ts temp dewp hum wspd wgust wdir pressure]
              (in-query db "\
select timestamp,
       ifnull(temperature, -1000),
       ifnull(dew_point, -1000),
       ifnull( humidity, -1),
       ifnull(wind_speed, -1),
       ifnull(wind_gusts, -1),
       ifnull(wind_direction, -1),
       ifnull(pressure, -1)
from WU_OBSERVATION
where wstation_id = ? and timestamp > ? and timestamp < ?"
                        station-id start-ts end-ts)))
    (wobs ts temp dewp hum wspd wgust wdir pressure)))

(define (wucache-get-active-since db type ident)
  (query-maybe-value
   db "\
select ifnull(active_since, 0) from WU_WSTATION
 where type = ? and ident = ?"
   (if (symbol? type) (symbol->string type) type) ident))

(define (wucache-update-active-since db type ident active-since)
  (let ((old-as (wucache-get-active-since db type ident)))
    (when (or (not old-as) (> active-since old-as))
      (query-exec
       db "\
update WU_WSTATION set active_since = ?
 where type = ? and ident = ?"
       active-since
       (if (symbol? type) (symbol->string type) type)
       ident))))

;;....................................................... Weather Lookup ....

(define (can-do-web-requests?)
  (and (allow-weather-download) (wu-api-key)))

(define (distance-from-wstation wstation lat lon)
  (map-distance/degrees
   lat lon
   (wstation-lat wstation) (wstation-lon wstation)))

;; Maximum distance for a cached weater station to be considered
(define *max-cache-dist* 
  (if (can-do-web-requests?) 10000 40000))

(define *min-cache-stations* 
  (if (can-do-web-requests?) 5 1))

(define (wu-get-nearby lat lon active-at)

  (define (get-nearby/cache lat lon active-at)
    (filter (lambda (ws)
              (and (let ((as (wstation-active-since ws)))
                     (or (not as) (> active-at as)))
                   (< (distance-from-wstation ws lat lon) *max-cache-dist*)))
            (wucache-get-all-wstations (wu-cache-db))))

  (define (get-nearby/web-req lat lon active-at)
    (let ((nearby (wu-fetch-nearby lat lon)))
      (wucache-store-wstation-list (wu-cache-db) nearby)
      ;; Use the cache to determine the "active since" value for stations
      (filter (lambda (ws)
                (let ((as (wucache-get-active-since
                           (wu-cache-db) (wstation-type ws) (wstation-ident ws))))
                  (or (not as) (> active-at as))))
              nearby)))

  (let ((cached (get-nearby/cache lat lon active-at)))
    (if (or (> (length cached) *min-cache-stations*)
            (not (can-do-web-requests?)))
        cached
        (get-nearby/web-req lat lon active-at))))

(define (wu-get-history wstype wsident timestamp)

  (define (get-history/cache wstype wsident timestamp)
    (let ((station-id (wucache-get-wstation-id (wu-cache-db) wstype wsident)))
      (if station-id
          (wucache-get-wobs (wu-cache-db) station-id
                            (- timestamp (* 2 3600))
                            (+ timestamp (* 5 3600)))
          '())))

  (define (get-history/web-req wstype wsident timestamp)
    (let ((wobs (wu-fetch-history wsident timestamp)))
      (if (null? wobs)
          (wucache-update-active-since (wu-cache-db) wstype wsident timestamp)
          (wucache-store-wobs-list (wu-cache-db) wstype wsident wobs))
      wobs))

  (let ((cached (get-history/cache wstype wsident timestamp)))
    (if (or (> (length cached) 5)
            (not (can-do-web-requests?)))
        cached
        (get-history/web-req wstype wsident timestamp))))

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
         (map-bbox (vector-ref row 2) (vector-ref row 3)
                   (vector-ref row 4) (vector-ref row 5))))))

(define (get-closest-wstation wstation-list lat lon)
  (let ((closest #f)
        (closest-distance #f))
    (for ((ws (in-list wstation-list)))
      (let ((new-dist (distance-from-wstation ws lat lon)))
        (when (or (not closest-distance) (< new-dist closest-distance))
          (set! closest ws)
          (set! closest-distance new-dist))))
    closest))

(define (get-best-observations wobs-list start duration)
  (let ((wo-start #f)
        (wo-start-diff #f)
        (wo-end #f)
        (wo-end-diff #f))
    (for ((wo (in-list wobs-list)))
      (let ((diff (abs (- (wobs-ts wo) start))))
        (when (or (not wo-start-diff) (< diff wo-start-diff))
          (set! wo-start wo)
          (set! wo-start-diff diff)))
      (let ((diff (abs (- (wobs-ts wo) (+ start duration)))))
        (when (or (not wo-end-diff) (< diff wo-end-diff))
          (set! wo-end wo)
          (set! wo-end-diff diff))))
    (list wo-start wo-end)))

(define (get-session-weather db sid)

  (define (search-weather wstations lat lon start duration)
    (if (null? wstations)
        #f
        (let ((ws (get-closest-wstation wstations lat lon)))
          (let ((history (wu-get-history (wstation-type ws)
                                         (wstation-ident ws)
                                         start)))
            (if history
                (let ((w (get-best-observations history start duration)))
                  (if (and (car w) (cdr w))
                      (list ws (car w) (cdr w))
                      (search-weather (remove ws wstations)
                                      lat lon start duration)))
                #f)))))

  (let-values (([start duration bbox]
                (get-session-weather-lookup-data db sid)))
    (if bbox
        (let-values (([lat lon] (bbox-center bbox)))
          (let ((wstations (wu-get-nearby lat lon start)))
            (search-weather wstations lat lon start duration)))
        #f)))

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
      (if (wstation? ws) (wstation-ident ws) ws)
      (wobs-ts wo)
      (or (wobs-temp wo) sql-null)
      (or (wobs-dewp wo) sql-null)
      (or (wobs-hum wo) sql-null)
      (or (wobs-wspd wo) sql-null)
      (or (wobs-wgust wo) sql-null)
      (or (wobs-wdir wo) sql-null)
      (or (wobs-pressure wo) sql-null)))))

(define (update-session-weather-auto db sid)
  (with-handlers
    (((lambda (e) #t) (lambda (e) #f)))
    (let ((w (get-session-weather db sid)))
      (when w
        (update-session-weather db sid (first w) (second w))))))


;;.............................................................. public data ....

(define (get-nearby-wstations-for-session db sid)
  (let-values (([start duration bbox]
                (get-session-weather-lookup-data db sid)))
    (if bbox
        (let-values (([lat lon] (bbox-center bbox)))
          (let ((wstations (wu-get-nearby lat lon start)))
            (sort
             (for/list ((ws (in-list wstations)))
               (cons ws (distance-from-wstation ws lat lon)))
             <
             #:key cdr)))
        '())))

(define (get-observations-for-station ws time)
  (let ((type 'icao) (ident ws))
    (if (string? ws)
        (let ((m (regexp-match "^(.*):(.*)$" ws)))
          (when m
            (set! type (string->symbol (list-ref m 1)))))
        (begin
          (set! type (wstation-type ws))
          (set! ident (wstation-ident ws))))
    (if (and type ident)
        (wu-get-history type ident time)
        #f)))



(define *wucache-db* #f)

(define (wu-cache-db)
  ;; Open an in-memory database, which will be only valid while the app is up
  ;; and running.  This will cache data requests to reduce the number of
  ;; calls, but avoid the problems of keeping old data in the database.
  ;;
  ;; NOTE: we don't need a database at all now, as all caching could be
  ;; implemented with hash tables, but it is more convenient to keep it around
  ;; for the caching mechanism.  Maybe one day I will remove it.
  (unless *wucache-db*
    (set! *wucache-db* (open-wucache-database 'memory)))
  *wucache-db*)
