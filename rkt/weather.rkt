#lang racket/base
;; weather.rkt -- weather utilities
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2019, 2020, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/contract
         racket/math)


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


;;...................................................... DS API requests ....


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


;;............................................................. provides ....

(provide (struct-out wobs))

(provide/contract
 (update-session-weather (-> connection? positive-integer? string? wobs? any/c)))
