#lang racket
;; bulk-export.rkt -- demonstrates how to export original files in bulk

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


(require db)
(require "../rkt/database.rkt")
(require "../rkt/al-prefs.rkt")

(define *db*
  (let ((db-file (al-get-pref 'activity-log:database-file (lambda () #f))))
    (unless db-file
      (error "No default database"))
    (db-open-activity-log db-file)))

;; Get a list of activity id's for the sessions we want to export.  The query
;; below exports all Garmin Vector activities (note that the equipment id's
;; are refereced directly, so they will not be identical in a different
;; database).
(define (get-candidates db)
  (query-list
   db
"select distinct S.activity_id
  from A_SESSION S, EQUIPMENT_USE EU
 where EU.equipment_id in (18, 20)
   and S.id = EU.session_id
   and S.sport_id = 2"))

(define (bulk-export db activity-id-list out-dir)
  (for ([id activity-id-list])
    (let ([fname (build-path out-dir (format "activity-~a.fit" id))])
      (printf "Exporting ~a...~%" fname)
      (db-export-raw-data id db fname))))
