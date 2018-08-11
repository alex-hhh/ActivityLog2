#lang racket/base
;; dbapp.rkt -- open the application database
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016 Alex Harsanyi (AlexHarsanyi@gmail.com)
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

(require "dbutil.rkt"
         "utilities.rkt"
         db/base
         racket/contract
         racket/runtime-path)

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(provide/contract
 [schema-version (-> exact-positive-integer?)]
 [current-database (-> (or/c #f connection?))]
 [set-current-database (-> (or/c #f connection?) any/c)]
 [open-activity-log (->* ((or/c 'memory path-string?)) ((or/c #f progress-callback/c)) connection?)]
 [add-db-open-callback (-> (-> connection? any/c) any/c)]
 [del-db-open-callback (-> (-> connection? any/c) any/c)])

(define (fail-with msg)
  (raise (make-exn:fail msg (current-continuation-marks))))

(define-runtime-path schema-file "../sql/db-schema.sql")

(define-runtime-path p19-file "../sql/p19-metrics.sql")
(define-runtime-path p20-file "../sql/p20-vtriact.sql")
(define-runtime-path p21-file "../sql/p21-hmetrics.sql")
(define-runtime-path p22-file "../sql/p22-smetrics.sql")
(define-runtime-path p23-file "../sql/p23-cp.sql")
(define-runtime-path p24-file "../sql/p24-vsz.sql")
(define-runtime-path p25-file "../sql/p25-index.sql")
(define-runtime-path p26-file "../sql/p26-vsz.sql")
(define-runtime-path p27-file "../sql/p27-tile-code-index.sql")
(define-runtime-path p28-file "../sql/p28-equipment.sql")
(define-runtime-path p29-file "../sql/p29-workouts.sql")

;; The schema version we expect in all databases we open.
(define (schema-version) 29)

;; Map a schema version to an upgrade file to the next version.
(define upgrade-patches
  (hash
   18 p19-file
   19 p20-file
   20 p21-file
   21 p22-file
   22 p23-file
   23 p24-file
   24 p25-file
   25 p26-file
   26 p27-file
   27 p28-file
   28 p29-file))

;; List of function to call after a new database was sucesfully opened.
(define db-open-callbacks '())

(define (add-db-open-callback proc)
  (set! db-open-callbacks (cons proc db-open-callbacks)))

(define (del-db-open-callback proc)
  (set! db-open-callbacks (remove proc db-open-callbacks)))

(define the-current-database #f)

(define (set-current-database db)
  (unless (or (eq? db #f) (connection? db))
    (fail-with "bad value for current-database"))
  (when db
    (for ([cb db-open-callbacks]) (cb db)))
  (set! the-current-database db))

;; The current database connection, if any.  NOTE: needs to be explicitely
;; set. `open-activity-log' will not set it!
(define (current-database) the-current-database)

;; Open the database in DATABASE-FILE, checking that it has the required
;; version or later.  A database schema will be created if this is a new
;; database.  If the database is not the expected version, it will be backed
;; up, than upgraded. Does not set `current-database'.
(define (open-activity-log database-file [progress-callback #f])
  (with-handlers
    ((db-exn-bad-version?
      (lambda (e)
        (let* ((expected (db-exn-bad-version-expected e))
               (actual (db-exn-bad-version-actual e))
               (patches (for/list ((v (in-range actual expected)))
                          ;; NOTE: we re-raise exception if we don't have the
                          ;; required patches.
                          (hash-ref upgrade-patches v (lambda () (raise e)))))
               (db (db-open database-file)))
          (maybe-backup-database database-file #t)
          (dbglog "upgrading database from version ~a to ~a" actual expected)
          (db-upgrade db patches #:progress-callback progress-callback)))))
    (define db
      (db-open
       database-file
       #:schema-file schema-file
       #:allow-higher-version #t
       #:expected-version (schema-version)
       #:progress-callback progress-callback))
    (define actual-version (query-value db "select version from SCHEMA_VERSION"))
    (unless (= actual-version (schema-version))
      (dbglog "Database has newer schema version, expected ~a, actual ~a"
              (schema-version) actual-version))
    db))
