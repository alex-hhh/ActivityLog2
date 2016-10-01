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
         db
         racket/contract
         racket/runtime-path)

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(provide/contract
 [schema-version (parameter/c exact-positive-integer?)]
 [current-database (-> (or/c #f connection?))]
 [set-current-database (-> (or/c #f connection?) any/c)]
 [open-activity-log (->* ((or/c 'memory path-string?)) ((or/c #f progress-callback/c)) connection?)]
 [add-db-open-callback (-> (-> connection? any/c) any/c)]
 [del-db-open-callback (-> (-> connection? any/c) any/c)])

(define (fail-with msg)
  (raise (make-exn:fail msg (current-continuation-marks))))

(define-runtime-path schema-file "../sql/db-schema.sql")

;; The schema version we expect in all databases we open.  An exception will
;; be raised if it does not match
(define schema-version
  (make-parameter 18 (lambda (v)
                       (fail-with "cannot set schema version"))))

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
;; version.  A database schema will be created if this is a new
;; database.  Does not set `current-database'.
(define (open-activity-log database-file [progress-callback #f])
  (db-open
   database-file
   #:schema-file schema-file
   #:expected-version (schema-version)
   #:progress-callback progress-callback))
