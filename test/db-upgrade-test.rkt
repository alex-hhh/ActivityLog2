#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2017, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require db
         racket/file
         rackunit
         "../rkt/dbapp.rkt")

;;(require rackunit/gui)

(define test-dir "./test-db")

(define (candidate-databases)
  ;; Ensure that test-db directory exists (in case the download database
  ;; script failed to run
  (make-directory* test-dir)
  (for/list ([path (in-directory test-dir)]
             #:when (let-values (([base name _] (split-path path)))
                      (regexp-match #rx"^al2-v[0-9]+\\.db$" name)))
    path))

(define (db-vesion path)
  (let ((db (sqlite3-connect #:database path #:mode 'create)))
    (begin0
        (query-value db "select version from SCHEMA_VERSION")
      (disconnect db))))

(define (test-upgrade path)
  (define version (db-vesion path))
  (define target-version (schema-version))
  (define tmpdb (build-path test-dir "tmp.db"))
  (check <= version target-version
         (format "~a cannot upgrade from version ~a to ~a~%"
                 (path->string path) version target-version))
  (printf "*** ~a: attempt upgrade from version ~a to ~a~%"
          (path->string path) version target-version)

  (define (progress-cb msg stmt total)
    (printf "   ~a ~a/~a~%" msg stmt total))

  (when (file-exists? tmpdb)
    (delete-file tmpdb))
  (copy-file path tmpdb)

  (define db #f)
  (check-not-exn
   (lambda () (set! db (open-activity-log tmpdb progress-cb))))

  (let ((nversion (query-value db "select version from SCHEMA_VERSION")))
    (check-eqv? nversion target-version
                (format "wrong version after upgrade, expected ~a, got ~a~%"
                        target-version nversion))
    (disconnect db)
    (delete-file tmpdb)))

(module+ test
  (require rackunit/text-ui)
  (for ((db (in-list (candidate-databases))))
    (test-upgrade db)))
