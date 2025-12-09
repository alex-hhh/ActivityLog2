#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; al2-activity-import.rkt -- command line tool to import activities
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2023, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/cmdline
         db
         racket/class
         racket/format
         "../rkt/dbapp.rkt"
         "../rkt/import.rkt"
         "../rkt/database.rkt"
         "../rkt/utilities.rkt"
         "../rkt/app-info.rkt"
         "../rkt/sport-charms.rkt")

(define start-timestamp (current-inexact-milliseconds))
(define database-file (make-parameter #f))
(define import-directory (make-parameter #f))
(define individual-files (make-parameter '()))

(define (log0 port msg . args)
  (define ts (/ (- (current-inexact-milliseconds) start-timestamp) 1000.0))
  (fprintf port "[~a] ~a~%"
           (~a #:width 8 #:align 'right (~r ts #:precision '(= 1)))
           (apply format msg args)))

(define (log msg . args)
  (apply log0 (current-output-port) msg args))

(define (elog msg . args)
  (apply log0 (current-error-port) msg args))

(command-line
 #:program "al2-activity-import"
 #:usage-help "Import new activities into an ActivityLog2 database"
 #:once-each
 (("-b" "--database")
  db-file
  "Location of the ActivityLog2 database"
  (unless (file-exists? db-file)
    (elog "database ~a does not exist" db-file)
    (exit 1))
  (database-file db-file))
 (("-d" "--import-directory")
  dir
  "Directory from where to import files"
  (unless (directory-exists? dir)
    (elog "directory ~a does not exist, or it is not a directory" dir)
    (exit 1))
  (import-directory dir))
 #:args filenames
 (when (and (not (null? filenames)) (import-directory))
   (elog "file names are incompatible with the --import-directory option")
   (exit 1))
 (for ([f (in-list filenames)])
   (unless (file-exists? f)
     (elog "file ~a does not exist" f)
     (exit 1)))
 (individual-files filenames))

(unless (database-file)
  (elog "Missing database file, try --help for options")
  (exit 1))

(when (and (eq? (import-directory) #f)
           (null? (individual-files)))
  (elog "Nothing to import, try --help for options")
  (exit 1))

(dbglog "Al2-Activity-Import started, version ~a, ~a, ~a"
        (app-version) (app-commit-id) (app-build-timestamp))
(dbglog "will try to open ~a" (database-file))

(define db
  (open-activity-log
   (database-file)
   ;; Callback is invoked if database needs to be upgraded, in that case,
   ;; print things out to the user.
   (lambda (text n total)
     (log "~a (~a of ~a)" text n total))))

(set-current-database db) ; Sadly, some code still uses the global (current-database)
(define sport-charms (new sport-charms% [dbc db]))

(if (import-directory)
    ;; NOTE: this call uses dbglog to output log messages already
    (import-new-activities-from-directory
     (import-directory)
     db
     sport-charms
     (lambda (file-name status extra)
       (log "Importing ~a: ~a (~a)" file-name status extra))
     (lambda (msg)
       (log "~a" msg)))

    (begin
      (query-exec db "delete from LAST_IMPORT")
      (for ([file (in-list (individual-files))])
        (dbglog "Importing ~a" file)
        (define iresult (db-import-activity-from-file file db))
        (dbglog "Imported ~a: ~a, ~a" file (car iresult) (cdr iresult))
        (log "File ~a: ~a, ~a" file (car iresult) (cdr iresult)))
      (when (> (query-value db "select count(*) from LAST_IMPORT") 0)
        ;; NOTE: the call below already uses dbglog
        (do-post-import-tasks
         db
         sport-charms
         (lambda (msg)
           (log "~a" msg))))))

(disconnect db)
