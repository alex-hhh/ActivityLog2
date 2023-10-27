#lang racket/base
;; main.rkt -- main application entry point
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2020, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require framework/splash
         racket/class
         racket/math
         (only-in map-widget map-widget-logger)
         "dbutil.rkt"
         "dialogs/first-run.rkt"
         "toplevel.rkt"
         "utilities.rkt"
         "app-info.rkt")

(provide main)

(define dbfile-key 'activity-log:database-file)

;; Create a log-receiver for LOGGERS which logs all their messages using
;; `dbglog`
(define (make-dbglog-sink . loggers)
  (define sources (for/list ([logger loggers])
                    (make-log-receiver logger 'info)))
  (define (do-logging)
    (let* ((log-item (apply sync sources))
           (level (vector-ref log-item 0))
           (message (vector-ref log-item 1)))
      ;; NOTE: an empty message indicates a notification retraction (as
      ;; determined by tag, which sits in the third place of LOG-ITEM vector).
      ;; Retractions are only used in the GUI to remove the message from the
      ;; banner, so we ignore them here.
      (unless (equal? message "")
        (dbglog "~a: ~a" level message)))
    (do-logging))
  (thread do-logging)
  (void))

(define (main start-timestamp cmdline-db-file)
  ;; notifications from our loggers go do the dbglog sink
  (make-dbglog-sink map-widget-logger user-notification-logger)
  (dbglog "ActivityLog2 started, version ~a, ~a, ~a"
          (app-version) (app-commit-id) (app-build-timestamp))
  (let ([d (exact-round (- (current-inexact-monotonic-milliseconds) start-timestamp))])
    (dbglog "startup duration ~a ms" d))
  (with-handlers
    (((lambda (e) #t)
      (lambda (e)
        (dbglog "caught exception in main: ~a" e)
        ;; Reset the default database on exception, next restard, this will
        ;; prompt the user to open another database...
        (put-pref dbfile-key #f)
        ;; Don't attempt to shut down workers here, as we might make a bad
        ;; problem worse...
        (exit 1))))
    (define database-file
      (or cmdline-db-file (get-pref dbfile-key (lambda () #f))))
    (cond ((not database-file)
           (dbglog "no database file stored in preferences"))
          ((and database-file (not (file-exists? database-file)))
           (dbglog "missing ~a" database-file))
          (#t
           (dbglog "will try to open ~a" database-file)))
    (unless (and database-file (file-exists? database-file))
      (let ((first-run-dlg (new first-run-dialog%)))
        (dbglog "running first-run-dialog%")
        (shutdown-splash)
        (close-splash)
        (set! database-file (send first-run-dlg run))
        (dbglog "database file is now ~a" database-file)))
    (if database-file
        (begin
          (maybe-backup-database database-file)
          (let ((tl (new toplevel-window% [database-path database-file])))
            ;; Toplevel window was successfully created, save the database
            ;; file as the new default to open next time.  Unless the database
            ;; file was specified on the command line, in which case we don't
            ;; save it.
            (unless cmdline-db-file
              (put-pref dbfile-key
                        (if (path? database-file)
                            (path->string database-file)
                            database-file)))
            (send tl run)))
        (dbglog "no database file, exiting application"))))
