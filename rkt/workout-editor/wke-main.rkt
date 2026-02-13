#lang racket/base
;; wke-main.rkt -- main application entry point for al2-workout-editor
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         "wke-toplevel.rkt"
         "wke-db.rkt"
         "../dbutil.rkt"
         "../dialogs/first-run.rkt"
         "../utilities.rkt"
         "../app-info.rkt")

(provide wke-main)

(define dbfile-key 'al2-workout-editor:database-file)

;; NOTE: logging is setup to go to one hard coded file for the entire project,
;; we should change that, so we can have multiple files

(define (wke-main start-timestamp cmdline-db-file)
  (let ([d (exact-round (- (current-inexact-monotonic-milliseconds) start-timestamp))])
    (dbglog "AL2-Workout-Editor started in ~a ms, version ~a, ~a, ~a"
            d (app-version) (app-commit-id) (app-build-timestamp)))
  (with-handlers
    (((lambda (e) #t)
      (lambda (e)
        (dbglog "AL2-Workout-Editor: caught exception in main: ~a" e)
        ;; Reset the default database on exception, next restard, this will
        ;; prompt the user to open another database...
        (put-pref dbfile-key #f)
        ;; Don't attempt to shut down workers here, as we might make a bad
        ;; problem worse...
        (exit 1))))
    (define database-file
      (or cmdline-db-file (get-pref dbfile-key (lambda () #f))))
    (cond ((not database-file)
           (dbglog "AL2-Workout-Editor: no database file stored in preferences"))
          ((and database-file (not (file-exists? database-file)))
           (dbglog "AL2-Workout-Editor: missing ~a" database-file))
          (#t
           (dbglog "AL2-Workout-Editor: will try to open ~a" database-file)))
    (unless (and database-file (file-exists? database-file))
      (let ((first-run-dlg (new first-run-dialog%
                                [db-file-name "AL2-Workouts.db"]
                                [backup-db-file-name "AL2-Workouts-~a.db"]
                                [create-db-callback wke-open-database])))
        (dbglog "AL2-Workout-Editor: running first-run-dialog%")
        (shutdown-splash)
        (close-splash)
        (set! database-file (send first-run-dlg run))
        (dbglog "AL2-Workout-Editor: database file is now ~a" database-file)))
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
        (dbglog "AL2-Workout-Editor: no database file, exiting application"))))
