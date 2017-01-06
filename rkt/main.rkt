#lang racket/base
;; activity-log-main.rkt -- main application entry point
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

(require racket/gui/base
         racket/class
         framework/splash
         "al-prefs.rkt"
         "dbglog.rkt"
         "dbutil.rkt"
         "first-run.rkt"
         "toplevel.rkt"
         "version.rkt")

(provide main)

(define dbfile-key 'activity-log:database-file)

(define (main)
  (dbglog "main started, version ~a, ~a, ~a"
          (app-version) (app-commit-id) (app-build-timestamp))
  (with-handlers
    (((lambda (e) #t)
      (lambda (e)
        (dbglog "caught exception in main: ~a" e)
        ;; Reset the default database on exception, next restard, this will
        ;; prompt the user to open another database...
        (al-put-pref dbfile-key #f))))
    (define database-file (al-get-pref dbfile-key (lambda () #f)))
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
            ;; file as the new default to open next time.
            (al-put-pref dbfile-key
                         (if (path? database-file)
                             (path->string database-file)
                             database-file))
            (send tl run)))
        (dbglog "no database file, exiting application"))))
