#lang racket/gui
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

(require racket/date
         (rename-in srfi/48 (format format-48))
         "al-prefs.rkt"
         "database.rkt"
         "first-run.rkt"
         "toplevel.rkt")

(provide main)

(define (main)
  (define database-file (al-get-pref 'activity-log:database-file (lambda () #f)))
  (unless (and database-file (file-exists? database-file))
    (let ((first-run-dlg (new first-run-dialog%)))
      (set! database-file (send first-run-dlg run))))
  (when database-file
    (maybe-backup-database database-file)
    (with-handlers
     ((db-exn-bad-db-version?
       (lambda (e)
         (message-box
          "Database open error" (db-exn-bad-db-version-message e) #f '(ok stop))))
      ((lambda (e) #t)
       (lambda (e)
         (message-box "Database open error" (format "~a : ~a" database-file e) #f '(ok stop)))))
     (let ((tl (new toplevel-window% [database-path database-file])))
       ;; Toplevel window was successfully created, save the database file as
       ;; the new default to open next time.
       (al-put-pref 'activity-log:database-file
                    (if (path? database-file)
                        (path->string database-file)
                        database-file))
       (send tl run)))
    ))


;;...................................................... database backup ....

;; Implement a simple backup strategy for the activity log databases: we copy
;; the db-file to a backup directory at the start of every week.  There is
;; currently no recovery implemented, the user needs to copy and restore
;; backups himself.


;; Place where we store backups.
(define db-backup-dir
  (build-path (al-get-pref-dir) "db-backup"))

;; ensure the backup directory exists, will never fail
(make-directory* db-backup-dir)

;; Return the timestamp of the start of the week (on Monday), as the number of
;; seconds since the UNIX epoch.
(define (week-start seconds)

  ;; number of seconds in a day
  (define day-as-seconds (* 24 60 60))

  (let ((d (seconds->date seconds #t)))
    (if (eqv? (date-week-day d) 0)      ; sunday
        (- seconds (* 6 day-as-seconds))
        (- seconds (* (- (date-week-day d) 1) day-as-seconds)))))

;; Return a backup file name for DB-FILE.  This is done by adding the week
;; start timestamp to the file name and making DB-BACKUP-DIR as the directory.
(define (get-backup-file-name db-file)
  (let-values (((_1 file _2) (split-path db-file)))
    (let* ((week-start (seconds->date (week-start (current-seconds))))
           (tag (string-replace
                 (format-48  "-~4F-~2F-~2F" (date-year week-start)
                             (date-month week-start)
                             (date-day week-start))
                 " " "0"))
           (backup-file (regexp-replace #rx"\\..*$" (path->string file) (string-append tag "\\0"))))
      (build-path db-backup-dir backup-file))))

;; Copy a file from ORIGINAL to BACKUP.  To reduce the chance of failure, we
;; first copy the ORIGINAL into a temporary file in the same target folder
;; than rename the temporary to BACKUP.  This way, BACKUP will not exist as a
;; partial file, so if something fails, we will attempt make the backup again.
(define (backup-database original backup)
  (let-values (((dir file _2) (split-path backup)))
    (let ((tmp (build-path dir (string-append (path->string file) ".tmp"))))
      (copy-file original tmp)
      (rename-file-or-directory tmp backup))))

;; Make a backup of DB-FILE, if needed (we only make one backup a week, if it
;; was already done for this week, we won't do it again).  Also, we won't make
;; a backup if the file is located in the backup dir already.
(define (maybe-backup-database db-file)
  (when (string? db-file) (set! db-file (string->path db-file)))
  (let-values (((dir _1 _2) (split-path db-file)))
    (let ((backup (get-backup-file-name db-file)))
      (when (and (file-exists? db-file)
                 (not (file-exists? backup))
                 (not (equal? dir db-backup-dir)))
        (backup-database db-file backup)))))
