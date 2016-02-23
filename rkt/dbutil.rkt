#lang racket/base
;; dbutil.rkt -- utilities to create, open and back-up databases

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

(require db
         "al-prefs.rkt"
         (rename-in srfi/48 (format format-48))
         racket/file
         racket/string
         racket/contract)

(provide (struct-out db-exn-bad-version)
         db-exn-bad-version-message)

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(provide/contract
 [db-open (->*
           (path-string?)
           (#:schema-file (or/c #f path-string?)
            #:expected-version exact-positive-integer?
            #:progress-callback (or/c #f progress-callback/c))
           connection?)]
 [maybe-backup-database (-> path-string? any/c)]
 [db-get-last-pk (-> string? connection? number?)])
 
;; Read the next SQL statement from INPUT-PORT.  The statement is assumed to
;; be terminated by the #\; character.
(define (read-next-statement input-port)
  (let ((out (open-output-string))
        (in-string? #f))
    
    ;; Return the next character in the input stream PORT, collapsing all
    ;; whitespace to a single space and skipping all comments.  Comments start
    ;; with "--" and extend until the end of the line.  Strings are being
    ;; tracked for.
    (define (get-next-char)
      (let ((ch (read-char input-port)))
        
        (when (eqv? ch #\')
          (set! in-string? (not in-string?)))
        
        (cond ((eqv? ch eof) ch)
              
              ((and (char-whitespace? ch)
                    (let ((ch-next (peek-char input-port)))
                      (or (eqv? ch-next eof)
                          (char-whitespace? ch-next))))
               ;; Colapse all whitespace into one single space
               (get-next-char))
              
              ((and (not in-string?)
                    (eqv? ch #\-)
                    (eqv? (peek-char input-port) #\-))
               ;; This is a comment, skip it until end of line
               (for ((v (in-producer (lambda () (read-char input-port)) #\newline)))
                 (begin #f))
               #\ )
              
              ((char-whitespace? ch) #\ ) ; all whitespace converted to space
              (#t ch))))
    
    ;; read from the input stream using GET-NEXT-CHAR until a semi-colon (#\;)
    ;; is seen.  Intermediate read chars are written to OUT.  The full
    ;; statement is returned, or #f on EOF.
    (define (loop)
      (let ((ch (get-next-char)))
        (cond ((eqv? ch eof) ; incomplete statement
               #f)
              ((and (eqv? ch #\;) (not in-string?))
               (get-output-string out))
              (#t
               (write-char ch out)
               (loop)))))
       
    (loop)))

;; Read SQL statements from INPUT-PORT, and return them as a list
(define (collect-statements input-port)
  (let loop ((statements '()))
    (if (eqv? (peek-char input-port) eof)
        (reverse statements)
        (let ((stmt (read-next-statement input-port)))
          (loop (if stmt (cons stmt statements) statements))))))


;; Read SQL statements from INPUT-PORT and run them against DB.  Statements
;; are executed for side effects (e.g CREATE TABLE)
(define (execute-statements input-port db)
  (define (loop)
    (unless (eqv? (peek-char input-port) eof)
      (let ((stmt (read-next-statement input-port)))
        (when stmt
          (query-exec db stmt)))
      (loop)))
  (loop))


;; Create the initial schema in a database, if it does not already exist.  A
;; database that has no tables (no entries in SQLITE_MASTER) is considered to
;; be newly created.  In that case, we run the SCHEMA-FILE script on it to
;; create the initial database schema.  DATABASE-FILE is the name of the file
;; from which DB was opened.  If creating the schema fails, the file will be
;; removed.
(define (maybe-create-schema database-file schema-file db [progress-callback #f])
  (let ((new-database? (= 0 (query-value db "select count(*) from SQLITE_MASTER"))))
    (when new-database?
      (with-handlers
       (((lambda (e) #t)
         (lambda (e)
           (disconnect db)
           ;; database-file can be 'memory for in memory databases
           (when (path-string? database-file)
             (delete-file database-file))
           (raise e))))
       (let* ((statements (call-with-input-file schema-file collect-statements))
              (statement-count (length statements)))
         (for ([stmt statements]
               [n statement-count])
           (query-exec db stmt)
           (when progress-callback
             (progress-callback "Executing SQL statement..." (+ n 1) statement-count))))))))

;; An exception used to indicate that the database was a incorrect schema
;; version
(struct db-exn-bad-version (file expected actual) #:transparent)
(define (db-exn-bad-version-message e)
  (format
   "bad schema version: expected ~a, actual ~a"
   (db-exn-bad-version-expected e)
   (db-exn-bad-version-actual e)))

(define (db-open database-file
                 #:schema-file [schema-file #f]
                 #:expected-version [expected-version 1]
                 #:progress-callback [progress-callback #f])
  (let ((db (sqlite3-connect #:database database-file #:mode 'create)))
    (when schema-file
      (maybe-create-schema database-file schema-file db progress-callback))
    (query-exec db "pragma foreign_keys = on")
    (let ((actual-version (query-value db "select version from SCHEMA_VERSION")))
      (unless (and (number? actual-version) (= actual-version expected-version))
        (disconnect db)
        (raise (db-exn-bad-version database-file expected-version actual-version))))
    db))

(define db-get-last-pk
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select seq from SQLITE_SEQUENCE where name = ?"))))
    (lambda (table-name db)
      (query-value db stmt table-name))))


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
