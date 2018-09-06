#lang racket/base
;; dbutil.rkt -- utilities to create, open and back-up databases,
;;               and other small helpers

;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         file/gunzip
         file/gzip
         json
         racket/contract
         racket/file
         racket/format
         racket/runtime-path
         "utilities.rkt"
         (for-syntax racket/base))

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(provide (struct-out db-exn-bad-version)
         ;; define-sql-statement need runtime-path to be present, so provide
         ;; it for all our clients.
         (all-from-out racket/runtime-path)
         db-exn-bad-version-message
         define-sql-statement)

(provide/contract
 [db-open (->*
           ((or/c 'memory path-string?))
           (#:schema-file (or/c #f path-string?)
            #:allow-higher-version boolean?
            #:expected-version (or/c #f exact-positive-integer?)
            #:progress-callback (or/c #f progress-callback/c))
           connection?)]
 [db-upgrade (->* (connection? (listof path-string?))
                  (#:progress-callback (or/c #f progress-callback/c))
                  connection?)]
 [maybe-backup-database (->* (path-string?) (boolean?) any/c)]
 [db-get-last-pk (-> string? connection? number?)]
 [sql-column-ref (->* (vector? exact-nonnegative-integer?) (any/c) any/c)]

 ;; NOTE: jsexpr? performs a deep check on its argument and might be too
 ;; expensive as a contract, especially since we use it in metrics.rkt
 [jsexpr->compressed-string (-> jsexpr? bytes?)]
 [compressed-string->jsexpr (-> bytes? jsexpr?)]

 )

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

;; Upgrade a database by executing statements from PATCHES, a list of file
;; names containing SQL statements.
(define (db-upgrade db patches #:progress-callback [progress-callback #f])
  (define statements
    (apply append
           (for/list ([patch patches])
             (call-with-input-file patch collect-statements))))
  (define statement-count (length statements))
  (for (((stmt n) (in-indexed statements)))
    (query-exec db stmt)
    (when progress-callback
      (progress-callback "Executing SQL upgrade statement..." (+ n 1) statement-count)))
  db)

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
                 #:allow-higher-version [allow-higher-version #f]
                 #:expected-version [expected-version #f]
                 #:progress-callback [progress-callback #f])
  (let ((db (sqlite3-connect #:database database-file #:mode 'create)))
    (when schema-file
      (maybe-create-schema database-file schema-file db progress-callback))
    (query-exec db "pragma foreign_keys = on")
    (query-exec db "pragma cache_size = 4000")
    (when expected-version
      (let ((actual-version (query-value db "select version from SCHEMA_VERSION")))
        (unless (and (number? actual-version)
                     (or (and allow-higher-version (>= actual-version expected-version))
                         (= actual-version expected-version)))
          (disconnect db)
          (raise (db-exn-bad-version database-file expected-version actual-version)))))
    db))

(define db-get-last-pk
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select seq from SQLITE_SEQUENCE where name = ?"))))
    (lambda (table-name db)
      (query-value db stmt table-name))))


;;................................................. define-sql-statement ....

;; Define a `virtual-statement` containing an SQL statement read from a file.
;; This macro is used as (define-sql-statement name file) and defines name to
;; be a function of no arguments which returns a `virtual-statement` when
;; invoked.  The SQL statement will be read from the specified file the first
;; time the function is called.
;;
;; This is intended to be used instead of defining SQL statements in the
;; Racket source code as strings.  They can be defined in separate files where
;; they can be nicely formatted, commented and tested in the sqlite command
;; prompt.

(define-syntax (define-sql-statement stx)
  (syntax-case stx ()
    [(_ name path)
     ;; Unfortunately, PATH cannot be used directly here, as it will need to
     ;; be relative to the location of *this* file if running the program in
     ;; place, but it will need to be relative to the location of the file
     ;; calling this macro when building an executable.
     ;;
     ;; https://groups.google.com/forum/#!topic/racket-users/svMKLFh_VC4
     (let-values ([(dir fn _) (split-path (syntax-source stx))])
       (with-syntax ([dir (or dir 'same)])
         #'(begin
             (define-runtime-path rpath (build-path dir path))
             (define name
               (let ([vq #f])
                 (lambda ()
                   (unless vq
                     (let ([s (call-with-input-file rpath read-next-statement)])
                       (set! vq (virtual-statement (lambda (_) s)))))
                   vq))))))]))


;;...................................................... database backup ....

;; Implement a simple backup strategy for the activity log databases: we copy
;; the db-file to a backup directory at the start of every week.  There is
;; currently no recovery implemented, the user needs to copy and restore
;; backups himself.


;; Place where we store backups.
(define db-backup-dir
  (build-path (data-directory) "db-backup"))

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

(define (make-tag year month day (index #f))
  (if index
      (string-append
       "-"
       (~a year #:width 4 #:align 'right #:pad-string "0")
       "-"
       (~a month #:width 2 #:align 'right #:pad-string "0")
       "-"
       (~a day #:width 2 #:align 'right #:pad-string "0")
       "-"
       (~a index))
      (string-append
       "-"
       (~a year #:width 4 #:align 'right #:pad-string "0")
       "-"
       (~a month #:width 2 #:align 'right #:pad-string "0")
       "-"
       (~a day #:width 2 #:align 'right #:pad-string "0"))))
  
;; Return a backup file name for DB-FILE.  This is done by adding the week
;; start timestamp to the file name and making DB-BACKUP-DIR as the directory.
;; If UNIQUE-NAME is #t a unique database backup name will be generated.
(define (get-backup-file-name db-file unique-name)
  (let-values (((_1 file _2) (split-path db-file)))
    (let* ((week-start (seconds->date (week-start (current-seconds))))
           (year (date-year week-start))
           (month (date-month week-start))
           (day (date-day week-start))
           (fname (path->string file)))
      (if unique-name
          (let loop ((index 0))
            (let* ((tag (make-tag year month day index))
                   (bfile (regexp-replace #rx"\\..*$" fname (string-append tag "\\0")))
                   (path (build-path db-backup-dir bfile)))
              (if (file-exists? path)
                  (loop (add1 index))
                  path)))
          (let* ((tag (make-tag year month day))
                 (bfile (regexp-replace #rx"\\..*$" fname (string-append tag "\\0")))
                 (path (build-path db-backup-dir bfile)))
            path)))))

;; Copy a file from ORIGINAL to BACKUP.  To reduce the chance of failure, we
;; first copy the ORIGINAL into a temporary file in the same target folder
;; than rename the temporary to BACKUP.  This way, BACKUP will not exist as a
;; partial file, so if something fails, we will attempt make the backup again.
(define (backup-database original backup)
  (let-values (((dir file _2) (split-path backup)))
    (let ((tmp (build-path dir (string-append (path->string file) ".tmp"))))
      (copy-file original tmp)
      (rename-file-or-directory tmp backup))))

;; Make a backup of DB-FILE, if needed unless the FORCE-BACKUP is #t in which
;; case we always create a backup. When FORCE-BACKUP is #f, we only make one
;; backup a week, if it was already done for this week, we won't do it again.
;;
;; Also, we won't make a backup if the file is located in the backup dir
;; already.
(define (maybe-backup-database db-file [force-backup #f])
  (when (string? db-file) (set! db-file (string->path db-file)))
  (let-values (((dir _1 _2) (split-path db-file)))
    (let ((backup (get-backup-file-name db-file force-backup)))
      (when (and (file-exists? db-file)
                 (not (file-exists? backup))
                 (not (equal? dir db-backup-dir)))
        (dbglog "database backup into ~a..." backup)
        (backup-database db-file backup)
        (dbglog "database backup completed")))))


;;....................................................... sql-column-ref ....

;; Reference a column in the SQL result set ROW.  If the referenced value is
;; sql-null?, replace it with the value of IF-NULL (which defaults to #f).
;;
;; The ideea is that callers of SQL-COLUMN-REF don't have to worry about
;; sql-null objects as they will be replaced with #f
(define (sql-column-ref row col [if-null #f])
  (let ((v (vector-ref row col)))
    (if (sql-null? v) if-null v)))


;;....................................................... json utilities ....

;; Take a JSEXPR and return a compressed string from it.  It is intended to be
;; stored in the database.
(define (jsexpr->compressed-string jsexpr)
  (let* ((str (jsexpr->string jsexpr))
         (in (open-input-bytes (string->bytes/utf-8 str)))
         (out (open-output-bytes)))
    (gzip-through-ports in out #f 0)
    (get-output-bytes out)))

;; Convert a compressed string back into a JSEXPR
(define (compressed-string->jsexpr data)
  (let ((in (open-input-bytes data))
        (out (open-output-bytes)))
    (gunzip-through-ports in out)
    (let ((str (bytes->string/utf-8 (get-output-bytes out))))
      (string->jsexpr str))))

