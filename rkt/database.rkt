#lang racket
;; database.rkt -- database access utilities
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

(require db
         file/gunzip
         file/gzip
         racket/runtime-path
         "fit-defs.rkt"
         "fit-file.rkt"
         "utilities.rkt")

(provide db-import-activity-from-file)
(provide db-import-activities-from-directory)
(provide db-re-import-activity)
(provide db-insert-activity)
(provide db-fetch-activity)
(provide db-fetch-session)
(provide db-delete-session)
(provide db-delete-activity)
(provide db-delete-activity-hard)
(provide db-open-activity-log)
(provide maybe-create-schema)
(provide db-export-raw-data)
(provide db-get-last-pk)
(provide db-get-activity-id)
(provide db-get-seasons)
(provide db-get-schema-version)
(provide (struct-out db-exn-bad-db-version)
         db-exn-bad-db-version-message)


;................................................... database utilities ....

(define (rassq1 tag alist)
  (if tag
      (let ((v (findf (lambda (x) (eq? (cdr x) tag)) alist)))
        (cond (v (car v))
              (#t sql-null)))
      sql-null))

(define db-get-last-pk
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select seq from SQLITE_SEQUENCE where name = ?"))))
    (lambda (table-name db)
      (query-value db stmt table-name))))

(define (db-row->alist fields row)
  (let ((result '())
        (index 0))
    (for-each (lambda (field)
                (let ((data (vector-ref row index)))
                  (set! index (+ index 1))
                  (unless (sql-null? data)
                          (set! result (cons (cons field data) result)))))
              fields)
    (reverse result)))


;.................................................. create-new-database ....

;; Read the next SQL statement from PORT, an input port.  The statement is
;; assumed to be terminate by the #\; character.
(define (read-next-statement port)
  (let ((out (open-output-string))
        (in-string? #f))
    
    ;; Return the next character in the input stream PORT, collapsing all
    ;; whitespace to a single space and skipping all comments.  Comments start
    ;; with "--" and extend until the end of the line.  Strings are being
    ;; tracked for.
    (define (get-next-char)
      (let ((ch (read-char port)))
        
        (when (eqv? ch #\')
          (set! in-string? (not in-string?)))
        
        (cond ((eqv? ch eof) ch)
              
              ((and (char-whitespace? ch)
                    (let ((ch-next (peek-char port)))
                      (or (eqv? ch-next eof)
                          (char-whitespace? ch-next))))
               ;; Colapse all whitespace into one single space
               (get-next-char))
              
              ((and (not in-string?)
                    (eqv? ch #\-)
                    (eqv? (peek-char port) #\-))
               ;; This is a comment, skip it until end of line
               (for ((v (in-producer (lambda () (read-char port)) #\newline)))
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

(define (collect-statements-from-file file-name)
  (call-with-input-file file-name
    (lambda (input)
      (let loop ((statements '()))
        (if (eqv? (peek-char input) eof)
            (reverse statements)
            (let ((stmt (read-next-statement input)))
              (loop (if stmt (cons stmt statements) statements))))))))


;; Read SQL statements from FILE-NAME and run them against DB.  Statements are
;; executed for side effects (e.g CREATE TABLE)
(define (execute-statements-from-file file-name db)
  (call-with-input-file file-name
    (lambda (input)
      (define (loop)
        (unless (eqv? (peek-char input) eof)
          (let ((stmt (read-next-statement input)))
            (when stmt
              (query-exec db stmt)))
          (loop)))
      (loop))))

(define (maybe-create-schema database-file schema-file db [progress-callback #f])
  ;; A database that has no tables (no entries in SQLITE_MASTER) is considered
  ;; to be newly created.  In that case, we run the schema-file script on it
  ;; to create the initial database schema.
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
       (let* ((statements (collect-statements-from-file schema-file))
              (num-statements (length statements)))
         (for ([stmt statements]
               [n num-statements])
           (query-exec db stmt)
           (when progress-callback
             (progress-callback "Executing SQL statement..." (+ n 1) num-statements))))))))

(define-runtime-path *schema-file* "../sql/db-schema.sql")
(define schema-version 12)
(define (db-get-schema-version) schema-version)

(struct db-exn-bad-db-version (file expected actual))
(define (db-exn-bad-db-version-message e)
  (format
   "Bad schema version for ~a:~% expected versiom ~a, actual version ~a"
   (db-exn-bad-db-version-file e)
   (db-exn-bad-db-version-expected e)
   (db-exn-bad-db-version-actual e)))


(define (db-open-activity-log database-file [progress-callback #f])
  (let ((db (sqlite3-connect #:database database-file #:mode 'create)))
    (maybe-create-schema database-file *schema-file* db progress-callback)
    (query-exec db "pragma foreign_keys = on")
    (let ((sv (query-value db "select version from SCHEMA_VERSION")))
      (unless (and (number? sv) (= sv schema-version))
        (disconnect db)
        (raise (db-exn-bad-db-version database-file schema-version sv)))
    db)))


;................................................... db-insert-activity ....

(define db-get-activity-id
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select id from ACTIVITY where guid = ?"))))
    (lambda (guid db)
      (with-handlers (((lambda (e) #t) (lambda (e) #f)))
                     (query-value db stmt guid)))))

(define db-insert-activity
  (let ((stmt (virtual-statement 
               (lambda (dbsys)
                 "insert into ACTIVITY(start_time, guid) values (?, ?)"))))
    (lambda (activity db)
      (call-with-transaction
       db
       (lambda ()
         (let ((start-time (assq1 'start-time activity))
               (guid (assq1 'guid activity)))
           (query-exec db stmt start-time (or guid sql-null))
           (let ((activity-id (db-get-last-pk "ACTIVITY" db))
                 (sessions (assq 'sessions activity)))
             (when sessions
                   (for-each (lambda (session)
                               (db-insert-session session activity-id db))
                             (cdr sessions)))
             activity-id)))))))

(define db-insert-section-summary
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into SECTION_SUMMARY (
                   total_timer_time,
                   total_elapsed_time,
                   total_distance,
                   total_calories,
                   avg_speed,
                   max_speed,
                   avg_heart_rate,
                   max_heart_rate,
                   avg_cadence,
                   max_cadence,
                   total_cycles,
                   avg_cycle_distance,
                   total_ascent,
                   total_descent,
                   total_corrected_ascent,
                   total_corrected_descent,
                   swim_stroke_id,
                   avg_vertical_oscillation,
                   avg_stance_time,
                   avg_stance_time_percent,
                   avg_power,
                   max_power,
                   normalized_power,
                   left_right_balance,
                   avg_left_torque_effectiveness,
                   avg_right_torque_effectiveness,
                   avg_left_pedal_smoothness,
                   avg_right_pedal_smoothness,
                   avg_left_pco,
                   avg_right_pco,
                   avg_left_pp_start,
                   avg_left_pp_end,
                   avg_right_pp_start,
                   avg_right_pp_end,
                   avg_left_ppp_start,
                   avg_left_ppp_end,
                   avg_right_ppp_start,
                   avg_right_ppp_end) 
                 values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (fields `(total-timer-time total-elapsed-time
                                   total-distance total-calories avg-speed 
                                   max-speed avg-heart-rate max-heart-rate
                                   avg-cadence max-cadence
                                   total-cycles avg-cycle-distance
                                   total-ascent total-descent 
                                   total-corrected-ascent total-corrected-descent
                                   swim-stroke
                                   avg-vertical-oscillation avg-stance-time 
                                   avg-stance-time-percent
                                   avg-power max-power normalized-power
                                   left-right-balance
                                   avg-left-torque-effectiveness avg-right-torque-effectiveness
                                   avg-left-pedal-smoothness avg-right-pedal-smoothness
                                   avg-left-pco avg-right-pco
                                   avg-left-pp-start avg-left-pp-end
                                   avg-right-pp-start avg-right-pp-end
                                   avg-left-ppp-start avg-left-ppp-end
                                   avg-right-ppp-start avg-right-ppp-end)))
    (lambda (record db)
      (let ((values (map (lambda (x) 
                           (let ((y (if (procedure? x)
                                        (x record)
                                        (assq1 x record))))
                             (cond (y)
                                   ((void? y) sql-null)
                                   (#t sql-null))))
                         fields)))
        (apply query-exec db stmt values))
      (db-get-last-pk "SECTION_SUMMARY" db))))

(define db-insert-session
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into A_SESSION(activity_id, summary_id, name, description, 
                                        start_time, sport_id, sub_sport_id, pool_length, pool_length_unit, 
                                        training_effect, training_stress_score, intensity_factor)
                  values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))))
    (lambda (session activity-id db)
      (let ((summary-id (db-insert-section-summary session db))
            (name (or (assq1 'name session) sql-null))
            (description (or (assq1 'description session) sql-null))
            (start-time (or (assq1 'start-time session) sql-null))
            (sport-id (rassq1 (assq1 'sport session) *sport*))
            (sub-sport (rassq1 (assq1 'sub-sport session) *sub-sport*))
            (pool-length (or (assq1 'pool-length session) sql-null))
            (pool-length-unit (rassq1 (assq1 'pool-length-unit session) *pool-length-unit*))
            (training-effect (or (assq1 'total-training-effect session) sql-null))
            (training-stress-score (or (assq1 'training-stress-score session) sql-null))
            (intensity-factor (or (assq1 'intensity-factor session) sql-null)))
        
        (query-exec 
         db stmt activity-id summary-id name description 
         start-time sport-id sub-sport pool-length pool-length-unit training-effect
         training-stress-score intensity-factor))
      (let ((session-id (db-get-last-pk "A_SESSION" db))
            (laps (assq 'laps session))
            (devices (assq 'devices session)))

        (when laps
              (for-each (lambda (lap) (db-insert-lap lap session-id db))
                        (cdr laps)))
        (when devices
              (for-each (lambda (device)
                          (let ((device-id (db-insert-device device db)))
                            (when device-id
                              (db-insert-device-use session-id device-id db))))
                        (cdr devices)))))))

(define db-insert-lap
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into A_LAP(session_id, start_time, summary_id) values (?, ?, ?)"))))
    (lambda (lap session-id db)
      (let ((summary-id (db-insert-section-summary lap db))
            (start-time (or (assq1 'start-time lap) sql-null)))
        (query-exec db stmt session-id start-time summary-id))
      (let ((lap-id (db-get-last-pk "A_LAP" db))
            (lengths (assq 'lengths lap)))
      (when lengths
            (for-each (lambda (length) (db-insert-length length lap-id db))
                      (cdr lengths)))))))

(define db-insert-length
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into A_LENGTH(lap_id, start_time, summary_id) values (?, ?, ?)"))))
    (lambda (length session-id db)
      (let ((summary-id (db-insert-section-summary length db))
            (start-time (or (assq1 'start-time length) sql-null)))
        (query-exec db stmt session-id start-time summary-id))
      (let ((length-id (db-get-last-pk "A_LENGTH" db))
            (track (assq 'track length)))
        (when track
              (for-each (lambda (trackpoint) (db-insert-trackpoint trackpoint length-id db))
                        (cdr track)))))))

(define db-insert-trackpoint
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                "insert into A_TRACKPOINT (
                   length_id,
                   timestamp,
                   position_lat, position_long, altitude, distance,
                   cadence, speed, heart_rate,
                   vertical_oscillation, stance_time, stance_time_percent,
                   power, accumulated_power, left_right_balance, 
                   left_torque_effectiveness, right_torque_effectiveness,
                   left_pedal_smoothness, right_pedal_smoothness,
                   left_pco, right_pco, left_pp_start, left_pp_end, right_pp_start, right_pp_end,
                   left_ppp_start, left_ppp_end, right_ppp_start, right_ppp_end)
                 values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (fields
         `(timestamp position-lat position-long altitude distance cadence
                     speed heart-rate vertical-oscillation
                     stance-time stance-time-percent
                     power accumulated-power left-right-balance
                     left-torque-effectiveness right-torque-effectiveness
                     left-pedal-smoothness right-pedal-smoothness
                     left-pco right-pco
                     left-pp-start left-pp-end right-pp-start right-pp-end
                     left-ppp-start left-ppp-end right-ppp-start right-ppp-end)))
    (lambda (trackpoint length-id db)
      (let ((values (map (lambda (x) 
                           (let ((y (if (procedure? x)
                                        (x trackpoint)
                                        (assq1 x trackpoint))))
                             (cond (y)
                                   ((void? y) sql-null)
                                   (#t sql-null))))
                         fields)))
        ;; Bulk import ocasionally fails here...
        (with-handlers (((lambda (e) #t) 
                         (lambda (e)
                           (display (format "Failed to insert record: ~a, ~a~%" values e))
                           (raise e))))
                       (apply query-exec db stmt length-id values))))))

(define db-get-device-id 
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select id from EQUIPMENT where serial_number = ?")))
        (stmt2 (virtual-statement
               (lambda (dbsys)
                 "select id from EQUIPMENT where (serial_number % 65536) = ?"))))
    (lambda (serial-number db)
      (or (query-maybe-value db stmt serial-number)
          (query-maybe-value db stmt2 serial-number)))))

;; NOTE: devices are indexed by their serial number.  if the serial number is
;; already in the database, it won't be updated
(define db-insert-device
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into EQUIPMENT(name, device_name, manufacturer_id, device_id, serial_number)
                  values (?, ?, ?, ?, ?)"))))
    (lambda (device-info db)
      (let ((serial-number (or (assq1 'serial-number device-info)
                               (assq1 'ant-device-number device-info))))
        (cond ((not serial-number) #f)              ; no serial number
              ((db-get-device-id serial-number db)) ; already present
              (#t                                   ; need to add a new device
               (let ((name sql-null)
                     (device-name (fit-get-device-name device-info))
                     (manufacturer (let ((m (assq1 'manufacturer device-info)))
                                     (cond ((symbol? m) (rassq1 m *manfacturer*))
                                           ((number? m) m)
                                           (#t sql-null))))
                     (product (let ((p (assq1 'product device-info)))
                                (cond ((symbol? p) (rassq1 p *garmin-product*))
                                      ((number? p) p)
                                      (#t sql-null)))))
                 (if (void? device-name)
                     (begin
                       (display (format "No device name for ~a~%" device-info))
                       #f)
                     (begin
                       (query-exec db stmt name device-name manufacturer product serial-number)
                       (db-get-last-pk "EQUIPMENT" db))))))))))

(define db-insert-device-use
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into EQUIPMENT_USE(session_id, equipment_id) values (?, ?)")))
        (stmt-chk (virtual-statement
                   (lambda (dbsys)
                     "select count(*) from EQUIPMENT_USE where session_id = ? and equipment_id = ?"))))
    
    (lambda (session-id equipment-id db)
      ;; Don't insert duplicates.  The session's device list might contain
      ;; duplicates.
      (when (= (query-value db stmt-chk session-id equipment-id) 0)
            (query-exec db stmt session-id equipment-id)))))



;.......................................... db-insert-activtiy-raw-data ....

(define  db-insert-activity-raw-data
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                "insert into ACTIVITY_RAW_DATA (activity_id, file_name, data)
                 values(?, ?, ?)"))))
    (lambda (activity-id file-name data db)
      (let ((in (open-input-bytes data))
            (out (open-output-bytes)))
        (gzip-through-ports in out file-name 0)
        (query-exec db stmt activity-id file-name (get-output-bytes out))))))


;................................................... db-import-activity ....

(struct db-exn-activity-exists (guid))
(struct db-exn-retired-device (serial))

;; augument activity-builder% to raise an exception and exit early if the
;; activity already exists in the database.

(define db-fit-activity-builder%
  (class activity-builder%
    (init-field db)
    (super-new)

    (define/override (on-file-id file-id)
      (super on-file-id file-id)
      (let ((guid (send this get-guid)))
        (when (db-get-activity-id guid db)
          (raise (db-exn-activity-exists guid))))
      (let ((serial (assq1 'serial-number file-id)))
        (when serial
          (let ((retired? (query-maybe-value 
                           db 
                           "select retired from EQUIPMENT where serial_number = ?"
                           serial)))
            (when (and retired? (> retired? 0))
              (raise (db-exn-retired-device serial)))))))))


(define (db-import-activity-from-blob data file-name db)
  ;; NOTE: call with transaction will only intercept SQL exceptions and roll
  ;; back.  It look like any other exception will cause the block to be
  ;; escaped and transaction left open.
  (call-with-transaction
   db (lambda ()
        (with-handlers 
         ((db-exn-activity-exists? 
           (lambda (e)
             (let ((guid (db-exn-activity-exists-guid e)))
               (cons 'already-exists guid))))
          (db-exn-retired-device?
           (lambda (e)
             (let ((serial (db-exn-retired-device-serial e)))
               (cons 'retired-device serial))))
          ((lambda (e) (and (cons? e) (eq? (car e) 'fit-file-error)))
           (lambda (e) (cons 'failed (cdr e))))
          ((lambda (e) #t)
           (lambda (e) (cons 'failed e))))
         (let* ((fit-stream (make-fit-data-stream data))
                (activity (let ((consumer (new db-fit-activity-builder% [db db])))
                            (read-fit-records fit-stream consumer)
                            (send consumer collect-activity)))
                (aid (db-insert-activity activity db)))
           (db-insert-activity-raw-data 
            aid 
            (if (path? file-name) (path->string file-name) file-name)
            data
            db)
           (query-exec db "insert into LAST_IMPORT(activity_id) values (?)" aid)
           (cons 'ok aid))))))

(define (db-import-activity-from-file file-name db)
  (let ((data (file->bytes file-name #:mode 'binary))
        (base-file (let ((p (if (path? file-name) file-name (string->path file-name))))
                     (let-values (((dir file x) (split-path p)))
                       file))))
    (db-import-activity-from-blob data base-file db)))

(define (db-import-activities-from-directory path db [results-callback #f])
  (for ((file (in-directory path)))
    (when (regexp-match #rx"\\.FIT$" (string-upcase (path->string file)))
      (let ((result (db-import-activity-from-file file db)))
        (when results-callback
          (results-callback file (car result) (cdr result)))))))

(define (db-re-import-activity activity-id db)
  (let ((data (db-extract-activity-raw-data activity-id db))
        (file-name (query-value 
                    db "select file_name from ACTIVITY_RAW_DATA where activity_id = ?" 
                    activity-id)))
    (call-with-transaction
     db
     (lambda ()
       (db-delete-activity-hard activity-id db)
       (db-import-activity-from-blob data file-name db)))))

(define (db-export-raw-data activity-id db out-file-name)
  (let ((data (db-extract-activity-raw-data activity-id db)))
    (call-with-output-file out-file-name
      (lambda (port)
        (write-bytes data port)))))
  


;.................................................... db-fetch-activity ....

(define (db-extract-activity-raw-data activity-id db)
  (let ((data (query-value 
               db "select data from ACTIVITY_RAW_DATA where activity_id = ?"
               activity-id)))
    (let ((in (open-input-bytes data))
          (out (open-output-bytes)))
      (gunzip-through-ports in out)
      (get-output-bytes out))))

(define db-fetch-activity
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select id, start_time, guid from ACTIVITY where id = ?"))))
    (lambda (id db)
      (let ((row (query-row db stmt id)))
        (list (cons 'guid (vector-ref row 2))
              (cons 'database-id (vector-ref row 0))
              (cons 'start-time (vector-ref row 1))
              (cons 'sessions (db-extract-sessions-for-activity (vector-ref row 0) db)))))))

(define db-extract-sessions-for-activity
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select S.id,
                         S.start_time,
                         S.name,
                         S.description,
                         SS.total_timer_time,
                         SS.total_elapsed_time,
                         SS.total_distance,
                         SS.total_calories,
                         SS.avg_speed,
                         SS.max_speed,
                         SS.avg_heart_rate,
                         SS.max_heart_rate,
                         SS.avg_cadence,
                         SS.max_cadence,
                         SS.total_cycles,
                         SS.avg_cycle_distance,
                         SS.total_ascent,
                         SS.total_descent,
                         SS.swim_stroke_id,
                         S.sport_id,
                         S.sub_sport_id,
                         S.pool_length,
                         S.pool_length_unit,
                         SS.avg_vertical_oscillation,
                         SS.avg_stance_time,
                         SS.avg_stance_time_percent,
                         S.training_effect,
                         SS.avg_power,
                         SS.max_power,
                         SS.normalized_power,
                         SS.left_right_balance,
                         SS.avg_left_torque_effectiveness,
                         SS.avg_right_torque_effectiveness,
                         SS.avg_left_pedal_smoothness,
                         SS.avg_right_pedal_smoothness,
                         S.training_stress_score,
                         S.intensity_factor,
                         S.rpe_scale
                    from A_SESSION S, SECTION_SUMMARY SS
                   where S.summary_id = SS.id
                     and S.activity_id = ?
                   order by S.start_time"))))
    (lambda (activity-id db)
      (for/list ((session (in-list (query-rows db stmt activity-id))))
                (db-extract-session session db)))))

(define db-fetch-session
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select S.id,
                         S.start_time,
                         S.name,
                         S.description,
                         SS.total_timer_time,
                         SS.total_elapsed_time,
                         SS.total_distance,
                         SS.total_calories,
                         SS.avg_speed,
                         SS.max_speed,
                         SS.avg_heart_rate,
                         SS.max_heart_rate,
                         SS.avg_cadence,
                         SS.max_cadence,
                         SS.total_cycles,
                         SS.avg_cycle_distance,
                         SS.total_ascent,
                         SS.total_descent,
                         SS.total_corrected_ascent,
                         SS.total_corrected_descent,
                         SS.swim_stroke_id,
                         S.sport_id,
                         S.sub_sport_id,
                         S.pool_length,
                         S.pool_length_unit,
                         SS.avg_vertical_oscillation,
                         SS.avg_stance_time,
                         SS.avg_stance_time_percent,
                         S.training_effect,
                         SS.avg_power,
                         SS.max_power,
                         SS.normalized_power,
                         SS.left_right_balance,
                         SS.avg_left_torque_effectiveness,
                         SS.avg_right_torque_effectiveness,
                         SS.avg_left_pedal_smoothness,
                         SS.avg_right_pedal_smoothness,
                         S.training_stress_score,
                         S.intensity_factor,
                         S.rpe_scale,
                         SS.avg_left_pco,
                         SS.avg_right_pco,
                         SS.avg_left_pp_start,
                         SS.avg_left_pp_end,
                         SS.avg_right_pp_start,
                         SS.avg_right_pp_end,
                         SS.avg_left_ppp_start,
                         SS.avg_left_ppp_end,
                         SS.avg_right_ppp_start,
                         SS.avg_right_ppp_end
                    from A_SESSION S, SECTION_SUMMARY SS
                   where S.summary_id = SS.id
                     and S.id = ?"))))
    (lambda (session-id db)
      (db-extract-session (query-row db stmt session-id) db))))

(define (db-extract-session session-row db)
  (let ((fields '(database-id start-time name description total-timer-time total-elapsed-time 
                  total-distance total-calories avg-speed max-speed avg-heart-rate max-heart-rate
                  avg-cadence max-cadence total-cycles avg-cycle-distance total-ascent total-descent
                  total-corrected-ascent total-corrected-descent
                  swim-stroke sport sub-sport pool-length pool-length-unit
                  avg-vertical-oscillation avg-stance-time
                  avg-stance-time-percent total-training-effect
                  avg-power max-power normalized-power 
                  left-right-balance 
                  avg-left-torque-effectiveness avg-right-torque-effectiveness
                  avg-left-pedal-smoothness avg-right-pedal-smoothness
                  training-stress-score intensity-factor rpe-scale
                  avg-left-pco avg-right-pco
                  avg-left-pp-start avg-left-pp-end avg-right-pp-start avg-right-pp-end
                  avg-left-ppp-start avg-left-ppp-end avg-right-ppp-start avg-right-ppp-end)))
    (let ((session-data (db-row->alist fields session-row)))
      (cons (cons 'weather (db-extract-weater-for-session (vector-ref session-row 0) db))
            (cons (cons 'laps (db-extract-laps-for-session (vector-ref session-row 0) db))
                  session-data)))))

(define db-extract-laps-for-session
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select L.id,
                         L.start_time,
                         SS.total_timer_time,
                         SS.total_elapsed_time,
                         SS.total_distance,
                         SS.total_calories,
                         SS.avg_speed,
                         SS.max_speed,
                         SS.avg_heart_rate,
                         SS.max_heart_rate,
                         SS.avg_cadence,
                         SS.max_cadence,
                         SS.total_cycles,
                         SS.avg_cycle_distance,
                         SS.total_ascent,
                         SS.total_descent,
                         SS.total_corrected_ascent,
                         SS.total_corrected_descent,
                         SS.swim_stroke_id,
                         SS.avg_vertical_oscillation,
                         SS.avg_stance_time,
                         SS.avg_stance_time_percent,
                         SS.avg_power,
                         SS.max_power,
                         SS.normalized_power,
                         SS.left_right_balance,
                         SS.avg_left_torque_effectiveness,
                         SS.avg_right_torque_effectiveness,
                         SS.avg_left_pedal_smoothness,
                         SS.avg_right_pedal_smoothness,
                         SS.avg_left_pco,
                         SS.avg_right_pco,
                         SS.avg_left_pp_start,
                         SS.avg_left_pp_end,
                         SS.avg_right_pp_start,
                         SS.avg_right_pp_end,
                         SS.avg_left_ppp_start,
                         SS.avg_left_ppp_end,
                         SS.avg_right_ppp_start,
                         SS.avg_right_ppp_end
                    from A_LAP L, SECTION_SUMMARY SS
                   where L.summary_id = SS.id
                     and L.session_id = ?
                  order by L.start_time"))))
    (lambda (session-id db)
      (for/list ((lap (in-list (query-rows db stmt session-id))))
                (db-extract-lap lap db)))))

(define (db-extract-lap lap-row db)
  (let ((fields '(database-id start-time total-timer-time total-elapsed-time 
                  total-distance total-calories avg-speed max-speed avg-heart-rate max-heart-rate
                  avg-cadence max-cadence total-cycles avg-cycle-distance total-ascent total-descent
                  total-corrected-ascent total-corrected-descent
                  swim-stroke avg-vertical-oscillation avg-stance-time
                  avg-stance-time-percent
                  avg-power max-power normalized-power
                  left-right-balance
                  avg-left-torque-effectiveness avg-right-torque-effectiveness
                  avg-left-pedal-smoothness avg-right-pedal-smoothness
                  avg-left-pco avg-right-pco
                  avg-left-pp-start avg-left-pp-end avg-right-pp-start avg-right-pp-end
                  avg-left-ppp-start avg-left-ppp-end avg-right-ppp-start avg-right-ppp-end)))

    (let ((lap-data (db-row->alist fields lap-row)))
      (cons (cons 'lengths (db-extract-lengths-for-lap (vector-ref lap-row 0) db))
            lap-data))))

(define db-extract-lengths-for-lap
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select L.id,
                         L.start_time,
                         SS.total_timer_time,
                         SS.total_elapsed_time,
                         SS.total_distance,
                         SS.total_calories,
                         SS.avg_speed,
                         SS.max_speed,
                         SS.avg_heart_rate,
                         SS.max_heart_rate,
                         SS.avg_cadence,
                         SS.max_cadence,
                         SS.total_cycles,
                         SS.avg_cycle_distance,
                         SS.total_ascent,
                         SS.total_descent,
                         SS.total_corrected_ascent,
                         SS.total_corrected_descent,
                         SS.swim_stroke_id,
                         SS.avg_vertical_oscillation,
                         SS.avg_stance_time,
                         SS.avg_stance_time_percent,
                         SS.avg_power,
                         SS.max_power,
                         SS.normalized_power,
                         SS.left_right_balance,
                         SS.avg_left_torque_effectiveness,
                         SS.avg_right_torque_effectiveness,
                         SS.avg_left_pedal_smoothness,
                         SS.avg_right_pedal_smoothness,
                         SS.avg_left_pco,
                         SS.avg_right_pco,
                         SS.avg_left_pp_start,
                         SS.avg_left_pp_end,
                         SS.avg_right_pp_start,
                         SS.avg_right_pp_end,
                         SS.avg_left_ppp_start,
                         SS.avg_left_ppp_end,
                         SS.avg_right_ppp_start,
                         SS.avg_right_ppp_end
                    from A_LENGTH L, SECTION_SUMMARY SS
                   where L.summary_id = SS.id
                     and L.lap_id = ?
                   order by L.start_time"))))
    (lambda (lap-id db)
      (for/list ((length (in-list (query-rows db stmt lap-id))))
                (db-extract-length length db)))))

(define (db-extract-length length-row db)
  (let ((fields '(database-id start-time total-timer-time total-elapsed-time 
                  total-distance total-calories avg-speed max-speed avg-heart-rate max-heart-rate
                  avg-cadence max-cadence total-cycles avg-cycle-distance total-ascent total-descent
                  total-corrected-ascent total-corrected-descent
                  swim-stroke avg-vertical-oscillation avg-stance-time
                  avg-stance-time-percent
                  avg-power max-power normalized-power
                  avg-left-torque-effectiveness avg-right-torque-effectiveness
                  avg-left-pedal-smoothness avg-right-pedal-smoothness
                  avg-left-pco avg-right-pco
                  avg-left-pp-start avg-left-pp-end avg-right-pp-start avg-right-pp-end
                  avg-left-ppp-start avg-left-ppp-end avg-right-ppp-start avg-right-ppp-end)))

    (let ((length-data (db-row->alist fields length-row)))
      (cons (cons 'track (db-extract-trackpoints-for-length (vector-ref length-row 0) db))
            length-data))))

(define db-extract-trackpoints-for-length
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select T.id,
                         T.timestamp,
                         T.position_lat,
                         T.position_long,
                         T.altitude,
                         T.corrected_altitude,
                         T.distance,
                         T.cadence,
                         T.speed,
                         T.heart_rate,
                         T.vertical_oscillation,
                         T.stance_time,
                         T.stance_time_percent,
                         T.power,
                         T.accumulated_power,
                         T.left_right_balance,
                         T.left_torque_effectiveness,
                         T.right_torque_effectiveness,
                         T.left_pedal_smoothness,
                         T.right_pedal_smoothness,
                         T.left_pco,
                         T.right_pco,
                         T.left_pp_start,
                         T.left_pp_end,
                         T.right_pp_start,
                         T.right_pp_end,
                         T.left_ppp_start,
                         T.left_ppp_end,
                         T.right_ppp_start,
                         T.right_ppp_end
                    from A_TRACKPOINT T
                   where T.length_id = ?
                   order by T.timestamp"))))
    (lambda (length-id db)
      (for/list ((trackpoint (in-list (query-rows db stmt length-id))))
                (db-extract-trackpoint trackpoint)))))

(define (db-extract-trackpoint trackpoint-row)
  (let ((fields '(database-id timestamp position-lat position-long 
                              altitude corrected-altitude distance cadence speed 
                              heart-rate vertical-oscillation 
                              stance-time stance-time-percent
                              power accumulated-power left-right-balance
                              left-torque-effectiveness right-torque-effectiveness
                              left-pedal-smoothness right-pedal-smoothness
                              left-pco right-pco
                              left-pp-start left-pp-end right-pp-start right-pp-end
                              left-ppp-start left-ppp-end right-ppp-start right-ppp-end)))
    (db-row->alist fields trackpoint-row)))

(define db-extract-weater-for-session
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select id, wstation, temperature, dew_point, humidity, 
                         wind_speed, wind_gusts, wind_direction, pressure
                  from SESSION_WEATHER
                  where session_id = ?"))))
    (lambda (session-id db)
      (let ((fields '(database-id source temperature dew-point humidity 
                                  wind-speed wind-gusts wind-direction pressure))
            (row (query-maybe-row db stmt session-id)))
        (if row (db-row->alist fields row) '())))))
    


;................................................... db-delete-sesssion ....

(define db-delete-session
  (let ((del-equipment-use
         (virtual-statement
          (lambda (dbsys) "delete from EQUIPMENT_USE where session_id = ?")))
        (del-time-in-zone
         (virtual-statement
          (lambda (dbsys) "delete from TIME_IN_ZONE where session_id = ?")))
        (del-section-summary
         (virtual-statement
          (lambda (dbsys) "delete from SECTION_SUMMARY where id in (
                             select summary_id from A_SESSION where id = ?)")))
        (del-trackpoints
         (virtual-statement
          (lambda (dbsys) "delete from A_TRACKPOINT where length_id in (
                             select id from A_LENGTH where lap_id in (
                               select id from A_LAP where session_id = ?))")))
        (del-lengths
         (virtual-statement
          (lambda (dbsys) "delete from A_LENGTH where lap_id in (
                             select id from A_LAP where session_id = ?)")))
        (del-laps
         (virtual-statement
          (lambda (dbsys) "delete from A_LAP where session_id = ?")))
        (del-labels
         (virtual-statement
          (lambda (dbsys) "delete from SESSION_LABEL where session_id = ?")))
        (del-weather
         (virtual-statement
          (lambda (dbsys) "delete from SESSION_WEATHER where session_id = ?")))
        (del-session
         (virtual-statement
          (lambda (dbsys) "delete from A_SESSION where id = ?"))))
    (lambda (session-id db)
      (call-with-transaction
       db (lambda ()
            (query-exec db del-equipment-use session-id)
            (query-exec db del-time-in-zone session-id)
            (query-exec db del-section-summary session-id)
            (query-exec db del-trackpoints session-id)
            (query-exec db del-lengths session-id)
            (query-exec db del-laps session-id)
            (query-exec db del-labels session-id)
            (query-exec db del-weather session-id)
            (query-exec db del-session session-id))))))



;................................................... db-delete-activtiy ....

;; Delete all sessions for this activity.  Effectively, this activtiy will no
;; longer show up in any reports and won't be imported again when scannig
;; folders for FIT files.  The activtiy can be re-imported from data in
;; ACTIVTIY_RAW_DATA, if needed.
(define db-delete-activity 
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "select S.id from A_SESSION S where S.activity_id = ?"))))
  (lambda (activity-id db)
    (call-with-transaction
     db (lambda ()
          (map (lambda (session-id)
                 (db-delete-session session-id db))
               (query-list db stmt activity-id))
          #t)))))

;; Really delete the activtiy from the database.  All data for this activity
;; is lost.  It will be however be re-imported if the FIT file is seen again
;; by `db-import-activities-from-directory'
(define db-delete-activity-hard
  (let ((del-activtiy
         (virtual-statement 
          (lambda (dbsys) "delete from ACTIVITY where id = ?")))
        (del-last-import
         (virtual-statement
          (lambda (dbsys) "delete from LAST_IMPORT where activity_id = ?")))
        (del-activity-raw-data
         (virtual-statement
          (lambda (dbsys) "delete from ACTIVITY_RAW_DATA where activity_id = ?")))
        (sel-sessions
         (virtual-statement
          (lambda (dbsys) "select id from A_SESSION where activity_id = ?"))))
    (lambda (activity-id db)
      (call-with-transaction 
       db (lambda ()
            (let ((sessions (query-list db sel-sessions activity-id)))
              (for-each (lambda (session) (db-delete-session session db)) sessions))
            (query-exec db del-activity-raw-data activity-id)
            (query-exec db del-last-import activity-id)
            (query-exec db del-activtiy activity-id))))))

(define (db-get-seasons db)
  (query-rows db "select name, start_date, end_date from SEASON order by name"))
