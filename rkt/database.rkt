#lang racket/base
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

(require db/base
         file/gunzip
         file/gzip
         racket/class
         racket/file
         racket/list
         racket/dict
         racket/contract
         racket/match
         racket/async-channel
         "dbutil.rkt"
         "dbapp.rkt"
         "fit-file/fit-defs.rkt"
         "fit-file/fit-file.rkt"
         "elevation-correction.rkt"     ; for lat-lon->tile-code
         "utilities.rkt"
         )

(provide db-import-activity-from-file)
(provide db-import-activities-from-directory)
(provide db-re-import-activity)
(provide db-insert-activity)
(provide db-fetch-activity)
(provide db-fetch-session)
(provide db-delete-session)
(provide db-delete-activity)
(provide db-delete-activity-hard)
(provide db-export-raw-data)
(provide db-get-activity-id)
(provide db-get-seasons)
(provide db-extract-activity-raw-data)

(provide/contract
 (get-session-start-time (-> exact-nonnegative-integer? (or/c number? #f))))


;................................................... database utilities ....

(define (rassq1 tag alist)
  (if tag
      (let ((v (findf (lambda (x) (eq? (cdr x) tag)) alist)))
        (cond (v (car v))
              (#t sql-null)))
      sql-null))

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
         (let ((start-time (dict-ref activity 'start-time #f))
               (guid (dict-ref activity 'guid #f)))
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
                                        (dict-ref record x #f))))
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
            (name (dict-ref session 'name sql-null))
            (description (dict-ref session 'description sql-null))
            (start-time (dict-ref session 'start-time sql-null))
            (sport-id (rassq1 (dict-ref session 'sport #f) *sport*))
            (sub-sport (rassq1 (dict-ref session 'sub-sport #f) *sub-sport*))
            (pool-length (dict-ref session 'pool-length sql-null))
            (pool-length-unit (rassq1 (dict-ref session 'pool-length-unit #f) *pool-length-unit*))
            (training-effect (dict-ref session 'total-training-effect sql-null))
            (training-stress-score (dict-ref session 'training-stress-score sql-null))
            (intensity-factor (dict-ref session 'intensity-factor sql-null)))

        ;; HACK: Garmin devices started putting in 0 for the sub-sport field,
        ;; but the rest of the activity-log code relies on NULL to mean
        ;; 'generic'.  This introduced a subtle bug when using the subsport to
        ;; find similar activities (for best-avg) or settings for the
        ;; inspector, as it would look for activities like #(1 0) instead of
        ;; #(1 #f), the situation would magically fix itself when editing the
        ;; head line for the session, as that saved the sub-sport correctly.
        (when (equal? sub-sport 0) (set! sub-sport sql-null))
        
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
          (for ([di (make-devinfo-list (cdr devices))])
            (put-devinfo db di session-id)))))))

(define db-insert-lap
  (let ((stmt (virtual-statement
               (lambda (dbsys)
                 "insert into A_LAP(session_id, start_time, summary_id) values (?, ?, ?)"))))
    (lambda (lap session-id db)
      (let ((summary-id (db-insert-section-summary lap db))
            (start-time (dict-ref lap 'start-time sql-null)))
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
            (start-time (dict-ref length 'start-time sql-null)))
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
                   left_ppp_start, left_ppp_end, right_ppp_start, right_ppp_end, tile_code)
                 values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (fields
         `(timestamp position-lat position-long altitude distance cadence
                     speed heart-rate vertical-oscillation
                     stance-time stance-time-percent
                     power accumulated-power left-right-balance
                     left-torque-effectiveness right-torque-effectiveness
                     left-pedal-smoothness right-pedal-smoothness
                     left-pco right-pco
                     left-pp-start left-pp-end right-pp-start right-pp-end
                     left-ppp-start left-ppp-end right-ppp-start right-ppp-end
                     ,(lambda (tp)
                        (let ((lat (dict-ref tp 'position-lat #f))
                              (lon (dict-ref tp 'position-long #f)))
                          (and lat lon (lat-lon->tile-code lat lon)))))))
    (lambda (trackpoint length-id db)
      (let ((values (map (lambda (x) 
                           (let ((y (if (procedure? x)
                                        (x trackpoint)
                                        (dict-ref trackpoint x #f))))
                             (cond ((void? y) sql-null)
                                   (y)
                                   (#t sql-null))))
                         fields)))
        ;; Bulk import ocasionally fails here...
        (with-handlers (((lambda (e) #t) 
                         (lambda (e)
                           (display (format "Failed to insert record: ~a, ~a~%" values e))
                           (raise e))))
                       (apply query-exec db stmt length-id values))))))


;;............................................ Equipment / Device import ....

;; Hold information about a device reported in the FIT file
(struct devinfo (ts sn manufacturer product name swver hwver bv bs)
  #:transparent)

;; Parse a device-info ALIST (as produced by the 'read-activity-from-file')
;; and construct a devinfo structure from it.
(define (make-devinfo alist)
  (let ((name (fit-get-device-name alist))
        (ts (assq 'timestamp alist))
        (sn (or (assq 'serial-number alist) (assq 'ant-device-number alist)))
        (manufacturer (assq 'manufacturer alist))
        (product (assq 'product alist))
        (swver (assq 'software-version alist))
        (hwver (assq 'hardware-version alist))
        (bv (assq 'battery-voltage alist))
        (bs (assq 'battery-status alist)))
    (devinfo (and ts (cdr ts))
             (and sn (cdr sn))
             (and manufacturer (cdr manufacturer))
             (and product (cdr product))
             name
             (and swver (cdr swver))
             (and hwver (cdr hwver))
             (and bv (cdr bv))
             (and bs (cdr bs)))))

;; Join two devinfo structures (we assume for the same serial number).  When
;; both structures contain a field, the devinfo with the most recent timestamp
;; is used.
(define (join-devinfo d1 d2)
  (if (< (devinfo-ts d1) (devinfo-ts d2))
      (join-devinfo d2 d1)
      (devinfo
        (devinfo-ts d1)
        (devinfo-sn d1)
        (or (devinfo-manufacturer d1) (devinfo-manufacturer d2))
        (or (devinfo-product d1) (devinfo-product d2))
        (or (devinfo-name d1) (devinfo-name d2))
        (or (devinfo-swver d1) (devinfo-swver d2))
        (or (devinfo-hwver d1) (devinfo-hwver d2))
        (or (devinfo-bv d1) (devinfo-bv d2))
        (or (devinfo-bs d1) (devinfo-bs d2)))))

;; Parse a devices list (list of device-info ALISTS) as produced by
;; 'read-activity-from-file' and return a list of devinfo structures.  Note
;; that the list contains multiple entries for the same device, we merge these
;; together and return a single list, with one entry for each device.
(define (make-devinfo-list devices)
  (let ((result (make-hash)))
    (for ([d devices])
      (let ((di (make-devinfo d)))
        (when (devinfo-sn di)
          (hash-update!
           result
           (devinfo-sn di)
           (lambda (prev)
             (if prev (join-devinfo prev di) di))
           #f))))
    (for/list ([v (in-hash-values result)]) v)))

(define stmt-get-device-id
  (virtual-statement
   (lambda (dbsys)
     "select id from EQUIPMENT where serial_number = ?")))

;; Some older devices reported only the bottom 16bits of the serial number, so
;; we look for that as well.
(define stmt-get-device-id-16bit
  (virtual-statement
   (lambda (dbsys)
     ;; NOTE: when using the lower 16 bits of the equipment serial, we might
     ;; have duplicates, in that case, just select the biggest serial number.
     "select id
       from EQUIPMENT
      where serial_number = (
        select max(E1.serial_number)
          from EQUIPMENT E1
         where (E1.serial_number % 65536) = ?)")))

;; Get the EQUIPMENT.id for a serial number, SN, or #f if not found.
(define (dev-id-from-sn db sn)
  (or (query-maybe-value db stmt-get-device-id sn)
      (query-maybe-value db stmt-get-device-id-16bit sn)))

(define stmt-put-equipment
  (virtual-statement
   (lambda (dbsys)
     "insert into EQUIPMENT(device_name, manufacturer_id, device_id, serial_number)
      values (?, ?, ?, ?)")))

(define stmt-put-equipment-ver
  (virtual-statement
   (lambda (dbsys)
     "insert into EQUIPMENT_VER(equipment_id, timestamp, software_version,
                                hardware_version, battery_voltage, battery_status)
      values(?, ?, ?, ?, ?, ?)")))

(define stmt-check-equipment-use
  (virtual-statement
   (lambda (dbsys)
     "select count(*) from EQUIPMENT_USE where session_id = ? and equipment_id = ?")))
(define stmt-put-equipment-use
  (virtual-statement
   (lambda (dbsys)
     "insert into EQUIPMENT_USE(session_id, equipment_id) values (?, ?)")))

(define (bs->num bs)
  (case bs
    ((new) 1)
    ((good) 2)
    ((ok) 3)
    ((low) 4)
    ((critical) 5)
    ((charging) 6)
    (else 7)))

;; Put a device info strucure in the database: add entries to EQUIPMENT,
;; EQUIPMENT_VER and EQUIPMENT_USE as needed.
(define (put-devinfo db di sid)
  (let ((id (dev-id-from-sn db (devinfo-sn di))))
    (unless id                 ; if it is already there, don't put a new entry
      (let ((manufacturer (let ((m (devinfo-manufacturer di)))
                            (cond ((symbol? m) (rassq1 m *manfacturer*))
                                  ((number? m) m)
                                  (#t sql-null))))
            (product (let ((p (devinfo-product di)))
                       (cond ((symbol? p) (rassq1 p *garmin-product*))
                             ((number? p) p)
                             (#t sql-null)))))
        (query-exec db stmt-put-equipment (devinfo-name di) manufacturer product (devinfo-sn di))
        (set! id (db-get-last-pk "EQUIPMENT" db))))
    ;; Put an entry into "EQUIPMENT_VER" if it does not exist, or its
    ;; timestamp is older than ours.
    (let ((row (query-maybe-row db "select id, timestamp from EQUIPMENT_VER where equipment_id = ?" id)))
      (when (or (not row) (< (vector-ref row 1) (devinfo-ts di)))
        (when row
          (query-exec db "delete from EQUIPMENT_VER where id = ?" (vector-ref row 0)))
        (query-exec db stmt-put-equipment-ver
                    id (devinfo-ts di)
                    (or (devinfo-swver di) sql-null)
                    (or (devinfo-hwver di) sql-null)
                    (or (devinfo-bv di) sql-null)
                    (bs->num (devinfo-bs di)))))

    (when (= (query-value db stmt-check-equipment-use sid id) 0)
      (query-exec db stmt-put-equipment-use sid id))

    id))

(define (put-devinfo-list db devinfo-list sid)
  (for ([d devinfo-list])
    (put-devinfo db d sid)))

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
      (let ((serial (dict-ref file-id 'serial-number #f)))
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
  (let ((data (query-maybe-value 
               db "select data from ACTIVITY_RAW_DATA where activity_id = ?"
               activity-id)))
    (if data
        (let ((in (open-input-bytes data))
              (out (open-output-bytes)))
          (gunzip-through-ports in out)
          (get-output-bytes out))
        #f)))

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
                         SS.avg_right_ppp_end,
                         SS.aerobic_decoupling
                    from A_SESSION S, SECTION_SUMMARY SS
                   where S.summary_id = SS.id
                     and S.activity_id = ?
                   order by S.start_time"))))
    (lambda (activity-id db)
      (for/list ((session (in-list (query-rows db stmt activity-id))))
                (db-extract-session session db)))))

(define fetch-session-stmt
  (virtual-statement
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
            SS.avg_right_ppp_end,
            SS.aerobic_decoupling,
            (select SH.sdnn
               from SESSION_HRV SH
               where SH.session_id = S.id) as hrv
       from A_SESSION S, SECTION_SUMMARY SS
      where S.summary_id = SS.id
        and S.id = ?")))

(define (db-fetch-session session-id db)
  (db-extract-session (query-row db fetch-session-stmt session-id) db))

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
                  avg-left-ppp-start avg-left-ppp-end avg-right-ppp-start avg-right-ppp-end aerobic-decoupling hrv)))
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
                         SS.avg_right_ppp_end,
                         SS.aerobic_decoupling
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
                  avg-left-ppp-start avg-left-ppp-end avg-right-ppp-start avg-right-ppp-end aerobic-decoupling)))

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
      (cons (cons 'track ;(db-extract-trackpoints-for-length (vector-ref length-row 0) db)
                  '()
                  )
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
        (del-bavg-cache
         (virtual-statement
          (lambda (dbsys) "delete from BAVG_CACHE where session_id = ?")))
        (del-hist-cache
         (virtual-statement
          (lambda (dbsys) "delete from HIST_CACHE where session_id = ?")))
        (del-scatter-cache
         (virtual-statement
          (lambda (dbsys) "delete from SCATTER_CACHE where session_id = ?")))
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
            (query-exec db del-bavg-cache session-id)
            (query-exec db del-hist-cache session-id)
            (query-exec db del-scatter-cache session-id)
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


;;............................................... get-session-start-time ....

(define sid-timestamp-cache (make-hash))
(define sid-timestamp-query
  (virtual-statement
   (lambda (dbsys)
     "select start_time from A_SESSION where id = ?")))

;; Clear the timestamp cache when the database changes
(add-db-open-callback (lambda (c) (set! sid-timestamp-cache (make-hash))))

;; Clear individual entries when sessions change or are deleted -- no need to
;; handle session-created notifications, as they will be added to the cache
;; when they are first requested.
(define dummy
  (let ((s (make-log-event-source)))
    (thread/dbglog
     #:name "session df change processor sid-timestamp-cache"
     (lambda ()
       (let loop ((item (async-channel-get s)))
         (when item
           (match-define (list tag data) item)
           (when (member tag '(session-updated session-deleted))
             (hash-remove! sid-timestamp-cache data))
           (loop (async-channel-get s))))))))

;; Return the start time of the session SID, of #f if no such session exists
;; in the database.  This should be an efficient function to call, as the
;; results are cached locally.
(define (get-session-start-time sid)
  (let ((ts (hash-ref sid-timestamp-cache sid #f)))
    (unless ts
      (set! ts (query-maybe-value (current-database) sid-timestamp-query sid)))
    (when ts
      (hash-set! sid-timestamp-cache sid ts))
    ts))
