#lang racket/base
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2018-2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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

;;;; Commentary

;; This code tests that the application can import correctly a variety of FIT
;; files and that session data frames can be created from them with the
;; correct data.  The FIT files are a collection of various "oddities" with
;; faulty data and unusual data plus files containing XDATA series.  It
;; requires fit files to be present in the `test-fit` folder, these are
;; downloaded by the `.travis/download-test-data.sh` script.

(require al2-test-runner
         data-frame
         db
         racket/dict
         rackunit
         "../rkt/database.rkt"
         "../rkt/fit-file/activity-util.rkt"
         "../rkt/fit-file/fit-file.rkt"
         "../rkt/models/ec-util.rkt"
         "../rkt/session-df/native-series.rkt"
         "../rkt/session-df/session-df.rkt"
         "../rkt/utilities.rkt"
         "test-util.rkt")

;; Copied from fit-file.rkt, for testing purposes
(define (get-start-time record)
  (or (dict-ref record 'start-time #f)
      (dict-ref record 'timestamp #f)))

;; Copied from fit-file.rkt for testing purposes
(define (get-start-end-times record)
  (define start-time (get-start-time record))
  (define duration
    (let ([timer (dict-ref record 'total-timer-time #f)]
          [elapsed (dict-ref record 'total-elapsed-time #f)])
      (cond ((and timer elapsed) (max timer elapsed))
            (timer)
            (elapsed)
            (#t #f))))
  (define end-time
    (or
     (and start-time duration (+ start-time duration))
     (dict-ref record 'timestamp #f)))
  (values start-time end-time))

(define (do-basic-checks file series-count row-count
                         #:expected-session-count (expected-session-count 1)
                         #:extra-db-checks (extra-db-checks #f)
                         #:extra-df-checks (extra-df-checks #f))

  (unless (file-exists? file)
    (skip-test))

  (dbglog "File ~a, ~a data-points ..." file row-count)
  ;; Simple consistency check: if we expect more than one session,
  ;; SERIES-COUNT and ROW-COUNT should be lists, one for each series.
  (check-true (if (number? series-count)
                  (= expected-session-count 1)
                  (= expected-session-count (length series-count))))
  (check-true (if (number? row-count)
                  (= expected-session-count 1)
                  (= expected-session-count (length row-count))))
  (with-fresh-database
    (lambda (db)
      (db-import-activity-from-file/check
       file db
       #:expected-session-count expected-session-count
       #:expected-row-count row-count
       #:expected-series-count series-count
       #:extra-db-checks extra-db-checks
       #:extra-df-checks extra-df-checks
       #:delete-sessions? #t))))

(define (do-multi-checks files
                         #:extra-db-checks (extra-db-checks #f))
  (unless (for/and ([f (in-list files)]) (file-exists? f))
    (skip-test))

  (dbglog "File multi-checks on ~a ..." files)
  (with-fresh-database
    (lambda (db)
      (for ([f (in-list files)])
        (db-import-activity-from-file f db))
      (when extra-db-checks
        (extra-db-checks db)))))

(define (check-run-power df)
  (when (equal? (df-get-property df 'sport #f) #(1 #f))
    (check-true (df-contains? df "pwr"))
    #;(df-describe df)
    ))

(define (check-xdata-app-count db n)
  (check = n (query-value db "select count(*) from XDATA_APP")
         "xdata app count"))

(define (check-xdata-app-present db app-guid)
  (check = 1 (query-value db "select count(*) from XDATA_APP where app_guid = ?" app-guid)
         (format "xdata app present: ~a" app-guid)))

(define (check-xdata-field-count db app-id n)
  (check = n (query-value db "select count(*) from XDATA_FIELD where app_id = (select id from XDATA_APP where app_guid = ?)" app-id)
         "xdata field count"))

(define (check-xdata-field-present db app-guid field-name)
  (check = 1 (query-value db "
select count(*)
from XDATA_FIELD XF, XDATA_APP XA
where XA.app_guid = ?
  and XF.app_id = XA.id
and XF.name = ?" app-guid field-name)
         (format "xdata field present, app ~a, field ~a" app-guid field-name)))

(define (check-xdata-trackpoint-values db app-guid field-name)
  (check < 0 (query-value db "
select count(*)
 from XDATA_VALUE XV, XDATA_FIELD XF, XDATA_APP XA
 where XV.field_id = XF.id and XF.app_id = XA.id
   and XA.app_guid = ?
   and XF.name = ?"
                          app-guid field-name)
         (format "xdata-has-trackpoint-values app ~a, field ~a" app-guid field-name)))

(define (check-xdata-summary-values db app-guid field-name)
  (check < 0 (query-value db "
select count(*)
 from XDATA_SUMMARY_VALUE XSV, XDATA_FIELD XF, XDATA_APP XA
 where XSV.field_id = XF.id and XF.app_id = XA.id
   and XA.app_guid = ?
   and XF.name = ?"
                          app-guid field-name)
         (format "xdata-has-summary-values app ~a, field ~a" app-guid field-name)))


(define (check-stryd-xdata db)
  (define app-id "660a581e5301460c8f2f034c8b6dc90f")
  ;; (check-xdata-app-count db 1)
  (check-xdata-app-present db app-id)
  (check-xdata-field-count db app-id 7)
  (for ([field '("Leg Spring Stiffness"
                 "Form Power" "Elevation" "Vertical Oscillation"
                 "Ground Time" "Cadence" "Power")])
    (check-xdata-field-present db app-id field))

  (for ([field '("Leg Spring Stiffness"
                 "Form Power" "Elevation" "Vertical Oscillation"
                 "Ground Time" "Cadence" "Power")])
    (check-xdata-trackpoint-values db app-id field)))

(define (check-garmin-run-power-data db)
  (define app-id "741afa11025048e286b514bd47e29391")
  (check-xdata-app-count db 1)
  (check-xdata-app-present db app-id)
  (check-xdata-field-count db app-id 5)
  ;; NOTE: the RP_SD field is an array of 5 values, see issue #42, we
  ;; currently discard this field, as it is unclear what to do with it.
  (for ([field '("RP_SD" "RP_WindEnabled" "RP_AvgPower" "RP_AvgLapPower" "RP_Power")])
    (check-xdata-field-present db app-id field))
  (check-xdata-trackpoint-values db app-id "RP_Power"))

(define (check-outdoorsports-xdata db)
  (check-xdata-app-count db 1)
  (check-xdata-app-present db "27dfb7e5900f4c2d80abc57015f42124")
  (check-xdata-field-count db "27dfb7e5900f4c2d80abc57015f42124" 9)
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "total_caloriesAV")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "total_calories")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "BatteryUsed")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "avg_cadence")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "Time")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "Latitude")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "Longitude")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "eE")
  (check-xdata-field-present db "27dfb7e5900f4c2d80abc57015f42124" "StrideDistance")

  (check-xdata-trackpoint-values
   db "27dfb7e5900f4c2d80abc57015f42124" "eE")
  (check-xdata-trackpoint-values
   db "27dfb7e5900f4c2d80abc57015f42124" "StrideDistance")

  (check-xdata-summary-values
   db "27dfb7e5900f4c2d80abc57015f42124" "total_caloriesAV")
  (check-xdata-summary-values
   db "27dfb7e5900f4c2d80abc57015f42124" "total_calories")
  (check-xdata-summary-values
   db "27dfb7e5900f4c2d80abc57015f42124" "BatteryUsed")
  (check-xdata-summary-values
   db "27dfb7e5900f4c2d80abc57015f42124" "avg_cadence")
  (check-xdata-summary-values
   db "27dfb7e5900f4c2d80abc57015f42124" "Time")
  )

(define fit-files-test-suite
  (test-suite
   "FIT file reading"
   (test-case "f0001.fit"
     (do-basic-checks "./test-fit/f0001.fit" 22 14035))
   (test-case "f0002.fit"
     (do-basic-checks "./test-fit/f0002.fit" 21 500))
   (test-case "f0003.fit"
     (do-basic-checks "./test-fit/f0003.fit" 15 47))
   (test-case "f0004.fit"
     (do-basic-checks "./test-fit/f0004.fit" 22 138294))
   (test-case "f0005.fit"
     (do-basic-checks "./test-fit/f0005.fit" 13 227))
   (test-case "f0006.fit"
     (do-basic-checks "./test-fit/f0006.fit" 13 1297))
   (test-case "f0007.fit"
     (do-basic-checks "./test-fit/f0007.fit" 13 1452))
   (test-case "f0008.fit"
     (do-basic-checks "./test-fit/f0008.fit" 13 2331))
   (test-case "f0009.fit"
     (do-basic-checks "./test-fit/f0009.fit" 9 57))
   (test-case "f0010.fit (a)"
     (do-basic-checks "./test-fit/f0010.fit" 23 8078
                      #:extra-db-checks
                      (lambda (db)
                        (define n (query-value db "select count(*) from SESSION_WEATHER"))
                        (check = n 10 "Expecting Some Weather Records"))))
   (test-case "f0010.fit (b)"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0010.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     (define data (read-activity-from-file the-fit-file))
     ;; There should be 4 weather records in the file
     (define session (car (dict-ref data 'sessions '())))
     ;; 10 weather records attached to the session
     (check = (length (dict-ref session 'weather-conditions '())) 10))
   (test-case "f0011.fit"
     (do-basic-checks "./test-fit/f0011.fit" 12 39
                      #:extra-df-checks
                      (lambda (df)
                        (define stop-points (df-get-property df 'stop-points '()))
                        (check = (length stop-points) 4)
                        ;; Since this is a small data set, stop points are
                        ;; added to the data, instead of clearing existing
                        ;; points, we could also check that stop points are
                        ;; correctly added...
                        (define data (extract-data df axis-elapsed-time axis-speed 0 #f))
                        (check = (vector-length data) (+ (df-row-count df)
                                                         (* 2 (length stop-points))))
                        ;; The timer time axis does not mark stop points.
                        (define data2 (extract-data df axis-timer-time axis-speed 0 #f))
                        (check = (vector-length data2) (df-row-count df)))))
   (test-case "f0012.fit"
     (do-basic-checks "./test-fit/f0012.fit" 5 54))
   (test-case "f0013.fit"
     (do-basic-checks "./test-fit/f0013.fit" 22 8253))
   (test-case "f0014.fit"
     (do-basic-checks
      "./test-fit/f0014.fit" 24 155
      #:extra-db-checks
      (lambda (db)
        (check-xdata-app-count db 1)
        (check-xdata-app-present db "f848e2ecad564dbd8e36eaf0316d5ea3")
        (check-xdata-field-count db "f848e2ecad564dbd8e36eaf0316d5ea3" 1)
        (check-xdata-field-present db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal")
        )))
   (test-case "f0015.fit"
     (do-basic-checks
      "./test-fit/f0015.fit" 26 4057
      #:extra-db-checks
      (lambda (db)
        (check-xdata-app-count db 2)
        (check-xdata-app-present db "f848e2ecad564dbd8e36eaf0316d5ea3")
        (check-xdata-app-present db "a7e5e2534392495ba0728883c92d7211")
        (check-xdata-field-count db "f848e2ecad564dbd8e36eaf0316d5ea3" 2)
        (check-xdata-field-count db "a7e5e2534392495ba0728883c92d7211" 2)
        (check-xdata-field-present db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal")
        (check-xdata-field-present db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal_min")
        (check-xdata-field-present db "a7e5e2534392495ba0728883c92d7211" "THb Sensor 188.000000 on L. Quad")
        (check-xdata-field-present db "a7e5e2534392495ba0728883c92d7211" "SmO2 Sensor 188.000000 on L. Quad")

        (check-xdata-trackpoint-values
         db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal")
        (check-xdata-trackpoint-values
         db "a7e5e2534392495ba0728883c92d7211" "THb Sensor 188.000000 on L. Quad")
        (check-xdata-trackpoint-values
         db "a7e5e2534392495ba0728883c92d7211" "SmO2 Sensor 188.000000 on L. Quad")
        (check-xdata-summary-values
         db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal_min")

        )))
   (test-case "f0016.fit"
     (do-basic-checks
      "./test-fit/f0016.fit" 29 2119
      #:extra-db-checks
      (lambda (db)
        (check-xdata-app-count db 1)
        (check-xdata-app-present db "00000000000000000000000000000000")
        (check-xdata-field-count db "00000000000000000000000000000000" 4)
        (check-xdata-field-present db "00000000000000000000000000000000" "Distance")
        (check-xdata-field-present db "00000000000000000000000000000000" "Speed")
        (check-xdata-field-present db "00000000000000000000000000000000" "Leg Spring Stiffness")
        (check-xdata-field-present db "00000000000000000000000000000000" "Form Power")

        (check-xdata-trackpoint-values
         db "00000000000000000000000000000000" "Distance")
        (check-xdata-trackpoint-values
         db "00000000000000000000000000000000" "Speed")
        (check-xdata-trackpoint-values
         db "00000000000000000000000000000000" "Leg Spring Stiffness")
        (check-xdata-trackpoint-values
         db "00000000000000000000000000000000" "Form Power")
        )
      #:extra-df-checks check-run-power))
   (test-case "f0017.fit"
     (do-basic-checks
      "./test-fit/f0017.fit" 20 3211
      #:extra-db-checks check-outdoorsports-xdata))
   (test-case "f0018.fit"
     (do-basic-checks
      ;; This file has a single 0 value inside the series cpsmth, for combined
      ;; pedal smoothness.  It also has l/r smoothness values...
      "./test-fit/f0018.fit" '(15 20 42 22 34) '(582 30 10217 10 8613)
      #:extra-db-checks check-stryd-xdata
      #:expected-session-count 5))
   (test-case "f0019.fit"
     (do-basic-checks
      "./test-fit/f0019.fit" 26 4081
      #:extra-db-checks check-stryd-xdata))
   (test-case "f0022.fit"
     (do-basic-checks "./test-fit/f0022.fit" 13 1868))
   (test-case "f0023.fit"
     (do-basic-checks
      "./test-fit/f0023.fit" 28 2138
      #:extra-db-checks check-garmin-run-power-data))
   (test-case "f0025.fit"
     (do-basic-checks
      "./test-fit/f0025.fit" 29 2148
      #:extra-db-checks
      (lambda (db)
        (check-stryd-xdata db)
        ;; Stryd Pace Application
        (define app-id "8e471b4dddf4481a9861d80f22113f9a")
        ;; (check-xdata-app-count db 1)
        (check-xdata-app-present db app-id)
        (check-xdata-field-count db app-id 8)
        (for ([field '("Instantaneous Speed" "Instantaneous Pace")])
          (check-xdata-field-present db app-id field))
        (for ([field '("Instantaneous Speed" "Instantaneous Pace")])
          (check-xdata-trackpoint-values db app-id field)))))
   (test-case "f0026.fit"
     (do-basic-checks
      "./test-fit/f0026.fit" 24 6098))
   (test-case "f0027.fit"
     (do-basic-checks
      "./test-fit/f0027.fit" 34 4948
      #:extra-df-checks
      (lambda (df)
        ;; These series were missing from the activities as they are provided
        ;; by "enhanced fields".  Check that they are present.
        (check-true (df-contains? df "spd"))
        (check-true (df-contains? df "alt")))))
   (test-case "f0028.fit"
     (do-basic-checks
      "./test-fit/f0028.fit" 27 941
      #:extra-df-checks
      (lambda (df)
        ;; These series were missing from the activities as they are provided
        ;; by "enhanced fields".  Check that they are present.
        (check-true (df-contains? df "spd"))
        (check-true (df-contains? df "alt")))))
   (test-case "f0029.fit"
     (do-basic-checks
      "./test-fit/f0029.fit" '(18 20 35 20 34) '(942 814 24062 330 19657)
      #:expected-session-count 5
      #:extra-db-checks
      (lambda (db)
        (check-xdata-app-count db 2)
        (check-xdata-app-present db "660a581e5301460c8f2f034c8b6dc90f")
        (check-xdata-app-present db "6dcfffe5cd3d41f38ba313fa0647b003")
        (check-xdata-field-count db "660a581e5301460c8f2f034c8b6dc90f" 7)
        (check-xdata-field-count db "6dcfffe5cd3d41f38ba313fa0647b003" 1)
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Leg Spring Stiffness")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Form Power")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Elevation")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Vertical Oscillation")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Ground Time")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Cadence")
        (check-xdata-field-present db "660a581e5301460c8f2f034c8b6dc90f" "Power")
        (check-xdata-field-present db "6dcfffe5cd3d41f38ba313fa0647b003" "wprime_bal"))))
   (test-case "f0031.fit"
     (do-basic-checks
      "./test-fit/f0031.fit" 8 1798
      #:extra-db-checks
      (lambda (db)
        ;; Importing this activity required building a lap and summary
        ;; information.  Check that it is present.
        (define s (db-fetch-session 1 db))
        (define laps (session-laps s))
        (check = 1 (length laps))
        (define lap0 (car laps))
        (check-true
         (let ((x (lap-avg-cadence lap0)))
           (and (number? x) (> x 0))))
        (check-true
         (let ((x (lap-max-cadence lap0)))
           (and (number? x) (> x 0))))
        (check-true
         (let ((x (lap-avg-power lap0)))
           (and (number? x) (> x 0))))
        (check-true
         (let ((x (lap-max-power lap0)))
           (and (number? x) (> x 0))))
        (check-true
         (let ((x (lap-avg-hr lap0)))
           (and (number? x) (> x 0))))
        (check-true
         (let ((x (lap-max-hr lap0)))
           (and (number? x) (> x 0)))))))
   (test-case "f0032.fit"
     (do-basic-checks
      "./test-fit/f0032.fit" 20 1473))
   (test-case "f0040.fit"
     (do-basic-checks
      "./test-fit/f0040.fit" 16 11272))
   (test-case "f0042.fit"
     (do-basic-checks
      "./test-fit/f0042.fit" 17 2110))
   (test-case "f0043.fit"
     (do-basic-checks
      "./test-fit/f0043.fit" 30 2669))
   (test-case "f0047.fit (a)"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0047.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     (define data (read-activity-from-file the-fit-file))

     (let session-loop ([sessions (dict-ref data 'sessions #f)])
       (unless (null? sessions)
         (define-values (session last-session?)
           (values (car sessions) (null? (cdr sessions))))
         (let lap-loop ([laps (session-laps session)])
           (unless (null? laps)
             (define-values (lap last-lap?)
               (values (car laps) (and last-session? (null? (cdr laps)))))
             (let length-loop ([lengths (lap-lengths lap)])
               (unless (null? lengths)
                 (define-values (len last-len?)
                   (values (car lengths) (and last-lap? (null? (cdr lengths)))))
                 (define-values (start end) (get-start-end-times len))
                 ;; We should do better here, for now we just check we have
                 ;; some records...
                 (define track (length-track len))
                 (check-true (> (length track) 0))

                 (let trackpoint-loop ([trackpoints track])
                   (unless (null? trackpoints)
                     (define-values (trackpoint last-trackpoint?)
                       (values (car trackpoints) (and last-len? (null? (cdr trackpoints)))))
                     (define ts (get-start-time trackpoint))
                     ;; Last trackpoint can be recorded after the session ended...
                     (if last-trackpoint?
                         (check-true (>= ts start)
                                     (format "timestamp ~a before length start ~a"
                                             ts start))
                         (check-true (and (>= ts start) (<= ts (ceiling end)))
                                     (format "timestamp ~a outside time range [~a .. ~a]"
                                             ts start end)))
                     (trackpoint-loop (cdr trackpoints))))
                 (length-loop (cdr lengths))))
             (lap-loop (cdr laps))))
         (session-loop (cdr sessions)))))

   (test-case "f0047.fit (b)"
     (do-basic-checks "./test-fit/f0047.fit" 15 63
                      #:extra-db-checks
                      (lambda (db)
                        ;; Expected HR data for all length records to be
                        ;; filled in, see #80
                        (define c
                          (query-value db "select count(*)
                                             from A_LENGTH L, SECTION_SUMMARY SS
                                            where L.summary_id = SS.id
                                              and (SS.max_heart_rate is null
                                                   or SS.avg_heart_rate is null)"))
                        (check = c 0))))
   (test-case "f0048.fit"
     (do-basic-checks
      "./test-fit/f0048.fit" 27 2191
      #:extra-df-checks (lambda (df)
                          (check-true (df-contains? df "tempe"))
                          (check-true (df-contains? df "temperature")))))
   (test-case "f0049.fit"
     (define the-fit-file "./test-fit/f0049.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define expected-track-lengths
       '(24 21 40 23 23 29 15 14 23 23 14 18 17 28 23 56 27 63 32 215 31 295
            28 23 146 9 96 26 23 27 53 24 64 22 78 18 34 12 29 20 23 8 62 21
            33 22 40 16 36 43 24 18 43 32 46 31 22 22 53 24 21 16 25 19 361
            22 19 9 42 12 47 23 39 19 19 9 32 21 24 23 38 30 25 28 30 24 30
            15 19 41 39))

     (define data (read-activity-from-file the-fit-file))
     (for ([session (in-list (dict-ref data 'sessions #f))])

       (for ([lap (in-list (session-laps session))])
         (for ([len (in-list (lap-lengths lap))])
           (check-false (null? expected-track-lengths) "expected-track-lengths too short")
           (define clen (car expected-track-lengths))
           (set! expected-track-lengths (cdr expected-track-lengths))
           ;; We should do better here, for now we just check we have some
           ;; records...
           (define-values (start end) (get-start-end-times len))
           (define cend (ceiling end))
           (define track (length-track len))
           (check-equal? clen (length track) "track length mismatch")
           (for ([trackpoint (in-list track)])
             (define ts (get-start-time trackpoint))
             (check-true (and (>= ts start) (<= ts cend))
                         (format "timestamp ~a outside time range [~a .. ~a]"
                                 ts start cend))))))
     (check-true (null? expected-track-lengths) "expected-track-lengths too long"))
   (test-case "f0051.fit"
     (do-basic-checks
      "./test-fit/f0051.fit" '(20 18 16 18 20) '(84 41 21 69 86)
      #:expected-session-count 5))
   (test-case "f0052.fit"
     (do-basic-checks
      "./test-fit/f0052.fit" 26 3866
      #:extra-df-checks
      (lambda (df)
        (check-true (df-contains? df "cpsmth")))))
   (test-case "f0053.fit"
     (do-basic-checks
      "./test-fit/f0053.fit" 14 1396
      #:extra-df-checks
      (lambda (df)
        (define limit (df-row-count df))
        ;; All lap indexes must be valid
        (for ([lap-start (in-vector (df-get-property df 'laps))])
          (define index (df-index-of df "timestamp" lap-start))
          (check-true (and (number? index)
                           (>= index 0)
                           (< index limit)))))))
   (test-case "f0055.fit"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0055.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     ;; Normally, just reading the file threw an exception
     (check-not-exn
      (lambda ()
        (define data (read-activity-from-file the-fit-file))
        (define session (car (dict-ref data 'sessions '())))
        (check-false (null? session)))))
   (test-case "f0056.fit"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0056.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     ;; Normally, just reading the file threw an exception
     (check-not-exn
      (lambda ()
        (define data (read-activity-from-file the-fit-file))
        (define session (car (dict-ref data 'sessions '())))
        (check-false (null? session)))))
   (test-case "f0057.fit"
     (do-basic-checks
      "./test-fit/f0057.fit" 29 4100
      #:extra-df-checks
      (lambda (df)
        (check-equal? (vector-length (df-get-property df 'laps)) 9))))
   (test-case "f0058.fit"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0058.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     ;; Normally, just reading the file threw an exception
     (check-not-exn
      (lambda ()
        (define data (read-activity-from-file the-fit-file))
        (define sessions (dict-ref data 'sessions '()))
        (check-false (null? sessions))
        (for ([session (in-list sessions)])
          (define devices (dict-ref session 'devices '()))
          (check-false (null? devices))))))
   (test-case "f0059.fit"
     ;; This test is different than the others as this checks that the FIT
     ;; file reader itself behaves correctly.
     (define the-fit-file "./test-fit/f0059.fit")
     (unless (file-exists? the-fit-file)
       (skip-test))
     (define data (read-activity-from-file the-fit-file))
     (define sessions (dict-ref data 'sessions '()))
     (check-equal? 1 (length sessions))
     (define laps (session-laps (car sessions)))
     (check-equal? 6 (length laps))
     (for ([lap (in-list laps)]
           [llen '(6 1 34 1 9 1)])
       (define lengths (lap-lengths lap))
       (check-equal? (length lengths) llen)
       (for ([len (in-list lengths)])
         (check-true (> (length (length-track len)) 0)))))
   (test-case "f0060.fit"
     (do-basic-checks
      "./test-fit/f0060.fit" 30 10413
      #:extra-df-checks
      (lambda (df)
        (check-true (df-contains? df "fg" "fgi" "rg" "rgi" "gr")))))
   (test-case "f0061.fit"
     (do-basic-checks
      "./test-fit/f0061.fit" 39 15205
      #:extra-db-checks
      (lambda (db)
        ;; This file is from a Wahoo ACE -- they use developer data fields,
        ;; but don't specify a developer id or application id, so it is a bit
        ;; unclear how they use them, but this seems to work, at least for
        ;; now.  As of Dec 2024, Wahoo does not support developer apps writing
        ;; data fields in their on FIT files, so they are in full control over
        ;; these...
        (check-xdata-app-count db 13)
        (check-xdata-field-count db "manufacturer-32-19" 1)
        (check-xdata-field-count db "manufacturer-32-18" 1)
        (check-xdata-field-count db "manufacturer-32-17" 1)
        (check-xdata-field-count db "manufacturer-32-14" 1)
        (check-xdata-field-count db "manufacturer-32-13" 1)
        (check-xdata-field-count db "manufacturer-32-12" 1)
        (check-xdata-field-count db "manufacturer-32-11" 1)
        (check-xdata-field-count db "manufacturer-32-10" 1)
        (check-xdata-field-count db "manufacturer-32-6" 1)
        (check-xdata-field-count db "manufacturer-32-5" 1)
        (check-xdata-field-count db "manufacturer-32-2" 1)
        (check-xdata-field-count db "manufacturer-32-1" 1))))
   (test-case "f0062.fit"
     (do-basic-checks
      "./test-fit/f0062.fit" 29 2126
      #:extra-db-checks
      (lambda (db)
        ;; Check that values were correctly inserted into the database
        (define row (query-row db "select rpe_scale, feel_scale from A_SESSION"))
        (check-equal? 4 (vector-ref row 0) "DB RPE mismatch")
        (check-equal? 5.0 (vector-ref row 1) "DB FEEL mismatch")
        ;; Fetching the session correctly retrieves the values...
        (define s (db-fetch-session 1 db))
        (check-equal? 4 (session-rpe s) "Session RPE mismatch")
        (check-equal? 5.0 (session-feel s) "Session FEEL mismatch"))))
   (test-case "f0063.fit"
     (do-basic-checks
      "./test-fit/f0063.fit" 16 2332
      #:extra-df-checks
      (lambda (df)
        (define last-row (sub1 (df-row-count df)))
        (define timer (df-ref df last-row "timer"))
        (define elapsed (df-ref df last-row "elapsed"))
        (check-=
         timer elapsed 109              ; manually calculated, somewhat silly
         (format "Timer (~a) and Elapsed (~a) series should be mostly identical" timer elapsed)))))
   (test-case "multi-checks"
     (do-multi-checks
      ;; These two files contain data from the same XDATA app, the application
      ;; should only be recorded once...
      '("./test-fit/f0014.fit" "./test-fit/f0015.fit")
      #:extra-db-checks
      (lambda (db)
        (check-xdata-app-count db 2)
        (check-xdata-app-present db "f848e2ecad564dbd8e36eaf0316d5ea3")
        (check-xdata-app-present db "a7e5e2534392495ba0728883c92d7211")
        (check-xdata-field-count db "f848e2ecad564dbd8e36eaf0316d5ea3" 2)
        (check-xdata-field-count db "a7e5e2534392495ba0728883c92d7211" 2)
        (check-xdata-field-present db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal")
        (check-xdata-field-present db "f848e2ecad564dbd8e36eaf0316d5ea3" "current_wbal_min")
        (check-xdata-field-present db "a7e5e2534392495ba0728883c92d7211" "THb Sensor 188.000000 on L. Quad")
        (check-xdata-field-present db "a7e5e2534392495ba0728883c92d7211" "SmO2 Sensor 188.000000 on L. Quad"))))
     ))

(module+ test
  (set-fix-elevation-on-import #t)       ; enable elevation correction -- we want to test it.

  ;; when set to #t, dbglog output is sent to stdout, useful for debugging
  ;; purposes.
  (set-dbglog-to-standard-output #f)

  (run-tests #:package "fit-test"
             #:results-file "test-results/fit-test.xml"
             ;; #:only '(("FIT file reading" "f0011.fit"))
             fit-files-test-suite))
