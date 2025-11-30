#lang racket/base
;; export-fit-settings.rkt - export device settings (hr and power zones) as
;; FIT files that can be uploaded to a device.
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2020, 2025 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         pict
         racket/class
         racket/gui/base
         racket/list
         racket/match
         racket/string
         "../fit-file/fit-file.rkt"
         "../models/sport-zone.rkt"
         "../sport-charms.rkt"
         "../utilities.rkt"
         "../widgets/main.rkt")

(provide export-settings-dialog%)

(define activity-class-names
  (list
   "0 - No Exercise"
   "1 - Ocasional exercise, < 15 min / week"
   "2 - Ocasional exercise, 15 - 30 min / week"
   "3 - Ocasional exercise, 30 min / week"
   "4 - Regular exercise, approx 45 min / week"
   "5 - Regular exercise, 45 min - 1 hour / week"
   "6 - Regular exercise, 1 - 3 hours / week"
   "7 - Regular exercise, 3 - 7 hours / week"
   "8 - Almost daily training, 7 - 11 hours / week"
   "9 - Daily training, 11 - 15 hours / week"
   "10 - Daily training, > 15 hours / week"))

;; Return the athlete boddyweight as an average of the last 2 weeks of
;; measurements, or if there are no measurements in the past two weeks, the
;; last measurement.  Returns #f if there are no bodyweight measurements
;; recorded in the database.
(define (get-bodyweight database)
  (let ([v (query-maybe-value
            database
            "
select avg(body_weight)
  from ATHLETE_METRICS
 where timestamp > strftime('%s', datetime('now', '-14 days'))")])
    (if (sql-null? v)
        (let ([v (query-maybe-value
                 database
         "
select body_weight
  from ATHLETE_METRICS
 where timestamp = (select max(timestamp) from ATHLETE_METRICS)")])
          (if (sql-null? v) #f v))
        v)))

;; Return the activty class of the athlete (see `activity-class-names`) based
;; on the average hours per week for the past 6 weeks.  Returns 0 if there are
;; no activities recorded in the past 6 weeks.
(define (get-activity-class database)
  (let ((hpw (query-maybe-value
            database
            "
  select ((sum(SS.total_timer_time) / 3600.0) / 6) as h
    from A_SESSION S, SECTION_SUMMARY SS
   where S.start_time > strftime('%s', datetime('now', '-42 days'))
     and S.summary_id = SS.id")))
    (if (and hpw (not (sql-null? hpw)))
        (cond
          ((< hpw 0.25) 1)
          ((< hpw 0.5) 2)
          ((= hpw 0.5) 3)             ; will probably never be hit
          ((< hpw 0.75) 4)
          ((< hpw 1) 5)
          ((< hpw 3) 6)
          ((< hpw 7) 7)
          ((< hpw 11) 8)
          ((< hpw 15) 9)
          (#t 10))
        0)))

;; Create a canvas which will display zones for SPORT/SUB-SPORT and
;; ZONE-METRIC.  zones will be reloaded each time the canvas is refreshed.
(define (make-zone-display-canvas parent sport sub-sport zone-metric)

  (define message-font
    (send the-font-list find-or-create-font 14 'default 'normal 'normal))

  (define (draw-centered-message dc msg font)
    (let-values (([cw ch] (send dc get-size))
                 ([w h x y] (send dc get-text-extent msg font #t)))
      (send dc set-font font)
      (send dc set-text-foreground "gray")
      (let ((ox (- (/ cw 2) (/ w 2)))
            (oy (- (/ ch 2) (/ h 2))))
        (send dc draw-text msg ox oy))))

  (define (on-paint canvas dc)
    (send dc clear)
    (let ((zones (sport-zones-for-sport sport sub-sport zone-metric)))
      (if zones
          (let-values ([(w h) (send canvas get-size)])
            (let ((pict (pp-sport-zones/compact-pict zones #:width w #:height h)))
              (draw-pict pict dc 0 0)))
          (begin
            (send dc set-text-foreground "gray")
            (draw-centered-message dc "No Zones Defined" message-font)))))

  (new canvas%
       [parent parent]
       [min-height 60]
       [stretchable-height #f]
       [stretchable-width #t]
       [paint-callback on-paint]))

;; Return a byte string containing the FIT data with HR zones for running.
(define (get-running-fit-settings)
  (define sport 1)                      ; running
  (define hr-zones (sport-zones-for-sport 1 #f 'heart-rate))
  (define hr-zone-data
    (and hr-zones (for/list ([zone (in-vector (sz-boundaries hr-zones))]) zone)))
  (let ((builder (new fit-sport-file%
                      [sport sport]
                      [max-hr (and hr-zone-data (last hr-zone-data))]
                      ;; NOTE: we use zone 0, but Garmin does not.
                      [hr-zones (and hr-zone-data (cdr hr-zone-data))])))
    (send builder get-fit-data)))

;; Return a byte string containing the FIT data with HR and Power zones for
;; cycling.  hr-zones? and power-zones? control what zones are stored.  In
;; addition, the athlete's FTP is also stored.
(define (get-cycling-fit-settings hr-zones? power-zones? ftp)
  (define sport 2)                    ; bike
  (define hr-zones (sport-zones-for-sport 2 #f 'heart-rate))
  (define power-zones (sport-zones-for-sport 2 #f 'power))
  (define hr-zone-data
    (and hr-zones (for/list ([zone (in-vector (sz-boundaries hr-zones))]) zone)))
  (define power-zone-data
    (and power-zones (for/list ([zone (in-vector (sz-boundaries power-zones))]) zone)))
  (let ((builder (new fit-sport-file%
                      [sport sport]
                      [max-hr (and hr-zone-data (last hr-zone-data))]
                      [ftp ftp]
                      [power-zones (and power-zone-data (cdr power-zone-data))]
                      [hr-zones (and hr-zone-data (cdr hr-zone-data))])))
    (send builder get-fit-data)))

;; Return a byte string containing the FIT data with athlete information: date
;; of birth, gender, body weight, activity class.  If collect-hrv is #t, the
;; flag is set in the FIT file and the device will store HRV data for
;; activities.
(define (get-athlete-fit-settings dob gender bw height ac collect-hrv)
  (let ((builder (new fit-settings-file%
                      [date-of-birth dob]
                      [gender gender]
                      [weight bw]
                      [height height]
                      [activity-class ac]
                      [collect-hrv-data? collect-hrv])))
    (send builder get-fit-data)))

;; Write data from BSTR, a byte string to FILE-NAME
(define (wr file-name bstr)
  (call-with-output-file file-name
    (lambda (out) (write-bytes bstr out))
    #:mode 'binary
    #:exists 'replace))

(define export-settings-dialog%
  (class edit-dialog-base%
    (init-field sport-charms)
    (super-new [title "Export FIT Settings"]
               [icon (edit-icon)]
               [save-button-name "Export"]
               [min-width 600]
               [min-height 300])

    (define tag 'activity-log:export-fit-settings)

    ;; database connection, set to a valid connection while the dialog is
    ;; shown
    (define db #f)

    (define dob-field #f)
    (define gender-field #f)
    (define height-field #f)

    (define bw-field #f)
    (define bw-auto-set-chkbox #f)

    (define ac-field #f)
    (define ac-auto-set-chkbox #f)

    (define hrv-chkbox #f)

    (define run-hrz-chkbox #f)
    (define run-hrz-canvas #f)

    (define bike-hrz-chkbox #f)
    (define bike-hrz-canvas #f)
    (define bike-pwrz-chkbox #f)
    (define bike-pwrz-canvas #f)
    (define bike-ftp-field #f)

    (define export-dir-field #f)

    (define (on-select-export-directory sender event)
      (let* ([current (send export-dir-field get-value)]
             [updated (get-directory
                       "Select directory for export..."
                       (send this get-top-level-window)
                       (if (equal? (string-trim current) "") #f current)
                       '(common))])
        (when updated
          (send export-dir-field set-value (path->string updated))
          ;; externally set values are not validated
          (send export-dir-field validate))))

    (define (on-autoset-bodyweight sender event)
      (let ([autoset? (send sender get-value)])
        (send bw-field enable (not autoset?))
        (when autoset?
          (let ((bw (get-bodyweight db)))
            (when bw
              (send bw-field set-numeric-value bw))))))

    (define (on-autoset-activity-class sender event)
      (let ([autoset? (send sender get-value)])
        (send ac-field enable (not autoset?))
        (when autoset?
          (let ((ac (get-activity-class db)))
            (when ac
              (send ac-field set-selection ac))))))

    (define (save-preferences)
      (let ((autoset-bw (send bw-auto-set-chkbox get-value))
            (autoset-ac (send ac-auto-set-chkbox get-value))
            (collect-hrv (send hrv-chkbox get-value))
            (export-run-hrz (send run-hrz-chkbox get-value))
            (export-bike-hrz (send bike-hrz-chkbox get-value))
            (export-bike-pwrz (send bike-pwrz-chkbox get-value))
            (export-directory (send export-dir-field get-value)))
        (put-pref tag
                     (list 'gen1 autoset-bw autoset-ac collect-hrv export-run-hrz
                           export-bike-hrz export-bike-pwrz export-directory)))
      (call-with-transaction
       db
       (lambda ()
         (let ((ftp (send bike-ftp-field get-converted-value)))
           (when (and ftp (not (eq? ftp 'empty)))
             (send sport-charms put-athlete-ftp ftp db)))

         (let ((dob (send dob-field get-converted-value)))
           (when (and dob (not (eq? dob 'empty)))
             (send sport-charms put-athlete-dob dob db)))

         (let ((gender (send gender-field get-selection)))
           (send sport-charms put-athlete-gender gender db))

         (let ((height (send height-field get-converted-value)))
           (when (and height (not (eq? height 'empty)))
             (send sport-charms put-athlete-height height db))))))

    (define (restore-preferences)
      (let ((prefs (get-pref tag (lambda () #f))))
        (when prefs
          (match-define (list ptag autoset-bw autoset-ac collect-hrv export-run-hrz
                              export-bike-hrz export-bike-pwrz export-directory) prefs)
          (when (eq? ptag 'gen1)
            (send bw-auto-set-chkbox set-value autoset-bw)
            (on-autoset-bodyweight bw-auto-set-chkbox #f)
            (send ac-auto-set-chkbox set-value autoset-ac)
            (on-autoset-activity-class ac-auto-set-chkbox #f)
            (send hrv-chkbox set-value collect-hrv)
            (send run-hrz-chkbox set-value export-run-hrz)
            (send bike-hrz-chkbox set-value export-bike-hrz)
            (send bike-pwrz-chkbox set-value export-bike-pwrz)
            (send export-dir-field set-value export-directory))))

      ;; DOB, gender, height and FTP are stored in the database, fetch them
      ;; from there.

      (let ((dob (send sport-charms get-athlete-dob db)))
        (when dob
          (send dob-field set-date-value dob)))

      (let ((gender (send sport-charms get-athlete-gender db)))
        (when gender
          (send gender-field set-selection gender)))

      (let ((height (send sport-charms get-athlete-height db)))
        (when height
          (send height-field set-numeric-value height)))

      (let ((ftp (send sport-charms get-athlete-ftp db)))
        (when ftp
          (send bike-ftp-field set-numeric-value ftp)))

      ;; Disable zone export check-boxes if there are no corresponding zones
      ;; defined.

      (let ((z (sport-zones-for-sport 1 #f 'heart-rate)))
        (unless z
          (send run-hrz-chkbox set-value #f))
        (send run-hrz-chkbox enable (not (eq? z #f))))

      (let ((z (sport-zones-for-sport 2 #f 'heart-rate)))
        (unless z
          (send bike-hrz-chkbox set-value #f))
        (send bike-hrz-chkbox enable (not (eq? z #f))))

      (let ((z (sport-zones-for-sport 2 #f 'power)))
        (unless z
          (send bike-pwrz-chkbox set-value #f))
        (send bike-pwrz-chkbox enable (not (eq? z #f)))))

    (let ([p (send this get-client-pane)])
      (let ([p1 (make-group-box-panel p "Athlete Info")])

        (let ([p (make-horizontal-pane p1)])
          (set! dob-field
                (new date-input-field% [parent p] [label "Birth date: "]
                     [stretchable-width #f] [allow-empty? #f]))
          (set! gender-field
                (new choice% [parent p] [label "Gender: "] [choices '("Female" "Male")]))
          (set! height-field
                (new number-input-field% [parent p] [label "Height: "]
                     [cue-text "meters"] [stretchable-width #f]
                     [allow-empty? #f])))

        (let ([p (make-horizontal-pane p1)])
          (set! bw-field
                (new number-input-field% [parent p] [label "Body weight: "]
                     [cue-text "kg"] [stretchable-width #f]
                     [allow-empty? #f]))
          (set! bw-auto-set-chkbox
                (new check-box% [parent p] [label "Autoset from recent data"]
                     [callback on-autoset-bodyweight])))

        (let ([p (make-horizontal-pane p1)])
          (set! ac-field (new choice% [parent p] [label "Activity class: "]
                              [choices activity-class-names]))
          (set! ac-auto-set-chkbox
                (new check-box% [parent p] [label "Autoset from recent activitites"]
                     [callback on-autoset-activity-class])))

        (set! hrv-chkbox
              (new check-box% [parent p1] [label "Collect HRV Data"])))

      (let ([p2 (make-group-box-panel p "Running")])

        (let ((p (make-horizontal-pane p2)))
          (set! run-hrz-chkbox
                (new check-box% [parent p] [label "Export HR Zones"]))
          (new message% [parent p] [label ""] [stretchable-width #t]))

        (set! run-hrz-canvas (make-zone-display-canvas p2 1 #f 'heart-rate)))

      (let ([p3 (make-group-box-panel p "Cycling")])
        (let ((p (make-horizontal-pane p3)))
          (set! bike-hrz-chkbox
                (new check-box% [parent p] [label "Export HR Zones"]))
          (new message% [parent p] [label ""] [stretchable-width #t]))
        (set! bike-hrz-canvas (make-zone-display-canvas p3 2 #f 'heart-rate))
        (let ([p (make-horizontal-pane p3)])
          (set! bike-pwrz-chkbox
                (new check-box% [parent p] [label "Export Power Zones"]))
          (set! bike-ftp-field
                (new number-input-field% [parent p] [label "FTP: "]
                     [min-value 0] [max-value 1000]
                     [min-width 100] [stretchable-width #f]))
          (new message% [parent p] [label ""] [stretchable-width #t]))
        (set! bike-pwrz-canvas (make-zone-display-canvas p3 2 #f 'power)))

      (let ([p (make-horizontal-pane p)])
        (set! export-dir-field
              (new validating-input-field% [parent p] [label "Export directory: "]
                   [validate-fn (lambda (x)
                                  (and (not (equal? (string-trim x) ""))
                                       (directory-exists? x)))]
                   [convert-fn (lambda (x) x)]))
        (new button% [parent p] [label "Browse..."]
             [callback on-select-export-directory]))
      )

    (define/override (has-valid-data?)
      (and (send dob-field has-valid-value?)
           (send height-field has-valid-value?)
           (send bike-ftp-field has-valid-value?)
           (send bw-field has-valid-value?)
           (send export-dir-field has-valid-value?)))

    (define (export-fit-settings)
      (let ((dob (send dob-field get-converted-value))
            (gender (send gender-field get-selection))
            (height (send height-field get-converted-value))
            (bw (send bw-field get-converted-value))
            (ac (send ac-field get-selection))
            (collect-hrv (send hrv-chkbox get-value))
            (export-run-hrz (send run-hrz-chkbox get-value))
            (export-bike-hrz (send bike-hrz-chkbox get-value))
            (export-bike-pwrz (send bike-pwrz-chkbox get-value))
            (export-directory (send export-dir-field get-value)))
        (when export-run-hrz
          (wr (build-path export-directory "run-settings.fit")
              (get-running-fit-settings)))
        (when (or export-bike-hrz export-bike-pwrz)
          (wr (build-path export-directory "bike-settings.fit")
              (get-cycling-fit-settings export-bike-hrz export-bike-pwrz (send sport-charms get-athlete-ftp))))
        (wr (build-path export-directory "athlete-settings.fit")
            (get-athlete-fit-settings dob gender bw height ac collect-hrv))))

    (define/public (show-dialog parent database)
      (set! db database)
      (restore-preferences)           ; need a database to restore preferences
      (let ((result (send this do-edit parent)))
        (when result
          (export-fit-settings)
          (save-preferences))
        (set! db #f)                    ; clear db field
        result))

    ))

