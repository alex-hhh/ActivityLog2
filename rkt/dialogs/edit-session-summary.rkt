#lang racket/base
;; edit-session-summary.rkt -- edit summary information about a session
;;
;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2015, 2018, 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/class
         racket/gui/base
         racket/match
         tzinfo
         "../al-widgets.rkt"
         "../dbutil.rkt"
         "../session-df/session-df.rkt"
         "../sport-charms.rkt"
         "../widgets/main.rkt"
         "edit-session-tss.rkt")

(provide get-edit-session-summary-dialog)

(define edit-session-summary-dialog%
  (class edit-dialog-base%
    (init)
    (super-new [title "Session Summary"]
               [icon (get-sport-bitmap-colorized #f #f)]
               [min-width 600]
               [min-height 500])

    (define timestamp #f)

    (define title-field #f)
    (define sport-choice #f)
    (define rpe-scale-choice #f)
    (define start-date-field #f)
    (define start-time-field #f)
    (define time-zone-field #f)
    (define duration-field #f)
    (define distance-field #f)
    (define labels-input #f)
    (define equipment-input #f)
    (define description-field #f)

    ;; Return the UNIX timestamp as stored in the date, time, and timezone
    ;; fields in the GUI.
    (define (get-gui-timestamp)
      (let ([date (send start-date-field get-converted-value)]
            [time (send start-time-field get-converted-value)])
        (and (number? date) (number? time)
             (let ([start-time (+ date time)])
               (let-values ([(tzid tzname) (send time-zone-field get-selection)])
                 (match (local-seconds->tzoffset tzname start-time)
                   [(tzoffset adj dst? abbrev) (- start-time adj)]
                   ;; Hmm, our time falls in a DST window, just use the first
                   ;; adjustment...
                   [(tzgap start (tzoffset adj1 dst1? abbrev1) (tzoffset adj2 dst2? abbrev2))
                    (- start-time adj1)]
                   ;; Hmm, our time falls in an DST overlap, just use the first
                   ;; adjustment...
                   [(tzoverlap (tzoffset adj1 dst1? abbrev1) (tzoffset adj2 dst2? abbrev2))
                    (- start-time adj1)]))))))

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (set! title-field (new text-field% [parent p0] [label "Name:"])))

      (let ((p0 (make-horizontal-pane p #f)))
          (set! start-date-field
                (new date-input-field% [parent p0] [label "Date: "] [local-time? #f]
                     [valid-value-cb (lambda (v) (set! timestamp (get-gui-timestamp)))]))
          (set! start-time-field
                (new time-of-day-input-field% [parent p0] [label "Start Time: "]
                     [valid-value-cb (lambda (v) (set! timestamp (get-gui-timestamp)))]))
          (set! time-zone-field
                (new time-zone-selector% [parent p0]
                     [callback (lambda (id tz)
                                 ;; Update time to reflect new time zone
                                 (when timestamp
                                   (send start-date-field set-date-value timestamp tz)
                                   (send start-time-field set-time-of-day-value timestamp tz)))])))

      (let  ((p0 (make-horizontal-pane p #f)))
        (set! sport-choice
              (new sport-selector% [parent p0]
                   [sports-in-use-only? #f]
                   [callback
                    (lambda (v)
                      (let ((icon (get-sport-bitmap-colorized (car v) (cdr v))))
                        (send this set-icon icon)))])))

      (let ((p0 (make-horizontal-pane p #f)))
        (set! duration-field
              (new duration-input-field% [parent p0] [label "Time: "]))
        (set! distance-field
              (new number-input-field% [parent p0] [label "Distance: "]
                   [cue-text "km"]))
        (set! rpe-scale-choice
              (new choice% [parent p0]
                   [label "RPE: "]
                   [choices '("0 -- Not Specified"
                              "1 -- Rest"
                              "2 -- Really Easy"
                              "3 -- Easy"
                              "4 -- Moderate"
                              "5 -- Challenging"
                              "6 -- Hard"
                              "7 -- Hard"
                              "8 -- Really Hard"
                              "9 -- Really, Really Hard"
                              "10 -- Maximal")])))

      (let ((p0 (new vertical-pane% [parent p]
                     [border 0] [spacing 5] [stretchable-height #f])))
        (set! labels-input (new label-input-field% [parent p0]))
        (set! equipment-input (new equipment-input-field% [parent p0])))

      (let ((p0 (make-horizontal-pane p #t)))
        (set! description-field
              (new text-field% [parent p0]
                   [label "Description: "] [style '(multiple)]))))

    (define/override (has-valid-data?)
      (let ((date (send start-date-field get-converted-value))
            (time (send start-time-field get-converted-value))
            (duration (send duration-field get-converted-value))
            (distance (send distance-field get-converted-value)))
        ;; NOTE: get-converted-value will return #f if the value is invalid,
        ;; and will return 'empty if the field is empty
        (and
         ;; Date and time must be valid and non-empty
         (and date (not (eq? date 'empty)))
         (and time (not (eq? time 'empty)))

         ;; Duration and distance must both be valid
         (and duration distance)

         ;; Duration must not be empty (distance can be)
         (not (eq? duration 'empty)))))

    (define (setup-for-session db session-id)
      (let* ((r (query-row db "
select S.name as title,
       S.sport_id,
       S.sub_sport_id,
       S.start_time,
       SS.total_timer_time,
       SS.total_distance,
       S.description,
       ifnull(S.rpe_scale, 0),
       (select name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id) as time_zone
  from A_SESSION S, SECTION_SUMMARY SS
 where S.summary_id = SS.id
   and S.id = ?" session-id))
             [tz (sql-column-ref r 8 #f)])
        (set! timestamp (sql-column-ref r 3 0))
        (send title-field set-value (sql-column-ref r 0 ""))
        (send start-date-field set-date-value timestamp tz)
        (send start-time-field set-time-of-day-value timestamp tz)
        (if tz
            (send time-zone-field set-selected-time-zone tz)
            (send time-zone-field set-default-time-zone))
        (send duration-field set-duration-value (sql-column-ref r 4 0))
        (send distance-field set-numeric-value (/ (sql-column-ref r 5 0) 1000.0))
        (send description-field set-value (sql-column-ref r 6 ""))
        (send rpe-scale-choice set-selection (sql-column-ref r 7 0))
        (let ((sport (sql-column-ref r 1 #f))
              (sub-sport (sql-column-ref r 2 #f)))
          (send sport-choice set-selected-sport sport sub-sport)
          (let ((sicon (get-sport-bitmap-colorized sport sub-sport)))
            (send this set-icon sicon)))
        (send labels-input setup-for-session db session-id)
        (send equipment-input setup-for-session db session-id))

      ;; Laps, Lengths and Trackpoint entries also have timestamps and, rather
      ;; than going through the trouble of updating them when the activity
      ;; start time changed, we just disable the controls for these
      ;; activities.
      ;;
      ;; We could say the same thing about the total distance and time, but we
      ;; allow editing those to allow fixing some bad recordings...
      (let ((have-laps? (query-value db "select count(*) from A_LAP where session_id = ?" session-id)))
        (send start-time-field enable (= have-laps? 0))
        (send start-date-field enable (= have-laps? 0))))

    (define (setup-for-new-session db)
      (send title-field set-value "")
      ;; For convenience, set the current date instead of the empty field.
      (set! timestamp (current-seconds))
      (send start-date-field set-date-value timestamp)
      (send start-time-field set-value "")
      (send start-date-field enable #t)
      (send start-time-field enable #t)
      (send time-zone-field set-default-time-zone)
      (send duration-field set-value "")
      (send distance-field set-value "")
      (send description-field set-value "")
      (send rpe-scale-choice set-selection 4) ; moderate
      (send sport-choice set-selected-sport #f #f)
      (send this set-icon (get-sport-bitmap-colorized #f #f))
      (send labels-input setup-for-session db #f)
      (send equipment-input setup-for-session db #f))

    (define (update-session db session-id)
      (call-with-transaction
       db
       (lambda ()
         (let ((sport (send sport-choice get-selection))
               (name (send title-field get-value))
               (desc (send description-field get-value))
               (timestamp (get-gui-timestamp))
               (rpe-scale (send rpe-scale-choice get-selection))
               (ssid (query-value db "select summary_id from A_SESSION where id = ?" session-id))
               (duration (if (send duration-field has-changed?)
                             (send duration-field get-converted-value)
                             #f))
               (distance (if (send distance-field has-changed?)
                             (let ((v (send distance-field get-converted-value)))
                               (if (eq? v 'empty) 0 (* 1000.0 v)))
                             #f)))
           (let-values ([(tzid tzname) (send time-zone-field get-selection)])
             (query-exec
              db
              "update A_SESSION
                set name = ?, description = ?, time_zone_id = ?,
                    start_time = ?, sport_id = ?, sub_sport_id = ?,
                    rpe_scale = ?
               where id = ?"
              name desc tzid timestamp
              (or (car sport) sql-null) (or (cdr sport) sql-null)
              (if (eqv? rpe-scale 0) sql-null rpe-scale)
              session-id))
           (when duration
             (query-exec
              db
              "update SECTION_SUMMARY set total_timer_time = ?, total_elapsed_time = ?
               where id = ?" duration duration ssid))
           (when distance
             (query-exec
              db
              "update SECTION_SUMMARY set total_distance = ? where id = ?"
              (if (> distance 0) distance sql-null) ssid))
           (when (or duration distance)
             ;; If either duration or distance have changed, update the
             ;; average speed.  This should work correctly even if one of them
             ;; is null.
             (query-exec
              db
              "update SECTION_SUMMARY set avg_speed = total_distance / total_elapsed_time where id = ?"
              ssid)))

         (send labels-input update-session-tags session-id)
         (send equipment-input update-session-tags session-id))))

    (define (insert-session db)
      (let* ((duration (let ((v (send duration-field get-converted-value)))
                         (if (eq? v 'empty) #f v)))
             (distance (let ((v (send distance-field get-converted-value)))
                         (if (eq? v 'empty) #f (* 1000.0 v))))
             (avg-speed (if (and duration distance (> duration 0))
                            (/ distance duration) #f))
             (sport (send sport-choice get-selection))
             (name (send title-field get-value))
             (desc (send description-field get-value))
             (start-time (get-gui-timestamp))
             (rpe-scale (send rpe-scale-choice get-selection)))
        (let-values ([(tzid tzname) (send time-zone-field get-selection)])
          (call-with-transaction
           db
           (lambda ()
             (define ssid
               (db-insert
                db
                "insert into SECTION_SUMMARY(total_timer_time, total_elapsed_time, total_distance, avg_speed)
             values(?, ?, ?, ?)"
                (or duration sql-null)
                (or duration sql-null)
                (or distance sql-null)
                (or avg-speed sql-null)))
             (define aid (db-insert db "insert into ACTIVITY(start_time) values (?)" start-time))
             (define sid
               (db-insert
                db
                "insert into A_SESSION(name, description, activity_id, time_zone_id, start_time, sport_id, sub_sport_id, rpe_scale, summary_id)
                 values(?, ?, ?, ?, ?, ?, ?, ?, ?)"
                (or name sql-null)
                (or desc sql-null)
                aid
                tzid
                start-time
                (or (car sport) sql-null)
                (or (cdr sport) sql-null)
                (if (eqv? rpe-scale 0) sql-null rpe-scale)
                ssid))
             (let* ((df (session-df db sid)))
               (maybe-update-session-tss sid df db)
               (send labels-input update-session-tags sid)
               (send equipment-input update-session-tags sid)
               sid))))))

    (define/public (show-dialog parent db session-id)
      (if session-id
          (setup-for-session db session-id)
          (setup-for-new-session db))
      (if (send this do-edit parent)
          (if session-id
              (update-session db session-id)
              (insert-session db))
          #f))
    ))

(define the-edit-session-summary-dialog #f)

(define (get-edit-session-summary-dialog)
  (unless the-edit-session-summary-dialog
    (set! the-edit-session-summary-dialog
          (new edit-session-summary-dialog%)))
  the-edit-session-summary-dialog)
