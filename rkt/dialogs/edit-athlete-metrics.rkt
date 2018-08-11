#lang racket/base
;; edit-athlete-metrics.rkt -- edit or update bodyweight, sleep quality, etc
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
         racket/class
         racket/gui/base
         "../al-widgets.rkt"
         "../dbutil.rkt"
         "../widgets/main.rkt")

(provide get-edit-athlete-metrics-dialog)

(define (index a b)
  (let ((tail (member a (reverse b))))
    (if tail (length (cdr tail)) #f)))


;;................................................ edit-athlete-metrics% ....

(define edit-athlete-metrics%
  (class edit-dialog-base%
    (init)
    (super-new [title "Athlete Metrics"] [icon (wscale-icon)])

    (define date-field #f)
    (define time-field #f)
    (define weight-field #f)
    (define sleep-hours-field #f)
    (define sleep-quality-field #f)
    (define overall-feeling-field #f)
    (define notes-field #f)

    (define sleepq '())
    (define overallq '())

    (define bwadj 0.1)             ; amount by which we adjust the body weight
    
    (define (decrement-body-weight)
      (when (send weight-field has-valid-value?)
        (let ((val (send weight-field get-converted-value)))
          (when (> val bwadj)
            (send weight-field set-numeric-value (- val bwadj))))))

    (define (increment-body-weight)
      (when (send weight-field has-valid-value?)
        (let ((val (send weight-field get-converted-value)))
          (send weight-field set-numeric-value (+ val bwadj)))))

    (define sltadj (* 15 60))           ; amount by which we adjust sleep time

    (define (decrement-sleep-time)
      (when (send sleep-hours-field has-valid-value?)
        (let ((val (send sleep-hours-field get-converted-value)))
          (when (> val sltadj)
            (send sleep-hours-field set-duration-value (- val sltadj))))))

    (define (increment-sleep-time)
      (when (send sleep-hours-field has-valid-value?)
        (let ((val (send sleep-hours-field get-converted-value)))
          (send sleep-hours-field set-duration-value (+ val sltadj)))))

    (let ((p (send this get-client-pane)))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! date-field
              (new date-input-field% [parent p0] [label "Date: "] [font al-dlg-item-font]))
        (set! time-field
              (new time-of-day-input-field% [parent p0] [label "Time: "] [font al-dlg-item-font])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! weight-field (new number-input-field%
                                [parent p0] [label "Bodyweight: "] [font al-dlg-item-font]
                                [cue-text "kg"]))
        ;; In tablet mode, add + and - buttons to allow for easy entering of
        ;; data
        (when (al-pref-tablet-friendly?)
          (new button% [parent p0] [label " - "]
               [callback (lambda (b e) (decrement-body-weight))])
          (new button% [parent p0] [label " + "]
               [callback (lambda (b e) (increment-body-weight))])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! sleep-hours-field (new duration-input-field%
                                     [parent p0] [label "Sleep time: "] [font al-dlg-item-font]))
        ;; In tablet mode, add + and - buttons to allow for easy entering of
        ;; data
        (when (al-pref-tablet-friendly?)
          (new button% [parent p0] [label " - "]
               [callback (lambda (b e) (decrement-sleep-time))])
          (new button% [parent p0] [label " + "]
               [callback (lambda (b e) (increment-sleep-time))]))
        (set! sleep-quality-field (new choice% [parent p0]
                                       [label "Sleep quality "] [font al-dlg-item-font]
                                       [choices '("Not specified" "Good" "Average" "Bad")])))

      (let ((p0 (make-horizontal-pane p #f)))
        (send p0 spacing al-dlg-item-spacing)
        (set! overall-feeling-field (new choice% [parent p0]
                                         [label "Overall feeling "] [font al-dlg-item-font]
                                         [choices '("Not specified" "Good" "Average" "Bad")])))
      (let ((p0 (make-horizontal-pane p #t)))
        (send p0 spacing al-dlg-item-spacing)
        (set! notes-field
              (new text-field% [parent p0]
                   [label "Notes: "] [style '(multiple)] [font al-dlg-item-font])))
      )

    ;; Setup the sleep and overall quality fields with values from the
    ;; database (DB)
    (define (setup-fields db)
      (send sleep-quality-field clear)
      (set! sleepq '(-1))
      (send sleep-quality-field append "Not specified")
      (for (([id val] (in-query db "select id, name from E_SLEEP_QUALITY order by id")))
        (set! sleepq (cons id sleepq))
        (send sleep-quality-field append val))
      (set! sleepq (reverse sleepq))

      (send overall-feeling-field clear)
      (set! overallq '(-1))
      (send overall-feeling-field append "Not specified")
      (for (([id val] (in-query db "select id, name from E_OVERALL_FEELING order by id")))
        (set! overallq (cons id overallq))
        (send overall-feeling-field append val))
      (set! overallq (reverse overallq)))

    (define (setup-new-metrics db)
      ;; Select the middle option by default, this should be an "average"
      (let ((ts (current-seconds)))
        (send date-field set-date-value ts)
        (send time-field set-time-of-day-value ts))
      (send sleep-quality-field set-selection 0)
      (send overall-feeling-field set-selection 0)
      ;; Use most recent weight as the starting point.
      (let ((bw (query-maybe-value
                 db
                 "select body_weight 
                    from ATHLETE_METRICS 
                   where timestamp = (select max(timestamp) from ATHLETE_METRICS)")))
        (if bw
            (send weight-field set-numeric-value bw)
            (send weight-field set-value "")))
      (let ((st (query-maybe-value
                 db
                 "select sleep_time 
                    from ATHLETE_METRICS 
                   where timestamp = (select max(timestamp) from ATHLETE_METRICS)")))
        (if st
            (send sleep-hours-field set-duration-value st)
            (send sleep-hours-field set-value "")))
      (send notes-field set-value ""))

    (define (setup-metrics db id)
      (let ((row (query-row db "select timestamp, body_weight, sleep_time,
                                       sleep_quality, overall_feeling, description
                                  from ATHLETE_METRICS
                                 where id = ?" id)))
        (let ((ts (vector-ref row 0)))
          (send date-field set-date-value ts)
          (send time-field set-time-of-day-value ts))
        (let ((bw (sql-column-ref row 1 #f)))
          (send weight-field set-value (if bw (format "~a" bw) "")))
        (let ((sleep (sql-column-ref row 2 #f)))
          (if sleep
              (send sleep-hours-field set-duration-value sleep)
              (send sleep-hours-field set-value "")))
        (let ((q (sql-column-ref row 3 -1)))
          (send sleep-quality-field set-selection (index q sleepq)))
        (let ((q (sql-column-ref row 4 -1)))
          (send overall-feeling-field set-selection (index q overallq)))
        (send notes-field set-value (sql-column-ref row 5 ""))))

    (define (insert-metrics db)
      (let ((ts (+ (send date-field get-converted-value)
                   (send time-field get-converted-value)))
            (bw (let ((v (send weight-field get-converted-value)))
                  (if (eq? v 'empty) #f v)))
            (slh (let ((v (send sleep-hours-field get-converted-value)))
                      (if (eq? v 'empty) #f v)))
            (slq (let ((v (list-ref sleepq (send sleep-quality-field get-selection))))
                   (if (eqv? v -1) #f v)))
            (ov (let ((v (list-ref overallq (send overall-feeling-field get-selection))))
                  (if (eqv? v -1) #f v)))
            (notes (send notes-field get-value)))
        (call-with-transaction
         db
         (lambda ()
           (query-exec db "insert into ATHLETE_METRICS(
                             timestamp, body_weight, sleep_time,
                             sleep_quality, overall_feeling,
                             description)
                           values(?, ?, ?, ?, ?, ?)"
                       ts (or bw sql-null) (or slh sql-null) (or slq sql-null) (or ov sql-null) notes)
           (db-get-last-pk "ATHLETE_METRICS" db)))))

    (define (update-metrics db id)
      (let ((ts (+ (send date-field get-converted-value)
                   (send time-field get-converted-value)))
            (bw (let ((v (send weight-field get-converted-value)))
                  (if (eq? v 'empty) #f v)))
            (slh (let ((v (send sleep-hours-field get-converted-value)))
                      (if (eq? v 'empty) #f v)))
            (slq (let ((v (list-ref sleepq (send sleep-quality-field get-selection))))
                   (if (eqv? v -1) #f v)))
            (ov (let ((v (list-ref overallq (send overall-feeling-field get-selection))))
                  (if (eqv? v -1) #f v)))
            (notes (send notes-field get-value)))
        (query-exec db "update ATHLETE_METRICS set
                           timestamp = ?, body_weight = ?, sleep_time = ?,
                           sleep_quality = ?, overall_feeling = ?,
                           description = ?
                        where id = ?"
                    ts (or bw sql-null) (or slh sql-null) (or slq sql-null) (or ov sql-null) notes id)
        id))

    (define/override (has-valid-data?)
      (and (send date-field has-valid-value?)
           (send time-field has-valid-value?)
           (send weight-field has-valid-value?)
           (send sleep-hours-field has-valid-value?)))

    (define/public (show-dialog parent db [id #f])
      (setup-fields db)
      (if id
          (setup-metrics db id)
          (setup-new-metrics db))
      (if (send this do-edit parent)
          (if id
              (update-metrics db id)
              (insert-metrics db))
          #f))

    ))

(define the-edit-athlete-metrics-dialog #f)

(define (get-edit-athlete-metrics-dialog)
  (unless the-edit-athlete-metrics-dialog
    (set! the-edit-athlete-metrics-dialog
          (new edit-athlete-metrics%)))
  the-edit-athlete-metrics-dialog)



