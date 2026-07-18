#lang racket/base

;; SPDX-License-Identifier: GPL-3.0-or-later
;; edit-pool-length.rkt -- Edit the pool length for a lap swim activity
;;
;; This file is part of ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2
;; Copyright (c) 2026 Alex Harsányi <AlexHarsanyi@gmail.com>
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
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "../widgets/main.rkt"
         "../fmt-util-ut.rkt"
         "../fmt-util.rkt"
         "../sport-charms.rkt"
         gui-widget-mixins
         racket/gui
         racket/contract
         db)

(provide/contract
 (interactive-edit-session-pool-length
  (-> connection?
      (or/c #f (is-a?/c top-level-window<%>))
      exact-nonnegative-integer?
      (or/c #f (list/c positive? (one-of/c 0 1))))))

;; Update the summary information in the A_LENGTH records for the session SID
;; to for a new pool length value. Note that this is not done inside a
;; separate database transaction, use `update-pool-length` instead.
(define (update-pool-length-in-lengths dbc sid pool-length-meters)
  ;; NOTE: SECTION_SUMMARY rows for A_LENGTHs only store avg_speed (no
  ;; total_distance or max_speed)
  (define section-summaries
    (query-rows
     dbc
     "select SS.id,
             SS.total_timer_time
        from A_LENGTH L, A_LAP P, SECTION_SUMMARY SS
       where L.lap_id = P.id
         and L.summary_id = SS.id
         and SS.swim_stroke_id is not null    -- skip rest laps
         and SS.total_timer_time > 0          -- make sure we grab numbers
         and P.session_id = ?"
     sid))
  (for ([row (in-list section-summaries)])
    (match-define (vector ssid duration) row)
    (define avg-speed (/ pool-length-meters duration))
    (query-exec
     dbc
     "update SECTION_SUMMARY set avg_speed = ? where id = ?"
     avg-speed ssid)))

;; Update the summary information in the A_LAP records for the session SID for
;; a new pool length value. Note that this is not done inside a separate
;; database transaction, use `update-pool-length` instead.
(define (update-pool-length-in-laps dbc sid pool-length-meters)
  (define section-summaries
    (query-rows
     dbc
     "select SS.id,
             sum(SSL.total_timer_time),
             count(L.id),
             min(SSL.total_timer_time)
        from A_LAP P, A_LENGTH L, SECTION_SUMMARY SS, SECTION_SUMMARY SSL
       where P.summary_id = SS.id
         and L.lap_id = P.id
         and L.summary_id = SSL.id
         and SS.swim_stroke_id is not null    -- skip rest laps
         and SS.total_timer_time > 0          -- make sure we grab numbers
         and P.session_id = ?
      group by P.id"
     sid))
  (for ([row (in-list section-summaries)])
    (match-define (vector ssid duration lengths fastest-length) row)
    (define distance (* pool-length-meters lengths))
    (define avg-speed (if (> duration 0) (/ distance duration) sql-null))
    (define max-speed (if (> fastest-length 0) (/ pool-length-meters fastest-length) sql-null))
    (query-exec
     dbc
     "update SECTION_SUMMARY
         set total_distance = ?,
             avg_speed = ?,
             max_speed = ?
       where id = ?"
     distance avg-speed max-speed ssid)))

;; Update the summary information for the session SID for a new pool length
;; value. Note that this is not done inside a separate database transaction,
;; use `update-pool-length` instead.
(define (update-pool-length-in-session dbc sid pool-length-meters)
  (match-define (vector ssid duration lengths fastest-length)
    (query-row
     dbc
     "select SS.id as section_summary_id,
             sum(SSL.total_timer_time),
             count(L.id) lengths,
             min(SSL.total_timer_time) fastest_length
       from A_SESSION S, A_LAP P, A_LENGTH L, SECTION_SUMMARY SS, SECTION_SUMMARY SSL
      where S.summary_id = SS.id
        and L.summary_id = SSL.id
        and L.lap_id = P.id
        and S.id = ?
        and P.session_id = S.id
        and L.summary_id = SSL.id
        and SSL.swim_stroke_id is not null
      group by S.id"
     sid))
  (define distance (* pool-length-meters lengths))
  (define avg-speed (if (> duration 0) (/ distance duration) sql-null))
  (define max-speed (if (> fastest-length 0) (/ pool-length-meters fastest-length) sql-null))
  (query-exec
   dbc
   "update SECTION_SUMMARY
       set total_distance = ?,
           avg_speed = ?,
           max_speed = ?
     where id = ?"
   distance avg-speed max-speed ssid))

;; Update the pool length across all elements of the session SID to a new
;; value. pool-length is a value that can be either meters or yards, as
;; defined by pool-length-unit (0 = meters, 1 = yards)
(define (update-pool-length dbc sid pool-length pool-length-unit)
  (call-with-transaction
   dbc
   (lambda ()
     (define pool-length-meters
       (if (= pool-length-unit 0)
           pool-length
           (* pool-length 0.9144)))
     (update-pool-length-in-lengths dbc sid pool-length-meters)
     (update-pool-length-in-laps dbc sid pool-length-meters)
     (update-pool-length-in-session dbc sid pool-length-meters)
     (query-exec
      dbc
      "update A_SESSION set pool_length = ?, pool_length_unit = ? where id = ?"
      pool-length (if (= pool-length-unit 0) 0 1) sid))))

;; Fetch information about the session SID for display in the
;; edit-pool-length% dialog.
(define (fetch-session-data dbc sid)
  (match-define (vector headline start-time time-zone
                        pool-length pool-length-unit
                        duration lengths fastest-length)
    (query-row
     dbc
     "select S.name,
             S.start_time,
             (select ETZ.name from E_TIME_ZONE ETZ where ETZ.id = S.time_zone_id),
             S.pool_length,
             S.pool_length_unit,
             sum(SSL.total_timer_time),
             count(L.id) lengths,
             min(SSL.total_timer_time) fastest_length
       from A_SESSION S, A_LAP P, A_LENGTH L, SECTION_SUMMARY SS, SECTION_SUMMARY SSL
      where S.summary_id = SS.id
        and L.summary_id = SSL.id
        and L.lap_id = P.id
        and S.id = ?
        and P.session_id = S.id
        and L.summary_id = SSL.id
        and SSL.swim_stroke_id is not null
      group by S.id"
     sid))
  (values
   (or headline "Unnamed Session")
   (or start-time (current-seconds))
   (or time-zone #f)
   (or pool-length 50)
   (or pool-length-unit 0)
   duration
   lengths
   fastest-length))

(define (validate-non-negative-rational v)
  (define n (string->number v))
  (and n (rational? n) (>= n 0) n))

(define (summary-values pl plu length-count duration fastest-length)
  (define meters
    (* pl (if (= plu 0) 1.0 0.9144)))
  (define distance (* length-count meters))
  (values
   distance
   (/ distance duration)
   (/ meters fastest-length)))

;; Show a dialog box that allows the user to provide a new pool length value
;; for a swim activity.  Note that this dialog only prompts the user, see
;; `interactive-edit-session-pool-length` for the full update of the session
;; data.
(define edit-pool-length%
  (class edit-dialog-base%
    (init-field headline
                start-time
                time-zone
                init-pool-length
                init-pool-length-unit
                length-count
                duration
                fastest-length)
    (super-new
     [title "Edit Pool Length"]
     [icon (edit-icon)])

    (define options
      (list (list "50 meters" 50 0)
            (list "25 meters" 25 0)
            (list "50 yards" 50 1)
            (list "25 yards" 25 1)
            (list "Custom" #f #f)))

    (define/private (select-option pool-length pool-length-unit)
      (let loop ([index 0]
                 [choices options])
        (if (null? choices)
            (sub1 index)                ; assume last is "Custom"
            (match-let ([(list _label pl plu) (car choices)])
              (if (and (equal? pool-length pl) (equal? pool-length-unit plu))
                  index
                  (loop (add1 index) (cdr choices)))))))

    (define-values (init-distance init-avg-speed init-max-speed)
      (summary-values init-pool-length
                      init-pool-length-unit
                      length-count
                      duration
                      fastest-length))
    (define init-option-selection
      (select-option init-pool-length init-pool-length-unit))

    (define/private (on-pool-length-changed)
      (define selection (send pool-length-choice get-selection))
      (match-define (list _label pool-length pool-length-unit)
        (list-ref options selection))
      (send bottom-pane change-children
            (lambda (_old)
              (if pool-length
                  (list pool-length-choice)
                  (list pool-length-choice pool-length-measure pool-length-unit-choice))))
      (define pl (if pool-length
                     pool-length
                     (send pool-length-measure get-value/validated)))
      (define plu (if pool-length-unit
                      pool-length-unit
                      (send pool-length-unit-choice get-selection)))
      (if pl
          (let-values ([(distance avg-speed max-speed)
                        (summary-values pl
                                        plu
                                        length-count
                                        duration
                                        fastest-length)])
            (send distance-message set-label (short-distance->string distance #t))
            (send avg-pace-message set-label (swim-pace->string avg-speed #t))
            (send max-pace-message set-label (swim-pace->string max-speed #t)))
          (begin
            (send distance-message set-label "--")
            (send avg-pace-message set-label "--")
            (send max-pace-message set-label "--"))))

    (define/override (has-valid-data?)
      (define selection (send pool-length-choice get-selection))
      (match-define (list _label pool-length pool-length-unit)
        (list-ref options selection))
      ;; We have valid data if one of the non-custom pool lengths are
      ;; selected, or the pool length field has a valid value
      (or pool-length (send pool-length-measure get-value/validated)))

    (define root-pane (send this get-client-pane))
    (define message-font
      (send the-font-list find-or-create-font 12 'default 'normal 'normal))
    (define top-pane (make-horizontal-pane root-pane #f))
    (define headline-label
      (new message%
           [parent top-pane]
           [label "Pool Swim: "]
           [stretchable-width #f]))
    (define headline-message
      (new message%
           [parent top-pane]
           [label headline]
           [font message-font]
           [stretchable-width #t]))
    (define middle-pane (make-horizontal-pane root-pane #f))
    (define start-time-label
      (new message%
           [parent middle-pane]
           [label "Start time: "]
           [stretchable-width #f]))
    (define start-time-message
      (new message%
           [parent middle-pane]
           [label (date-time->string start-time #:time-zone time-zone)]
           [font message-font]
           [min-width 130]
           [stretchable-width #t]))
    (define middle2-pane (make-horizontal-pane root-pane #f))
    (define distance-label
      (new message%
           [parent middle2-pane]
           [label "Distance: "]
           [stretchable-width #f]))
    (define distance-message
      (new message%
           [parent middle2-pane]
           [label (short-distance->string init-distance #t)]
           [font message-font]
           [min-width 75]
           [stretchable-width #t]))
    (define avg-pace-label
      (new message%
           [parent middle2-pane]
           [label "Avg Pace: "]
           [stretchable-width #f]))
    (define avg-pace-message
      (new message%
           [parent middle2-pane]
           [label (swim-pace->string init-avg-speed #t)]
           [font message-font]
           [min-width 130]
           [stretchable-width #t]))
    (define max-pace-label
      (new message%
           [parent middle2-pane]
           [label "Max Pace: "]
           [stretchable-width #f]))
    (define max-pace-message
      (new message%
           [parent middle2-pane]
           [label (swim-pace->string init-max-speed #t)]
           [font message-font]
           [min-width 130]
           [stretchable-width #t]))
    (define bottom-pane (make-horizontal-pane root-pane #f))
    (define pool-length-choice
      (new choice%
           [parent bottom-pane]
           [label "Pool Length: "]
           [font message-font]
           [choices (map first options)]
           [selection init-option-selection]
           [callback (lambda (_c _e) (on-pool-length-changed))]))
    (define pool-length-measure
      (new (validate-mixin
            validate-non-negative-rational
            (lambda (v) (~r v #:precision 0))
            (cue-mixin
             "length"
             text-field%))
           [parent bottom-pane]
           [style (if (= (add1 init-option-selection) (length options))
                      '(single)
                      '(single deleted))]
           [label ""]
           [init-value (~r init-pool-length #:precision 0)]
           [font message-font]
           [callback (lambda (_c _e) (on-pool-length-changed))]
           [valid-callback (lambda (_c _v) (on-pool-length-changed))]))
    (define pool-length-unit-choice
      (new choice%
           [parent bottom-pane]
           [style (if (= (add1 init-option-selection) (length options))
                      '()
                      '(deleted))]
           [label ""]
           [font message-font]
           [choices '("Meters" "Yards")]
           [selection init-pool-length-unit]
           [callback (lambda (_c _e) (on-pool-length-changed))]))

    (define/public (show-dialog (parent-window #f))
      (define result (send this do-edit parent-window))
      (if result
          (let ([selection (send pool-length-choice get-selection)])
            (match-define (list _label pool-length pool-length-unit)
              (list-ref options selection))
            (define pl (if pool-length
                           pool-length
                           (send pool-length-measure get-value/validated)))
            (define plu (if pool-length-unit
                            pool-length-unit
                            (send pool-length-unit-choice get-selection)))
            (if (and pl plu)
                (list pl plu)
                #f))
          #f))

    ))

;; Prompt the user to select a new pool length value for the session SID and
;; update it.  DBC is the connection to the database, while TOPLEVEL is the
;; toplevel window that will be the parent for the dialog box (can be #f).
;; SID is the session ID.
(define (interactive-edit-session-pool-length dbc toplevel sid)
  (define sport
    (query-maybe-row
     dbc "select ifnull(sport_id, 0), ifnull(sub_sport_id, 0)
          from A_SESSION where id = ?"
     sid))
  (if (vector? sport)
      (unless (is-lap-swimming? sport)
        (raise-argument-error 'sid "a lap swimming session id" sid))
      (raise-argument-error 'sid "an existing lap swim session id" sid))
  (define-values
    (headline start-time time-zone
              pool-length pool-length-unit
              duration length-count fastest-length)
    (fetch-session-data dbc sid))
  (define dialog
    (new edit-pool-length%
         [headline headline]
         [start-time start-time]
         [time-zone time-zone]
         [init-pool-length pool-length]
         [init-pool-length-unit pool-length-unit]
         [duration duration]
         [length-count length-count]
         [fastest-length fastest-length]))
  (define result (send dialog show-dialog toplevel))
  (if result
      (match-let ([(list updated-pool-length updated-pool-length-unit) result])
        (update-pool-length dbc sid updated-pool-length updated-pool-length-unit)
        result)
      #f))
